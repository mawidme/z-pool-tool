module Dynparam = Database.Dynparam
open Repo_entity

let id_select_fragment = Pool_common.Id.sql_select_fragment
let id_value_fragment = Pool_common.Id.sql_value_fragment

let sql_select_columns =
  [ id_select_fragment ~field:"pool_contacts_possible_duplicates.uuid"
  ; id_select_fragment
      ~field:"pool_contacts_possible_duplicates.target_user_uuid"
  ]
  @ Contact.Repo.sql_select_columns
  @ [ "pool_contacts_possible_duplicates.score" ]
;;

let joins =
  Format.asprintf
    {sql|
    INNER JOIN pool_contacts 
      ON pool_contacts.user_uuid = pool_contacts_possible_duplicates.contact_uuid
    %s
  |sql}
    Contact.Repo.joins
;;

let find_request_sql ?(count = false) =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_contacts_possible_duplicates %s %s ORDER BY score DESC |sql}
    columns
    joins
;;

let similarity_request user_columns custom_field_columns similarities average =
  let user_columns = user_columns |> CCString.concat "," in
  let custom_field_columns = custom_field_columns |> CCString.concat "," in
  let similarities = similarities |> CCString.concat "," in
  let contact_joins =
    {sql| 
      INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
      LEFT JOIN pool_custom_field_answers ON pool_contacts.user_uuid = pool_custom_field_answers.entity_uuid
    |sql}
  in
  [%string
    {sql|
      WITH filtered_contacts AS (
        SELECT
          user_users.uuid,
          %{user_columns},
          %{custom_field_columns}
        FROM
          pool_contacts
        %{contact_joins}
        WHERE 
          pool_contacts.email_verified IS NOT NULL
          AND pool_contacts.disabled = 0
        GROUP BY user_users.uuid
      ),
      similarity_scores AS (
        SELECT
          t.uuid as target_uuid,
          contacts.uuid,
          %{similarities}
        FROM
        filtered_contacts AS t
        CROSS JOIN filtered_contacts AS contacts
        WHERE
          t.uuid = UNHEX(REPLACE($1, '-', ''))
          AND contacts.uuid <> t.uuid
      ),
      average_similarity AS (
        SELECT
          target_uuid,
          uuid,
          %{average} AS similarity_score
        FROM similarity_scores
      )
      SELECT
        %{id_select_fragment ~field:"target_uuid"},
        %{id_select_fragment ~field:"uuid"},
        CAST(similarity_score AS FLOAT)
      FROM
        average_similarity
      WHERE 
        similarity_score >= $2
      ORDER BY
        similarity_score DESC;
    |sql}]
;;

let find_similars database_label ~user_uuid custom_fields =
  let open CCList in
  let open Entity in
  let open Format in
  let open Caqti_request.Infix in
  let concat_sql ?table { Column.sql_table; sql_column; _ } =
    asprintf "%s.%s" (CCOption.value ~default:sql_table table) sql_column
  in
  let make_similarity_name sql_column = asprintf "%s_similarity" sql_column in
  let make_comparison (left_column, right_column) =
    let open SimilarityCriteria in
    function
    | Fuzzy -> asprintf "SOUNDEX(%s) = SOUNDEX(%s)" left_column right_column
    | Exact -> asprintf "%s = %s" left_column right_column
  in
  let user_similarities column =
    let open Column in
    let with_name comp =
      asprintf "%s as %s" comp (make_similarity_name column.sql_column)
    in
    let target_col = asprintf "t.%s" column.sql_column in
    let user_col = concat_sql ~table:"contacts" column in
    make_comparison (user_col, target_col) column.criteria |> with_name
  in
  let field_similarities field =
    let id = Custom_field.(id field |> Id.to_common |> Id.value) in
    let target_col = asprintf "t.`%s`" id in
    let user_col = asprintf "contacts.`%s`" id in
    asprintf
      "%s as %s"
      (make_comparison (target_col, user_col) SimilarityCriteria.Exact)
      (make_similarity_name id |> asprintf "`%s`")
  in
  let user_columns =
    columns
    >|= fun col -> asprintf "%s as %s" (concat_sql col) col.Column.sql_column
  in
  (* Dynparam not required, atm *)
  let dyn =
    Dynparam.(
      empty
      |> add Pool_common.Repo.Id.t user_uuid
      |> add Caqti_type.float Entity.alert_threshold)
  in
  let custom_field_columns =
    let open Custom_field in
    (* Using placeholders like $2 or ? is not supported in colum names *)
    custom_fields
    >|= fun field ->
    let id = id field |> Id.to_common |> Id.value in
    let column =
      asprintf
        {sql| MAX(CASE WHEN pool_custom_field_answers.custom_field_uuid = %s THEN pool_custom_field_answers.value END) AS %s |sql}
        (id |> asprintf "\"%s\"" |> id_value_fragment)
        (asprintf "`%s`" id)
    in
    column
  in
  let similarities =
    map user_similarities columns @ map field_similarities custom_fields
  in
  let average_similarity =
    let not_null = CCFun.uncurry (asprintf "(%s IS NOT NULL) * %d") in
    let coalesce = CCFun.uncurry (asprintf "COALESCE(%s * %d, 0)") in
    let user_similarities =
      columns
      >|= fun { Column.sql_column; weight; _ } ->
      make_similarity_name sql_column, weight
    in
    let custom_field_similarities =
      custom_fields
      >|= fun field ->
      Custom_field.(
        ( id field
          |> Id.to_common
          |> Id.value
          |> make_similarity_name
          |> asprintf "`%s`"
        , 1 ))
    in
    let similarities = user_similarities @ custom_field_similarities in
    let division =
      similarities
      >|= not_null
      |> CCString.concat " + "
      |> asprintf "NULLIF(%s, 0)"
    in
    similarities
    >|= coalesce
    |> CCString.concat " + "
    |> fun average -> Format.asprintf "(%s) / %s" average division
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    similarity_request
      user_columns
      custom_field_columns
      similarities
      average_similarity
    |> pt ->* raw
  in
  Database.collect database_label request pv
;;

let insert_request =
  Format.asprintf
    {sql|
    INSERT INTO pool_contacts_possible_duplicates (
      uuid, 
      target_user_uuid, 
      contact_uuid,
      score
    ) VALUES 
      %s
    ON DUPLICATE KEY UPDATE
      score = VALUES(score),
      updated_at = NOW();
  |sql}
;;

let insert pool = function
  | [] -> Lwt.return_unit
  | rows ->
    let open Dynparam in
    let open Caqti_request.Infix in
    let id = Pool_common.Repo.Id.t in
    let id_sql = "UNHEX(REPLACE(?, '-', ''))" in
    let dyn, sql =
      rows
      |> CCList.fold_left
           (fun (dyn, sql) (target_id, contact_id, score) ->
             let dyn =
               dyn
               |> add id (Pool_common.Id.create ())
               |> add id target_id
               |> add id contact_id
               |> add Caqti_type.float score
             in
             let values =
               [ id_sql; id_sql; id_sql; "?" ]
               |> CCString.concat ","
               |> Format.asprintf "(%s)"
             in
             let sql = sql @ [ values ] in
             dyn, sql)
           (empty, [])
    in
    let sql = CCString.concat "," sql in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request = insert_request sql |> pt ->. Caqti_type.unit in
    Database.exec pool request pv
;;

let find_request =
  let open Caqti_request.Infix in
  {sql| WHERE pool_contacts_possible_duplicates.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
  |> find_request_sql
  |> Id.t ->! t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Duplicate)
;;

let find_by_contact pool contact =
  let request =
    let open Caqti_request.Infix in
    let contact_columns =
      Contact.Repo.sql_select_columns |> CCString.concat ", "
    in
    (* TODO: If the combanation is unique in both ways, group by can be replaced
       with a select distinct *)
    [%string
      {sql|
        WITH duplicates AS (
          SELECT 
            uuid,
            target_user_uuid as target_user_uuid,
            contact_uuid as duplicate_user_uuid,
            score
          FROM pool_contacts_possible_duplicates
          WHERE target_user_uuid = UNHEX(REPLACE($1, '-', ''))
        UNION ALL
          SELECT 
            uuid,
            contact_uuid as target_user_uuid,
            target_user_uuid as duplicate_user_uuid,
            score
          FROM pool_contacts_possible_duplicates
          WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
        )
        SELECT  
          duplicates.uuid,
          duplicates.target_user_uuid,
          %{contact_columns},
          duplicates.score
        FROM duplicates
        INNER JOIN pool_contacts 
          ON pool_contacts.user_uuid = duplicates.duplicate_user_uuid
          %{Contact.Repo.joins}
        GROUP BY user_users.uuid
        ORDER BY score DESC
      |sql}]
    |> Contact.Repo.Id.t ->* t
  in
  Database.collect pool request (Contact.id contact)
;;
