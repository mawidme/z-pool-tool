module Id = Pool_common.Id
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let find_request_sql where_fragment =
  Format.asprintf
    "%s\n%s"
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(user_users.uuid), 1, 8), '-',
          SUBSTR(HEX(user_users.uuid), 9, 4), '-',
          SUBSTR(HEX(user_users.uuid), 13, 4), '-',
          SUBSTR(HEX(user_users.uuid), 17, 4), '-',
          SUBSTR(HEX(user_users.uuid), 21)
        )),
        user_users.email,
        user_users.username,
        user_users.name,
        user_users.given_name,
        user_users.password,
        user_users.status,
        user_users.admin,
        user_users.confirmed,
        user_users.created_at,
        user_users.updated_at,
        pool_contacts.recruitment_channel,
        pool_contacts.terms_accepted_at,
        pool_contacts.language,
        pool_contacts.paused,
        pool_contacts.disabled,
        pool_contacts.verified,
        pool_contacts.email_verified,
        pool_contacts.num_invitations,
        pool_contacts.num_assignments,
        pool_contacts.firstname_version,
        pool_contacts.lastname_version,
        pool_contacts.paused_version,
        pool_contacts.language_version,
        pool_contacts.created_at,
        pool_contacts.updated_at
      FROM pool_contacts
        LEFT JOIN user_users
        ON pool_contacts.user_uuid = user_users.uuid
    |sql}
    where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.uuid = UNHEX(REPLACE(?, '-', ''))
        AND user_users.admin = 0
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find pool id =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  >|= CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_by_email_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_by_email pool email =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_by_email_request
    (Pool_user.EmailAddress.value email)
  >|= CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_confirmed_request =
  let open Caqti_request.Infix in
  find_request_sql
    {sql|
      WHERE user_users.email = ?
        AND user_users.admin = 0
        AND user_users.confirmed = 1
    |sql}
  |> Caqti_type.string ->! Repo_model.t
;;

let find_confirmed pool email =
  let open Lwt.Infix in
  Utils.Database.find_opt
    (Database.Label.value pool)
    find_confirmed_request
    (Pool_user.EmailAddress.value email)
  >|= CCOption.to_result Pool_common.Message.(NotFound Field.Contact)
;;

let find_filtered_request filter =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    WHERE
      %s
      AND user_users.admin = 0
      AND user_users.confirmed = 1
      AND NOT EXISTS
        (SELECT 1
        FROM pool_invitations
        WHERE
            pool_invitations.contact_id = pool_contacts.id
          AND
            pool_invitations.experiment_id IN (
              SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE($1, '-', '')))
            )
        AND NOT EXISTS
        (SELECT 1
        FROM pool_assignments
        WHERE
            pool_assignments.contact_id = pool_contacts.id
          AND
            pool_assignments.session_id IN (
              SELECT id FROM pool_sessions WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE($1, '-', '')))
            )
    |sql}
    filter
  |> find_request_sql
  |> Caqti_type.string ->* Repo_model.t
;;

let find_filtered pool experiment_id filter =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_filtered_request filter)
    (experiment_id |> Pool_common.Id.value)
;;

let find_multiple_request ids =
  Format.asprintf
    {sql|
    WHERE user_uuid IN ( %s )
    AND user_users.admin = 0
   |sql}
    (CCList.mapi
       (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
       ids
    |> CCString.concat ",")
  |> find_request_sql
;;

let find_multiple pool ids =
  let open Caqti_request.Infix in
  let dyn =
    CCList.fold_left
      (fun dyn id ->
        dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
      Dynparam.empty
      ids
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request = find_multiple_request ids |> pt ->* Repo_model.t in
  Utils.Database.collect (pool |> Pool_database.Label.value) request pv
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_contacts (
        user_uuid,
        recruitment_channel,
        terms_accepted_at,
        language,
        paused,
        disabled,
        verified,
        email_verified,
        num_invitations,
        num_assignments,
        firstname_version,
        lastname_version,
        paused_version,
        language_version,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11,
        $12,
        $13,
        $14,
        $15,
        $16
      )
    |sql}
  |> Repo_model.contact ->. Caqti_type.unit
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

module Paused = struct
  let update_request =
    let open Caqti_request.Infix in
    let open Pool_common.Repo in
    {sql|
      UPDATE pool_contacts
      SET
        paused = $2,
        paused_version = $3
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(tup3 Id.t Pool_user.Repo.Paused.t Version.t)
       ->. Caqti_type.unit
  ;;

  let update pool (Entity.{ paused; paused_version; _ } as contact) =
    Utils.Database.exec
      (Database.Label.value pool)
      update_request
      ( contact |> Entity.id
      , paused |> Pool_user.Paused.value
      , paused_version |> Pool_common.Version.value )
  ;;
end

module Language = struct
  let update_request =
    let open Caqti_request.Infix in
    let open Pool_common.Repo in
    {sql|
      UPDATE pool_contacts
      SET
        language = $2,
        language_version = $3
      WHERE user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(tup3 Id.t (option Language.t) Version.t ->. unit)
  ;;

  let update pool (Entity.{ language; language_version; _ } as contact) =
    Utils.Database.exec
      (Database.Label.value pool)
      update_request
      ( contact |> Entity.id
      , language
      , language_version |> Pool_common.Version.value )
  ;;
end

let update_version_for_request field =
  let open Caqti_request.Infix in
  let field =
    match field with
    | `Firstname -> "firstname_version"
    | `Lastname -> "lastname_version"
  in
  let update = {sql| UPDATE pool_contacts SET |sql} in
  let where = {sql| WHERE user_uuid = UNHEX(REPLACE($1, '-', '')) |sql} in
  Format.asprintf "%s\n%s = $2\n%s" update field where
  |> Caqti_type.(Pool_common.Repo.(tup2 Id.t Version.t)) ->. Caqti_type.unit
;;

let update_version_for pool field (id, version) =
  Utils.Database.exec
    (Database.Label.value pool)
    (field |> update_version_for_request)
    (id, version |> Pool_common.Version.value)
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_contacts
      SET
        recruitment_channel = $2,
        terms_accepted_at = $3,
        language = $4,
        paused = $5,
        disabled = $6,
        verified = $7,
        email_verified = $8,
        num_invitations = $9,
        num_assignments = $10,
        firstname_version = $11,
        lastname_version = $12,
        paused_version = $13,
        language_version = $14
      WHERE
        user_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Repo_model.Write.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec
    (Database.Label.value pool)
    update_request
    (Entity.Write.create t)
;;

let delete_unverified_contact_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_contacts
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_user_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM user_users
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified_email_verifications_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_email_verifications
    WHERE user_uuid = UNHEX(REPLACE(?, '-', '')) AND verified IS NULL
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete_unverified pool id =
  let exec request =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      request
      (Pool_common.Id.value id)
  in
  let%lwt _ = exec delete_unverified_user_request in
  let%lwt _ = exec delete_unverified_email_verifications_request in
  exec delete_unverified_contact_request
;;
