open Entity
module RepoEntity = Repo_entity

module Sql = struct
  let find_all_sql fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
          title,
          description,
          url,
          database_url,
          smtp_auth_server,
          smtp_auth_port,
          smtp_auth_username,
          smtp_auth_authentication_method,
          smtp_auth_protocol,
          styles,
          icon,
          logos,
          partner_logos,
          mainenance,
          disabled,
          default_language,
          created_at,
          updated_at
        FROM pool_tenant
      |sql}
    in
    Format.asprintf "%s %s" select_from fragment
  ;;

  let find_all_request =
    "" |> find_all_sql |> Caqti_request.collect Caqti_type.unit RepoEntity.t
  ;;

  let find_all = Utils.Database.collect find_all_request

  let insert_sql =
    {sql|
      INSERT INTO pool_tenant (
        uuid,
        title,
        description,
        url,
        database_url,
        smtp_auth_server,
        smtp_auth_port,
        smtp_auth_username,
        smtp_auth_authentication_method,
        smtp_auth_protocol,
        styles,
        icon,
        logos,
        partner_logos,
        mainenance,
        disabled,
        default_language,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      );
    |sql}
  ;;

  let insert_request = Caqti_request.exec RepoEntity.t insert_sql
  let insert t = Utils.Database.exec insert_request t

  let insert_with_logs t =
    Logs.info (fun m -> m "========: %s" (Entity.show t));
    let insert () = Utils.Database.exec insert_request t in
    ()
    |> insert
    |> Lwt_result.map_err (fun err ->
           Logs.info (fun m -> m "%s" "=================");
           Logs.info (fun m -> m "Error: %s" err);
           Logs.info (fun m -> m "%s" "=================");
           err)
  ;;
end

let find_by_id (id : string) : (t, string) result Lwt.t = Utils.todo id
let find_all = Sql.find_all
let insert = Sql.insert_with_logs
let update t = Utils.todo t
let destroy = Utils.todo
