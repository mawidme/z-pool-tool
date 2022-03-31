module HttpUtils = Http_utils
module Message = HttpUtils.Message

let require_admin ~login_path_f =
  let open Utils.Lwt_result.Infix in
  let fail_action = () |> login_path_f |> HttpUtils.redirect_to in
  let filter handler req =
    let context = Pool_context.find req in
    match context with
    | Error _ -> fail_action
    | Ok context ->
      let db_pool = context.Pool_context.tenant_db in
      Service.User.Web.user_from_session ~ctx:(Pool_tenant.to_ctx db_pool) req
      >|> (function
      | Some user ->
        Admin.user_is_admin db_pool user
        >|> (function
        | false -> fail_action
        | true -> handler req)
      | None -> fail_action)
  in
  Rock.Middleware.create ~name:"user.require.admin" ~filter
;;