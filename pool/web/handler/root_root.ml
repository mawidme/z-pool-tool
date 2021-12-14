module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Database = Database_pool

let create req =
  let open Lwt_result.Syntax in
  let open Utils.Lwt_result.Infix in
  let user () =
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    Sihl.Web.Request.urlencoded "email" req
    ||> CCOption.to_result Pool_common.Message.EmailAddressMissingRoot
    >>= HttpUtils.validate_email_existance tenant_db
  in
  let events () =
    let open CCResult.Infix in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    urlencoded
    |> Cqrs_command.Root_command.Create.decode
    >>= Cqrs_command.Root_command.Create.handle
    |> Lwt_result.lift
  in
  let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
  let return_to_overview () =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ Pool_common.Message.(Created Root) ] ]
  in
  ()
  |> user
  >>= events
  |> Lwt_result.map_err (fun err -> err, "/root/tenants/")
  |>> handle
  |>> return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let toggle_status req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
  let events user =
    Cqrs_command.Root_command.ToggleStatus.handle user |> Lwt_result.lift
  in
  let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
  let return_to_overview () =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ Pool_common.Message.(Updated Root) ] ]
  in
  id
  |> Root.find
  >>= events
  |> Lwt_result.map_err (fun err -> err, "/root/tenants/")
  |>> handle
  |>> return_to_overview
  >|> HttpUtils.extract_happy_path
;;
