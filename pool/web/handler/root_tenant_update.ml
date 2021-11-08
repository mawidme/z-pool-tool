module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Common = Pool_common

let update req command success_message =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" |> Common.Id.of_string in
  let redirect_path = Format.asprintf "/root/tenants/%s" (Common.Id.value id) in
  let events tenant =
    let open Lwt_result.Syntax in
    let%lwt multipart_encoded =
      Sihl.Web.Request.to_multipart_form_data_exn req
    in
    let* _ =
      File.update_files
        [ "styles", tenant.Tenant.Write.styles |> Tenant.Styles.Write.value
        ; "icon", tenant.Tenant.Write.icon |> Tenant.Icon.Write.value
        ]
        req
    in
    let* logo_files =
      File.upload_files (Tenant.LogoMapping.LogoType.all ()) req
    in
    let events_list urlencoded =
      let open CCResult.Infix in
      match command with
      | `EditDetail ->
        Cqrs_command.Tenant_command.EditDetails.(
          decode urlencoded >>= handle tenant)
      | `EditDatabase ->
        Cqrs_command.Tenant_command.EditDatabase.(
          decode urlencoded >>= handle tenant)
    in
    logo_files @ multipart_encoded
    |> File.multipart_form_data_to_urlencoded
    |> HttpUtils.format_request_boolean_values [ "disabled" ]
    |> events_list
    |> Lwt_result.lift
  in
  let handle = Lwt_list.iter_s (Pool_event.handle_event Common.Database.root) in
  let return_to_overview () =
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ success_message ] ]
  in
  id
  |> Tenant.find_full
  >>= events
  |> Lwt_result.map_err (fun err -> err, redirect_path)
  |>> handle
  |>> return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let update_detail req =
  let message = Common.Error.I18n.RootTenant.message_update_detail in
  update req `EditDetail message
;;

let update_database req =
  let message = Common.Error.I18n.RootTenant.message_update_database in
  update req `EditDatabase message
;;

let delete_asset req =
  let open Sihl.Web in
  let asset_id = Router.param req "asset_id" |> Common.Id.of_string in
  let tenant_id = Router.param req "tenant_id" |> Common.Id.of_string in
  let redirect_path =
    Format.asprintf "root/tenants/%s" (Common.Id.value tenant_id)
  in
  let%lwt result =
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let open Utils.Lwt_result.Infix in
    let ctx = Common.(Utils.pool_to_ctx Database.root) in
    let event tenant =
      Cqrs_command.Tenant_command.DestroyLogo.handle tenant asset_id
      |> Lwt_result.lift
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event Common.Database.root)
    in
    let destroy_file () =
      Service.Storage.delete ~ctx ~id:(Common.Id.value asset_id)
    in
    let return_to_tenant () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ "File was successfully deleted." ] ]
    in
    tenant_id
    |> Tenant.find
    >>= event
    |>> handle
    |>> destroy_file
    |>> return_to_tenant
  in
  result |> HttpUtils.extract_happy_path
;;
