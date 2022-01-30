module Message = Http_utils.Message
module Login = Public_login
module Common = Pool_common
module Database = Pool_database

let index req =
  let query_lang = Http_utils.find_query_lang req in
  if Http_utils.is_req_from_root_host req
  then Http_utils.redirect_to "/root"
  else (
    let%lwt result =
      let open Lwt_result.Syntax in
      let message =
        CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
      in
      let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
      let* tenant = Pool_tenant.find_by_label tenant_db in
      let%lwt language = General.language_from_request req tenant_db in
      Page.Public.index language tenant message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
    in
    result
    |> CCResult.map_err (fun err ->
           err, Http_utils.path_with_lang query_lang "/error")
    |> Http_utils.extract_happy_path)
;;

let index_css req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* styles = Pool_tenant.find_styles tenant_db in
    let%lwt file =
      Service.Storage.find
        ~ctx:(Pool_tenant.to_ctx Database.root)
        (styles |> Pool_tenant.Styles.id |> Pool_common.Id.value)
    in
    let%lwt content =
      Service.Storage.download_data_base64 file ||> Base64.decode_exn
    in
    Sihl.Web.Response.of_plain_text content
    |> Sihl.Web.Response.set_content_type
         (styles
         |> Pool_tenant.Styles.mime_type
         |> Pool_common.File.Mime.to_string)
    |> Lwt.return_ok
  in
  match result with
  | Ok res -> Lwt.return res
  | Error _ ->
    Lwt.return
      (Sihl.Web.Response.set_content_type
         "text/css"
         (Sihl.Web.Response.of_plain_text ""))
;;

let email_confirmation_note req =
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/")
    @@
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt language = General.language_from_request req tenant_db in
    let txt_to_string m = Common.Utils.text_to_string language m in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let html =
      Common.I18n.(
        Page.Utils.note
          language
          (txt_to_string EmailConfirmationTitle)
          (txt_to_string EmailConfirmationNote))
    in
    message |> html |> Sihl.Web.Response.of_html |> Lwt.return_ok
  in
  result |> Http_utils.extract_happy_path
;;

let not_found req =
  let query_lang = Http_utils.find_query_lang req in
  let open Lwt_result.Syntax in
  let%lwt result =
    Lwt_result.map_err (fun err ->
        err, Http_utils.path_with_lang query_lang "/error")
    @@ let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
       let%lwt language = General.language_from_request req tenant_db in
       let html = Page.Utils.error_page_not_found language () in
       Sihl.Web.Response.of_html html |> Lwt.return_ok
  in
  result |> Http_utils.extract_happy_path
;;

let asset req =
  let open Sihl.Contract.Storage in
  let asset_id = Sihl.Web.Router.param req "id" in
  let%lwt file =
    Service.Storage.find ~ctx:(Pool_tenant.to_ctx Database.root) asset_id
  in
  let%lwt content = Service.Storage.download_data_base64 file in
  let mime = file.file.mime in
  let content = content |> Base64.decode_exn in
  Sihl.Web.Response.of_plain_text content
  |> Sihl.Web.Response.set_content_type mime
  |> Lwt.return
;;

let error req =
  let%lwt tenant_error =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* _ = Pool_tenant.find_by_label tenant_db in
    Ok
      ( Common.Message.TerminatoryTenantErrorTitle
      , Common.Message.TerminatoryTenantError )
    |> Lwt.return
  in
  let root_error =
    ( Common.Message.TerminatoryRootErrorTitle
    , Common.Message.TerminatoryRootError )
  in
  let error_page (title, note) =
    Page.Utils.error_page_terminatory title note ()
  in
  (match tenant_error with
  | Ok tenant_error -> tenant_error
  | Error _ -> root_error)
  |> error_page
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;
