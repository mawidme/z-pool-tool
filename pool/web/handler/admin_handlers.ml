module I18n = Admin_i18n
module Message = Http_utils.Message
module Settings = Admin_settings

let create_layout req = General.create_tenant_layout `Admin req

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    Lwt_result.map_err (fun err -> err, "/error")
    @@
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    Page.Admin.dashboard context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path req
;;