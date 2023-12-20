module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = "/admin/settings/schedules"

let show =
  Http_utils.Htmx.handler
    ~active_navigation
    ~error_path:active_navigation
    ~query:(module Schedule)
    ~create_layout:General.create_tenant_layout
  @@ fun context query ->
  let%lwt schedules, query = Schedule.find_by query in
  let page = Page.Admin.Settings.Schedule.index context schedules query in
  Lwt.return (Ok page)
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Schedule.Guard.Access.index |> Guardian.validate_admin_entity
end
