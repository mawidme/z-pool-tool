module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let index req =
  let open Utils.Lwt_result.Infix in
  let id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db id in
       let%lwt filtered_contacts =
         Contact.find_filtered
           tenant_db
           experiment.Experiment.id
           experiment.Experiment.filter
       in
       let* invitations =
         Invitation.find_by_experiment tenant_db experiment.Experiment.id
       in
       Page.Admin.Experiments.invitations
         invitations
         experiment
         filtered_contacts
         context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@ let* contact_ids =
         let open Lwt.Infix in
         Sihl.Web.Request.urlencoded_list
           Pool_common.Message.Field.(Contacts |> array_key)
           req
         >|= CCList.map Pool_common.Id.of_string
         >|= fun list ->
         if CCList.is_empty list
         then Error Pool_common.Message.(NoOptionSelected Field.Contact)
         else Ok list
       in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* contacts =
         let find_missing contacts =
           let retrieved_ids = CCList.map Contact.id contacts in
           CCList.fold_left
             (fun missing id ->
               match CCList.mem ~eq:Pool_common.Id.equal id retrieved_ids with
               | true -> missing
               | false -> CCList.cons id missing)
             []
             contact_ids
         in
         let%lwt contacts = Contact.find_multiple tenant_db contact_ids in
         Lwt_result.lift
         @@
         match CCList.length contact_ids == CCList.length contacts with
         | true -> Ok contacts
         | false ->
           find_missing contacts
           |> CCList.map Pool_common.Id.value
           |> fun ids ->
           Error Pool_common.Message.(NotFoundList (Field.Contacts, ids))
       in
       let* default_language = Settings.default_language tenant_db in
       let%lwt invited_contacts =
         Invitation.find_multiple_by_experiment_and_contacts
           tenant_db
           (CCList.map Contact.id contacts)
           experiment
       in
       let%lwt events =
         Cqrs_command.Invitation_command.Create.(
           handle { experiment; contacts; invited_contacts } default_language
           |> Lwt_result.lift)
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
           ]
       in
       events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    let open HttpUtils in
    ( get_field_router_param req Experiment |> Pool_common.Id.of_string
    , get_field_router_param req Invitation |> Pool_common.Id.of_string )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@ let* invitation = Invitation.find tenant_db id in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* default_language = Settings.default_language tenant_db in
       let events =
         Cqrs_command.Invitation_command.Resend.handle
           Invitation.{ invitation; experiment }
           default_language
         |> Lwt.return
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;