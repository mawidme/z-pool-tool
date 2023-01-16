module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let create req =
  let open Utils.Lwt_result.Infix in
  let tags = Logger.req req in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Pool_common.Id.of_string Session req )
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* experiment =
         Experiment.find_public database_label experiment_id contact
       in
       let* session = Session.find_public database_label id in
       let* follow_ups =
         Session.find_follow_ups database_label id
         >|+ CCList.map Session.to_public
       in
       let* waiting_list =
         Waiting_list.find_by_contact_and_experiment
           database_label
           contact
           experiment
       in
       let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* confirmation_email =
         let* language =
           let* default = Settings.default_language database_label in
           contact.Contact.language
           |> CCOption.value ~default
           |> Lwt_result.return
         in
         Message_template.AssignmentConfirmation.create_from_public_session
           database_label
           language
           tenant
           session
           contact
       in
       let%lwt already_enrolled =
         let open Utils.Lwt_result.Infix in
         Assignment.find_by_experiment_and_contact_opt
           database_label
           experiment.Experiment.Public.id
           contact
         ||> CCList.is_empty
         ||> not
       in
       let events =
         (* TODO: merge emails, one confirmation for all sessions *)
         let open Cqrs_command.Assignment_command.Create in
         CCList.map
           (fun (session, waiting_list) ->
             handle
               ~tags
               { contact; session; waiting_list; experiment }
               confirmation_email
               already_enrolled)
           ((session, waiting_list) :: CCList.map (fun m -> m, None) follow_ups)
         |> CCResult.flatten_l
         |> CCResult.map CCList.flatten
         |> Lwt_result.lift
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ HttpUtils.Message.set
               ~success:[ Pool_common.Message.(AssignmentCreated) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
