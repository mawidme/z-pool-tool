open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message

let session_title (s : Session.t) =
  Pool_common.I18n.SessionDetailTitle (s.Session.start |> Session.Start.value)
;;

let session_path experiment session =
  Format.asprintf
    "/admin/experiments/%s/sessions/%s"
    Experiment.(Id.value experiment.id)
    (Pool_common.Id.value session.Session.id)
;;

let location_select language options selected () =
  let open Pool_location in
  selector
    ~add_empty:true
    ~option_formatter:(fun (l : t) -> l.name |> Name.value)
    ~required:true
    language
    Message.Field.Location
    (fun (l : t) -> l.id |> Id.value)
    options
    selected
    ()
;;

let session_form
  csrf
  language
  (experiment : Experiment.t)
  default_reminder_lead_time
  ?(session : Session.t option)
  ?(follow_up_to : Session.t option)
  ?(duplicate : Session.t option)
  locations
  ~flash_fetcher
  =
  let open CCFun in
  let open Session in
  let open Pool_common in
  let has_assignments =
    session
    |> CCOption.map_or ~default:false (fun s -> s |> Session.has_assignments)
  in
  let default_value_session =
    (* Prefill the form with values if making a duplicate, editing a session or
       creating a follow up to a parent. The importance is 1. duplicate, 2.
       session editing, 3. follow_up_to *)
    CCOption.(duplicate <+> session <+> follow_up_to)
  in
  let reschedule_hint () =
    match session, has_assignments with
    | Some session, true ->
      let action =
        Format.asprintf
          "/admin/experiments/%s/sessions/%s/reschedule"
          (Experiment.Id.value experiment.Experiment.id)
          (Id.value session.Session.id)
        |> Sihl.Web.externalize_path
      in
      p
        [ txt "There are assignments for this session. Please use the "
        ; a
            ~a:[ a_href action ]
            [ txt "form provided to reschedule a session." ]
        ]
    | _ -> txt ""
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") default_value_session in
  let amount fnc = value (fnc %> ParticipantAmount.value %> CCInt.to_string) in
  let action, submit =
    let base =
      Format.asprintf
        "/admin/experiments/%s/sessions"
        (Experiment.Id.value experiment.Experiment.id)
    in
    match session, follow_up_to with
    | None, None -> base, Message.(Create (Some Field.Session))
    | None, Some follow_up_to ->
      ( Format.asprintf
          "%s/%s/follow-up"
          base
          (follow_up_to.Session.id |> Id.value)
      , Message.(Create (Some Field.FollowUpSession)) )
    | Some session, _ ->
      ( Format.asprintf "%s/%s" base (session.id |> Id.value)
      , Message.(Update (Some Field.Session)) )
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ (let value =
             (* Don't want start date filled out in form if creating with
                duplication or follow up *)
             if CCOption.is_some duplicate || CCOption.is_some follow_up_to
             then None
             else
               Some
                 (value (fun s -> s.start |> Start.value |> Ptime.to_rfc3339))
           in
           flatpicker_element
             language
             Message.Field.Start
             ~required:true
             ~flash_fetcher
             ?value
             ~warn_past:true
             ~additional_attributes:
               (if has_assignments then [ a_disabled () ] else []))
        ; timespan_picker
            language
            ~required:true
            Message.Field.Duration
            ~help:I18n.TimeSpanPickerHint
            ?value:
              (CCOption.map
                 (fun (s : t) -> s.duration |> Duration.value)
                 session)
            ~flash_fetcher
            ~additional_attributes:
              (if has_assignments then [ a_disabled () ] else [])
        ; reschedule_hint ()
        ; textarea_element
            language
            Message.Field.Description
            ~value:
              (value (fun s ->
                 s.description |> CCOption.map_or ~default:"" Description.value))
            ~flash_fetcher
        ; location_select
            language
            locations
            (default_value_session |> CCOption.map (fun s -> s.location))
            ()
        ; input_element
            language
            `Number
            Message.Field.MaxParticipants
            ~required:true
            ~value:(amount (fun s -> s.max_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Message.Field.MinParticipants
            ~required:true
            ~value:(amount (fun s -> s.min_participants))
            ~flash_fetcher
        ; input_element
            language
            `Number
            Message.Field.Overbook
            ~required:true
            ~value:(amount (fun s -> s.overbook))
            ~flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt (Utils.text_to_string language I18n.Reminder) ]
        ; div
            ~a:[ a_class [ "grid-col-2" ] ]
            [ div
                [ timespan_picker
                    language
                    Message.Field.LeadTime
                    ~help:I18n.TimeSpanPickerHint
                    ?value:
                      (CCOption.bind session (fun (e : t) ->
                         e.reminder_lead_time
                         |> CCOption.map Reminder.LeadTime.value))
                    ~flash_fetcher
                ; (experiment.Experiment.session_reminder_lead_time
                   |> CCOption.value ~default:default_reminder_lead_time
                   |> fun t ->
                   Utils.text_to_string
                     language
                     (I18n.SessionReminderDefaultLeadTime
                        (t |> Reminder.LeadTime.value))
                   |> txt
                   |> HttpUtils.default_value_style)
                ]
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language submit () ]
    ]
;;

let reschedule_session
  Pool_context.{ csrf; language; _ }
  experiment
  (session : Session.t)
  flash_fetcher
  =
  let open Session in
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/reschedule"
      (Experiment.Id.value experiment.Experiment.id)
      (Id.value session.Session.id)
  in
  form
    ~a:
      [ a_class [ "stack" ]
      ; a_method `Post
      ; a_action (action |> Sihl.Web.externalize_path)
      ]
    [ csrf_element csrf ()
    ; flatpicker_element
        language
        Message.Field.Start
        ~required:true
        ~flash_fetcher
        ~value:(session.start |> Start.value |> Ptime.to_rfc3339)
        ~disable_past:true
    ; timespan_picker
        language
        ~required:true
        Message.Field.Duration
        ~help:I18n.TimeSpanPickerHint
        ~value:(session.duration |> Duration.value)
        ~flash_fetcher
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element
            ~classnames:[ "push" ]
            language
            Message.(Reschedule (Some Field.Session))
            ()
        ]
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control Message.(Reschedule (Some Field.Session)))
       experiment
;;

let waiting_list_radio_button language session =
  let open Pool_common in
  if Session.is_fully_booked session
  then span [ txt (Utils.error_to_string language Message.SessionFullyBooked) ]
  else if CCOption.is_some session.Session.follow_up_to
  then
    span
      [ txt
          (Utils.error_to_string language Message.SessionRegistrationViaParent)
      ]
  else (
    match Session.assignment_creatable session |> CCResult.is_ok with
    | false -> txt ""
    | true ->
      input
        ~a:
          [ a_input_type `Radio
          ; a_name Message.Field.(show Session)
          ; a_value Session.(session.id |> Id.value)
          ]
        ())
;;

let session_list
  layout
  Pool_context.{ language; csrf; _ }
  experiment_id
  grouped_sessions
  chronological
  =
  let open Pool_common in
  let follow_up_icon () =
    span
      ~a:[ a_class [ "font-bold" ] ]
      [ abbr
          ~a:
            [ a_title
                (Utils.field_to_string language Message.Field.FollowUpSession)
            ]
          [ txt "(F)" ]
      ]
  in
  let chronological_id = "chronological-sessions" in
  let add_session_btn =
    link_as_button
      ~style:`Success
      ~icon:`Add
      ~classnames:[ "small" ]
      ~control:(language, Message.(Add (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/create"
         (experiment_id |> Experiment.Id.value))
  in
  let rows =
    CCList.flat_map
      (fun (parent, follow_ups) ->
        let open Session in
        let session_row session follow_ups =
          let delete_form () =
            if Session.is_deletable session follow_ups |> CCResult.is_ok
            then
              form
                ~a:
                  [ a_method `Post
                  ; a_action
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s/delete"
                         (Experiment.Id.value experiment_id)
                         (Id.value session.id)
                       |> Sihl.Web.externalize_path)
                  ; a_user_data
                      "confirmable"
                      (Utils.confirmable_to_string language I18n.DeleteSession)
                  ]
                [ csrf_element csrf ()
                ; submit_element
                    language
                    Message.(Delete None)
                    ~submit_type:`Error
                    ()
                ]
            else
              submit_element
                language
                Message.(Delete None)
                ~submit_type:`Disabled
                ()
          in
          let row_attrs =
            let id = a_user_data "id" (Pool_common.Id.value session.id) in
            session.follow_up_to
            |> CCOption.map (fun parent ->
                 a_user_data "parent-id" (Pool_common.Id.value parent))
            |> CCOption.map_or ~default:[ id ] (fun parent -> [ id; parent ])
          in
          let title =
            let date = span [ txt (session |> session_date_to_human) ] in
            match CCOption.is_some session.follow_up_to, chronological with
            | false, true | false, false -> date
            | true, true ->
              div
                ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                [ date; follow_up_icon () ]
            | true, false -> div ~a:[ a_class [ "inset"; "left" ] ] [ date ]
          in
          let base = [ title ] in
          let closed_at =
            session.closed_at
            |> CCOption.map_or ~default:"" (fun t ->
                 Utils.Time.formatted_date_time t)
            |> txt
          in
          let canceled_at =
            session.canceled_at
            |> CCOption.map_or ~default:"" (fun t ->
                 Utils.Time.formatted_date_time t)
            |> txt
          in
          let cells =
            match layout with
            | `SessionOverview ->
              let cells =
                Session.
                  [ txt
                      (CCInt.to_string
                         (session.assignment_count |> AssignmentCount.value))
                  ; txt
                      (if CCOption.is_some session.closed_at
                       then
                         session.show_up_count
                         |> ShowUpCount.value
                         |> CCInt.to_string
                       else "")
                  ; txt
                      (if CCOption.is_some session.closed_at
                       then
                         session.participant_count
                         |> ParticipantCount.value
                         |> CCInt.to_string
                       else "")
                  ; canceled_at
                  ; closed_at
                  ; div
                      ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
                      [ Format.asprintf
                          "/admin/experiments/%s/sessions/%s"
                          (Experiment.Id.value experiment_id)
                          (Id.value session.id)
                        |> edit_link
                      ; delete_form ()
                      ]
                  ]
              in
              base @ cells
            | `WaitingList ->
              let cells =
                [ waiting_list_radio_button language session
                ; txt
                    (CCInt.to_string
                       (session.assignment_count |> AssignmentCount.value))
                ; txt (session |> Session.available_spots |> CCInt.to_string)
                ; canceled_at
                ; closed_at
                ]
              in
              base @ cells
          in
          cells |> CCList.map CCFun.(CCList.return %> td) |> tr ~a:row_attrs
        in
        session_row parent follow_ups
        :: CCList.map CCFun.(flip session_row []) follow_ups)
      grouped_sessions
  in
  let thead =
    let open Message in
    let base = [ Field.Date ] |> Table.fields_to_txt language in
    let cells =
      match layout with
      | `SessionOverview ->
        base
        @ ([ Field.AssignmentCount
           ; Field.ShowUpCount
           ; Field.ParticipantCount
           ; Field.CanceledAt
           ; Field.ClosedAt
           ]
           |> Table.fields_to_txt language)
        @ [ add_session_btn ]
      | `WaitingList ->
        let to_txt = Table.field_to_txt language in
        base
        @ [ txt ""
          ; Field.AssignmentCount |> to_txt
          ; txt (Utils.text_to_string language I18n.AvailableSpots)
          ; Field.CanceledAt |> to_txt
          ; Field.ClosedAt |> to_txt
          ]
    in
    cells |> Component.Table.table_head
  in
  let table =
    let id = if chronological then [ a_id chronological_id ] else [] in
    table
      ~a:([ a_class [ "table"; "striped"; "align-last-end" ] ] @ id)
      ~thead
      rows
  in
  let hover_script =
    match chronological with
    | false -> txt ""
    | true ->
      let js =
        {js|
          const highlight = "highlighted";

          document.addEventListener("DOMContentLoaded", () => {
            const table = document.getElementById("chronological-sessions");
            const toggleClass = (e) => {
              const { id, parentId } = e.currentTarget.dataset;
              if (parentId) {
                table
                  .querySelector(`[data-id='${parentId}']`)
                  .classList.toggle(highlight);
              } else {
                table.querySelectorAll(`[data-parent-id='${id}']`).forEach((tr) => {
                  tr.classList.toggle(highlight);
                });
              }
              e.currentTarget.classList.toggle(highlight);
            };
            table.querySelectorAll("tbody tr").forEach((row) => {
              row.addEventListener("mouseenter", (e) => {
                toggleClass(e);
              });
              row.addEventListener("mouseleave", (e) => {
                toggleClass(e);
              });
            });
          });
      |js}
      in
      script (Unsafe.data js)
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ p [ I18n.SessionIndent |> Utils.text_to_string language |> txt ]
    ; a
        ~a:
          [ a_href
              (if chronological
               then "?"
               else
                 Format.asprintf "?%s=true" Message.Field.(show Chronological))
          ]
        [ (if chronological
           then I18n.SwitchGrouped
           else I18n.SwitchChronological)
          |> Utils.text_to_string language
          |> txt
        ]
      (* TODO [aerben] allow tables to be sorted generally? *)
    ; (if chronological
       then
         p
           [ txt "Sessions marked with "
           ; follow_up_icon ()
           ; txt " are follow-up sessions."
           ]
       else txt "")
    ; table
    ; hover_script
    ]
;;

let index
  (Pool_context.{ language; _ } as context)
  experiment
  grouped_sessions
  chronological
  =
  let open Pool_common in
  let html =
    session_list
      `SessionOverview
      context
      experiment.Experiment.id
      grouped_sessions
      chronological
  in
  Page_admin_experiments.experiment_layout
    ~hint:I18n.ExperimentSessions
    language
    (Page_admin_experiments.NavLink I18n.Sessions)
    experiment
    ~active:I18n.Sessions
    html
;;

let new_form
  Pool_context.{ language; csrf; _ }
  experiment
  default_reminder_lead_time
  duplicate_session
  locations
  flash_fetcher
  =
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.Control Message.(Create (Some Field.Session)))
    experiment
    (session_form
       csrf
       language
       experiment
       default_reminder_lead_time
       ?duplicate:duplicate_session
       locations
       ~flash_fetcher)
;;

let detail
  (Pool_context.{ language; _ } as context)
  experiment
  (session : Session.t)
  assignments
  =
  let open Session in
  let open Pool_common in
  let session_link ?style (show, url, control) =
    let style, icon =
      style |> CCOption.map_or ~default:(`Primary, None) CCFun.id
    in
    match show with
    | false -> None
    | true ->
      link_as_button
        ~control:(language, control)
        ~classnames:[ "small" ]
        ~style
        ?icon
        (Format.asprintf
           "/admin/experiments/%s/sessions/%s/%s"
           (Experiment.Id.value experiment.Experiment.id)
           (Id.value session.id)
           url)
      |> CCOption.pure
  in
  let session_overview =
    let table =
      let open Message in
      let parent =
        CCOption.map
          (fun follow_up_to ->
            ( Field.MainSession
            , a
                ~a:
                  [ a_href
                      (Format.asprintf
                         "/admin/experiments/%s/sessions/%s"
                         (Experiment.Id.value experiment.Experiment.id)
                         (Id.value follow_up_to)
                       |> Sihl.Web.externalize_path)
                  ]
                [ Message.Show
                  |> Utils.control_to_string language
                  |> CCString.capitalize_ascii
                  |> txt
                ] ))
          session.follow_up_to
      in
      let rows =
        let amount amt = amt |> ParticipantAmount.value |> string_of_int in
        [ ( Field.Start
          , session.start
            |> Start.value
            |> Utils.Time.formatted_date_time
            |> txt )
        ; ( Field.Duration
          , session.duration
            |> Duration.value
            |> Utils.Time.formatted_timespan
            |> txt )
        ; ( Field.Description
          , CCOption.map_or ~default:"" Description.value session.description
            |> Http_utils.add_line_breaks )
        ; ( Field.Location
          , Partials.location_to_html language session.Session.location )
        ; Field.MaxParticipants, amount session.max_participants |> txt
        ; Field.MinParticipants, amount session.min_participants |> txt
        ; Field.Overbook, amount session.overbook |> txt
        ]
        |> fun rows ->
        let canceled =
          session.canceled_at
          |> CCOption.map (fun c ->
               Field.CanceledAt, Utils.Time.formatted_date_time c |> txt)
        in
        let closed =
          session.closed_at
          |> CCOption.map (fun c ->
               Field.ClosedAt, Utils.Time.formatted_date_time c |> txt)
        in
        rows @ ([ canceled; closed ] |> CCList.filter_map CCFun.id)
      in
      Table.vertical_table `Striped language ~align_top:true
      @@ CCOption.map_or ~default:rows (CCList.cons' rows) parent
    in
    let links =
      let duplicate =
        let base =
          Format.asprintf
            "/admin/experiments/%s/sessions"
            (Experiment.Id.value experiment.Experiment.id)
        in
        let id_value = Pool_common.Id.value in
        let link =
          match session.follow_up_to with
          | Some parent_session ->
            Format.asprintf
              "%s/%s/follow-up?duplicate_id=%s"
              base
              (id_value parent_session)
              (id_value session.id)
          | None ->
            Format.asprintf
              "%s/create/?duplicate_id=%s"
              base
              (id_value session.id)
        in
        link_as_button
          ~control:(language, Message.Duplicate (Some Field.Session))
          ~classnames:[ "small" ]
          link
      in
      let wrap = div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] in
      let right =
        Message.
          [ ( CCOption.is_none session.follow_up_to
            , "follow-up"
            , Create (Some Field.FollowUpSession) )
          ]
        |> CCList.filter_map session_link
        |> CCList.cons duplicate
        |> wrap
      in
      let left =
        Message.
          [ ( ( session.assignment_count |> AssignmentCount.value > 0
                && CCOption.is_none session.closed_at
              , "reschedule"
              , Reschedule (Some Field.Session) )
            , None )
          ; ( ( session |> is_closable |> CCResult.is_ok
              , "close"
              , Close (Some Field.Session) )
            , None )
          ; ( ( session |> is_cancellable |> CCResult.is_ok
              , "cancel"
              , Cancel (Some Field.Session) )
            , Some (`Error, Some `CloseCircle) )
          ]
        |> CCList.filter_map (fun (t, style) -> session_link ?style t)
        |> wrap
      in
      div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between" ] ]
        [ left; right ]
    in
    div ~a:[ a_class [ "stack" ] ] [ table; links ]
  in
  let assignments_html =
    let assignment_list =
      Page_admin_assignments.(
        Partials.overview_list
          Session
          context
          experiment.Experiment.id
          session
          assignments)
    in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt (Utils.nav_link_to_string language I18n.Assignments) ]
      ; assignment_list
      ]
  in
  let edit_button =
    link_as_button
      ~icon:`Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Session)))
      (Format.asprintf
         "/admin/experiments/%s/sessions/%s/edit"
         (Experiment.Id.value experiment.Experiment.id)
         (Id.value session.id))
  in
  let html =
    div ~a:[ a_class [ "stack-lg" ] ] [ session_overview; assignments_html ]
  in
  Page_admin_experiments.experiment_layout
    ~buttons:edit_button
    language
    (Page_admin_experiments.I18n (session_title session))
    experiment
    html
;;

let edit
  Pool_context.{ language; csrf; _ }
  experiment
  default_reminder_lead_time
  (session : Session.t)
  locations
  session_reminder_templates
  sys_languages
  flash_fetcher
  =
  let open Message_template in
  let session_path =
    Format.asprintf "%s/%s" (session_path experiment session)
  in
  let form =
    div
      [ p
          [ txt
              (session
               |> session_title
               |> Pool_common.Utils.text_to_string language)
          ]
      ; session_form
          csrf
          language
          experiment
          default_reminder_lead_time
          ~session
          locations
          ~flash_fetcher
      ]
  in
  let message_templates_html label list =
    let edit_path m =
      Message_template.prefixed_template_url ~append:"edit" m |> session_path
    in
    let new_path =
      if CCList.is_empty (Message_template.filter_languages sys_languages list)
      then None
      else session_path Label.(prefixed_human_url label) |> CCOption.pure
    in
    div
      [ h3 ~a:[ a_class [ "heading-2" ] ] [ txt (Label.to_human label) ]
      ; Page_admin_message_template.table language list new_path edit_path
      ]
  in
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ form
      ; message_templates_html Label.SessionReminder session_reminder_templates
      ]
  in
  html
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control Message.(Edit (Some Field.Session)))
       experiment
;;

let follow_up
  Pool_context.{ language; csrf; _ }
  experiment
  default_reminder_lead_time
  duplicate_session
  (parent_session : Session.t)
  locations
  flash_fetcher
  =
  let open Pool_common in
  div
    [ p
        [ txt
            Utils.(
              parent_session
              |> session_title
              |> text_to_string language
              |> CCFormat.asprintf
                   "%s %s"
                   (I18n.FollowUpSessionFor |> text_to_string language))
        ]
    ; session_form
        csrf
        language
        experiment
        default_reminder_lead_time
        ~follow_up_to:parent_session
        ?duplicate:duplicate_session
        locations
        ~flash_fetcher
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control
          Message.(Create (Some Field.FollowUpSession)))
       experiment
;;

let close
  Pool_context.{ language; csrf; _ }
  experiment
  (session : Session.t)
  assignments
  =
  let open Pool_common in
  let control = Message.(Close (Some Field.Session)) in
  let form =
    let checkbox_element ?(disabled = false) contact field =
      let disabled = if disabled then [ a_disabled () ] else [] in
      div
        [ input
            ~a:
              ([ a_input_type `Checkbox
               ; a_name Message.Field.(array_key field)
               ; a_value (contact |> Contact.id |> Id.value)
               ]
               @ disabled)
            ()
        ]
    in
    let table =
      let link (id, label) =
        span
          ~a:[ a_id id ]
          [ abbr
              ~a:
                [ a_title
                    Pool_common.(
                      Utils.control_to_string language Message.ToggleAll
                      |> CCString.capitalize_ascii)
                ]
              [ txt label ]
          ]
      in
      let thead =
        txt ""
        :: ([ "all-showup", "S"; "all-participated", "P" ] |> CCList.map link)
      in
      CCList.map
        (fun ({ Assignment.contact; _ } : Assignment.t) ->
          [ div [ strong [ txt (Contact.fullname contact) ] ]
          ; checkbox_element contact Message.Field.ShowUp
          ; checkbox_element ~disabled:true contact Message.Field.Participated
          ])
        assignments
      |> Table.horizontal_table ~thead `Striped
      |> fun table ->
      form
        ~a:
          [ a_method `Post
          ; a_class [ "stack" ]
          ; a_action
              (Format.asprintf
                 "/admin/experiments/%s/sessions/%s/close"
                 (Experiment.Id.value experiment.Experiment.id)
                 (Id.value session.Session.id)
               |> Sihl.Web.externalize_path)
          ; a_user_data "detect-unsaved-changes" ""
          ]
        [ Input.csrf_element csrf ()
        ; table
        ; div
            ~a:[ a_class [ "flexrow"; "justify-end" ] ]
            [ Input.submit_element language control ~submit_type:`Primary () ]
        ]
    in
    let scripts =
      {js|
        const showUp = document.querySelectorAll('[name="show_up[]"]');
        const participated = document.querySelectorAll('[name="participated[]"]');
        for(let i = 0; i < showUp.length; i++) {
          let elm = showUp[i];
          let target = document.querySelector(`[name="participated[]"][value="${elm.value}"]`)
          elm.addEventListener("change", () => {
            target.disabled = !elm.checked;
          })
        }

        const isActive = (elm) => {
          return elm.dataset.active;
        }

        const toggleActive = (elm, state) => {
          const newState = state == null ? !isActive(elm) : state;
          if(newState) {
            elm.dataset.active = true;
          } else {
            elm.removeAttribute("data-active");
          }
        }

        function setAllShowUp(value) {
          showUp.forEach((elm) => {
            var event = new Event('change');
            elm.checked = value;
            elm.dispatchEvent(event);
          });
        }

        const toggleShowUp = document.getElementById("all-showup");
        toggleShowUp.addEventListener("click", () => {
          setAllShowUp(!isActive(toggleShowUp));
          toggleActive(toggleShowUp);
        })

        const toggleParticipated = document.getElementById("all-participated");
        toggleParticipated.addEventListener("click", () => {
          const state = !isActive(toggleParticipated);
          if(state) {
            setAllShowUp(true);
            toggleActive(toggleShowUp, true);
          }
          participated.forEach((elm) => {
            elm.checked = state;
          });
          toggleActive(toggleParticipated);
        })
      |js}
    in
    div
      [ h4
          ~a:[ a_class [ "heading-4" ] ]
          [ txt
              (Utils.field_to_string language Message.Field.Participants
               |> CCString.capitalize_ascii)
          ]
      ; p
          [ Utils.hint_to_string language I18n.SessionClose
            |> HttpUtils.add_line_breaks
          ]
      ; table
      ; script (Unsafe.data scripts)
      ]
  in
  div
    [ p [ txt (session |> session_title |> Utils.text_to_string language) ]
    ; form
    ]
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control control)
       experiment
;;

let cancel
  Pool_context.{ language; csrf; _ }
  experiment
  (session : Session.t)
  flash_fetcher
  =
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s/cancel"
      (Experiment.Id.value experiment.Experiment.id)
      (Id.value session.Session.id)
  in
  (match Session.is_cancellable session with
   | Error reason -> p [ reason |> Utils.error_to_string language |> txt ]
   | Ok _ ->
     form
       ~a:
         [ a_class [ "stack" ]
         ; a_method `Post
         ; a_action (action |> Sihl.Web.externalize_path)
         ]
       [ csrf_element csrf ()
       ; textarea_element ~flash_fetcher language Message.Field.Reason
       ; span
           [ I18n.SessionCancelMessage |> Utils.hint_to_string language |> txt ]
       ; p [ I18n.NotifyVia |> Utils.text_to_string language |> txt ]
       ; checkbox_element ~flash_fetcher language Message.Field.Email
         (* TODO issue #149 re-add this *)
         (* ; checkbox_element ~flash_fetcher language Message.Field.SMS *)
       ; div
           ~a:[ a_class [ "flexrow" ] ]
           [ submit_element
               ~classnames:[ "push" ]
               language
               Message.(Cancel (Some Field.Session))
               ()
           ]
       ])
  |> Page_admin_experiments.experiment_layout
       language
       (Page_admin_experiments.Control Message.(Cancel (Some Field.Session)))
       experiment
;;

let message_template_form
  ({ Pool_context.language; _ } as context)
  tenant
  experiment
  session
  languages
  label
  template
  flash_fetcher
  =
  let open Message_template in
  let action =
    let go =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s/%s"
        Experiment.(Id.value experiment.Experiment.id)
        (Pool_common.Id.value session.Session.id)
    in
    match template with
    | None -> go (Label.prefixed_human_url label)
    | Some template -> prefixed_template_url template |> go
  in
  let title =
    let open Pool_common in
    (match template with
     | None -> Message.(Create None)
     | Some _ -> Message.(Edit None))
    |> fun control ->
    Page_admin_experiments.String
      (Format.asprintf
         "%s %s"
         (control |> Utils.control_to_string language)
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      ~experiment
      ~session
      language
      tenant
      label
  in
  Page_admin_message_template.template_form
    context
    ~text_elements
    ?languages
    template
    action
    flash_fetcher
  |> Page_admin_experiments.experiment_layout language title experiment
;;
