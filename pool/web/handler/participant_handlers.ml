module Command = Cqrs_command.Participant_command
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module User = Pool_user

let create_layout req = General.create_tenant_layout `Participant req

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    Page.Participant.dashboard context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
    |> Lwt_result.map_err (fun err -> err, "/index")
  in
  result |> HttpUtils.extract_happy_path req
;;

let sign_up req =
  let result context =
    let open Lwt_result.Syntax in
    let open Lwt_result.Infix in
    Lwt_result.map_err (fun err -> err, "/index")
    @@
    let csrf = HttpUtils.find_csrf req in
    let message =
      Sihl.Web.Flash.find_alert req
      |> CCFun.flip CCOption.bind Message.of_string
    in
    let go = CCFun.flip Sihl.Web.Flash.find req in
    let channels = Participant.RecruitmentChannel.all () in
    let email = go "email" in
    let firstname = go "firstname" in
    let lastname = go "lastname" in
    let recruitment_channel = go "recruitment_channel" in
    let tenant_db = context.Pool_context.tenant_db in
    let language = context.Pool_context.language in
    let* terms = Settings.terms_and_conditions tenant_db language in
    Page.Participant.sign_up
      csrf
      channels
      email
      firstname
      lastname
      recruitment_channel
      terms
      context
    |> create_layout req context message
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let sign_up_create req =
  let open Utils.Lwt_result.Infix in
  let terms_key = "_terms_accepted" in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    let path_with_lang = HttpUtils.path_with_language query_lang in
    Lwt_result.map_err (fun msg ->
        ( msg
        , path_with_lang "/signup"
        , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* () =
         CCList.assoc ~eq:( = ) terms_key urlencoded
         |> CCList.hd
         |> CCString.equal "true"
         |> Utils.Bool.to_result
              Pool_common.Message.TermsAndConditionsNotAccepted
         |> Lwt_result.lift
       in
       let tenant_db = context.Pool_context.tenant_db in
       let* remove_participant_event =
         let find_participant email =
           email
           |> Participant.find_by_email tenant_db
           ||> function
           | Ok partitipant ->
             if partitipant.Participant.user.Sihl_user.confirmed
             then Error Pool_common.Message.EmailAlreadyInUse
             else Ok (Some partitipant)
           | Error _ -> Ok None
         in
         Sihl.Web.Request.urlencoded "email" req
         ||> CCOption.to_result
               Pool_common.Message.ParticipantSignupInvalidEmail
         >== Pool_user.EmailAddress.create
         >>= find_participant
         >>= CCOption.map_or ~default:(Lwt_result.return []) (fun p ->
                 Command.DeleteUnverified.handle p |> Lwt_result.lift)
       in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes tenant_db
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let preferred_language = HttpUtils.browser_language_from_req req in
       let* events =
         let open CCResult.Infix in
         Command.SignUp.(
           decode urlencoded
           >>= handle ?allowed_email_suffixes preferred_language)
         >>= (fun e -> Ok (remove_participant_event @ e))
         |> Lwt_result.lift
       in
       Utils.Database.with_transaction tenant_db (fun () ->
           let%lwt () = Pool_event.handle_events tenant_db events in
           HttpUtils.redirect_to_with_actions
             (path_with_lang "/email-confirmation")
             [ Message.set
                 ~success:[ Pool_common.Message.EmailConfirmationMessage ]
             ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    let tenant_db = context.Pool_context.tenant_db in
    let%lwt redirect_path =
      let%lwt user = Http_utils.user_from_session tenant_db req in
      CCOption.bind user (fun user ->
          Some (General.dashboard_path tenant_db query_lang user))
      |> CCOption.value
           ~default:
             ("/login" |> HttpUtils.path_with_language query_lang |> Lwt.return)
    in
    (let* token =
       Sihl.Web.Request.query Pool_common.Message.(field_name Token) req
       |> CCOption.map Email.Token.create
       |> CCOption.to_result Pool_common.Message.(NotFound Token)
       |> Lwt_result.lift
     in
     let ctx = Pool_tenant.to_ctx tenant_db in
     let* email =
       Service.Token.read ~ctx (Email.Token.value token) ~k:"email"
       ||> CCOption.to_result Pool_common.Message.TokenInvalidFormat
       >== Pool_user.EmailAddress.create
       >>= Email.find_unverified_by_address tenant_db
       |> Lwt_result.map_err (fun _ -> Pool_common.Message.(Invalid Token))
     in
     let* participant = Participant.find tenant_db (Email.user_id email) in
     let* events =
       match participant.Participant.user.Sihl.Contract.User.confirmed with
       | false ->
         Command.VerifyAccount.(handle { email } participant) |> Lwt_result.lift
       | true ->
         Command.UpdateEmail.(handle participant email) |> Lwt_result.lift
     in
     let%lwt () = Pool_event.handle_events tenant_db events in
     HttpUtils.redirect_to_with_actions
       redirect_path
       [ Message.set ~success:[ Pool_common.Message.EmailVerified ] ]
     |> Lwt_result.ok)
    |> Lwt_result.map_err (fun msg -> msg, redirect_path)
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let csrf = HttpUtils.find_csrf req in
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  let result context =
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun err -> err, "/login")
    @@ let* user =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound User)
       in
       let language = context.Pool_context.language in
       let* terms = Settings.terms_and_conditions tenant_db language in
       Page.Participant.terms csrf user.Sihl_user.id terms context
       |> create_layout req context message
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let terms_accept req =
  let result context =
    let query_lang = context.Pool_context.query_language in
    Lwt_result.map_err (fun msg ->
        msg, "/login" |> HttpUtils.path_with_language query_lang)
    @@
    let open Lwt_result.Syntax in
    let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
    let tenant_db = context.Pool_context.tenant_db in
    let* participant = Participant.find tenant_db id in
    let* events =
      Command.AcceptTermsAndConditions.handle participant |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    HttpUtils.(redirect_to (path_with_language query_lang "/dashboard"))
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let user_update_csrf = "_user_update_csrf"

let show is_edit req =
  let result context =
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    let append_lang = HttpUtils.path_with_language query_lang in
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun err -> err, append_lang "/login")
    @@ let* user =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound User)
       in
       let* participant =
         Participant.find
           tenant_db
           (user.Sihl_user.id |> Pool_common.Id.of_string)
         |> Lwt_result.map_err (fun err -> err)
       in
       let message =
         CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
       in
       match is_edit with
       | false ->
         Page.Participant.detail participant context
         |> create_layout req context message
         >|= Sihl.Web.Response.of_html
       | true ->
         let csrf = HttpUtils.find_csrf req in
         let* tenant_languages =
           Pool_context.Tenant.find req
           |> Lwt_result.lift
           >|= fun c -> c.Pool_context.Tenant.tenant_languages
         in
         Page.Participant.edit
           csrf
           user_update_csrf
           participant
           tenant_languages
           context
         |> create_layout req context message
         >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let details = show false
let edit = show true

let update req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values [ "paused" ]
  in
  let result context =
    let query_lang = context.Pool_context.query_language in
    let path_with_lang = HttpUtils.path_with_language query_lang in
    let go name = CCList.assoc ~eq:String.equal name urlencoded |> CCList.hd in
    let version_raw = go "version" in
    let name = go "field" in
    let open Utils.Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* user =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result
            Pool_common.Message.(NotFound User, path_with_lang "/login")
    in
    let* participant =
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_err (fun err -> err, path_with_lang "/login")
    in
    let language = context.Pool_context.language in
    let* tenant_context =
      Pool_context.Tenant.find req
      |> Lwt_result.lift
      |> Lwt_result.map_err (fun err -> err, path_with_lang "/user/edit")
    in
    let version =
      version_raw
      |> CCInt.of_string
      |> CCOption.get_exn_or
           (Pool_common.Utils.error_to_string
              language
              Pool_common.Message.(NotANumber name))
    in
    let field_name =
      let open Pool_common in
      function
      | "firstname" -> Message.Firstname
      | "lastname" -> Message.Lastname
      | "paused" -> Message.Paused
      | "language" -> Message.DefaultLanguage
      | k -> failwith @@ Utils.error_to_string language Message.(NotHandled k)
    in
    let get_version =
      CCOption.get_exn_or
        (Pool_common.Utils.error_to_string
           language
           Pool_common.Message.(NotHandled name))
    in
    let current_version =
      Participant.version_selector participant name |> get_version
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Participant_command.UpdateDetails in
      if Pool_common.Version.value current_version <= version
      then urlencoded |> decode >>= handle participant
      else Error (Pool_common.Message.MeantimeUpdate (field_name name))
    in
    let hx_post = Sihl.Web.externalize_path (path_with_lang "/user/update") in
    let csrf = HttpUtils.find_csrf req in
    let htmx_element participant classnames ?error () =
      let open Participant in
      let csrf_element = Htmx.csrf_element_swap csrf ~id:user_update_csrf () in
      let html_response input =
        [ Htmx.create input language ~classnames ~hx_post ?error ()
        ; csrf_element
        ]
        |> HttpUtils.multi_html_to_plain_text_response
      in
      Lwt_result.return
      @@
      match name with
      | "paused" ->
        Htmx.Paused (participant.paused_version, participant.paused)
        |> html_response
      | "firstname" ->
        Htmx.Firstname (participant.firstname_version, participant |> firstname)
        |> html_response
      | "lastname" ->
        Htmx.Lastname (participant.lastname_version, participant |> lastname)
        |> html_response
      | "language" ->
        (match error with
        | Some _ ->
          Htmx.Language
            ( participant.language_version
            , participant.language
            , tenant_context.Pool_context.Tenant.tenant_languages )
          |> html_response
        | None ->
          Sihl.Web.Response.of_plain_text ""
          |> Sihl.Web.Response.add_header ("HX-Redirect", "/user/edit"))
      | k ->
        failwith
        @@ Pool_common.Utils.error_to_string
             language
             Pool_common.Message.(NotHandled k)
    in
    match events with
    | Ok events ->
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      let* participant =
        Participant.(participant |> id |> find tenant_db)
        |> Lwt_result.map_err (fun err -> err, "/login")
      in
      htmx_element participant [ "success" ] ()
    | Error err -> htmx_element participant [ "error" ] ~error:err ()
  in
  Lwt.catch
    (fun () -> result |> HttpUtils.extract_happy_path req)
    (fun exn ->
      Logs.err (fun m -> m "%s" @@ Printexc.to_string exn);
      Sihl.Web.Response.of_plain_text ""
      |> Sihl.Web.Response.add_header ("HX-Redirect", "/error")
      |> Lwt.return)
;;

let update_email req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    Lwt_result.map_err (fun msg ->
        HttpUtils.(
          ( msg
          , path_with_language query_lang "/user/edit"
          , [ urlencoded_to_flash urlencoded ] )))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* participant =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound User)
      >>= fun user ->
      Participant.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
    in
    let%lwt allowed_email_suffixes =
      let open Utils.Lwt_result.Infix in
      Settings.find_email_suffixes tenant_db
      ||> fun suffixes ->
      if CCList.is_empty suffixes then None else Some suffixes
    in
    let* new_email =
      Pool_user.EmailAddress.create
        (CCList.assoc ~eq:CCString.equal "email" urlencoded |> CCList.hd)
      |> Lwt_result.lift
    in
    let* events =
      Command.RequestEmailValidation.(
        handle ?allowed_email_suffixes participant new_email |> Lwt_result.lift)
    in
    Utils.Database.with_transaction tenant_db (fun () ->
        let%lwt () = Pool_event.handle_events tenant_db events in
        HttpUtils.(
          redirect_to_with_actions
            (path_with_language query_lang "/email-confirmation")
            [ Message.set
                ~success:[ Pool_common.Message.EmailConfirmationMessage ]
            ]))
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update_password req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_context.query_language in
    let tenant_db = context.Pool_context.tenant_db in
    Lwt_result.map_err (fun msg ->
        HttpUtils.(
          ( msg
          , path_with_language query_lang "/user/edit"
          , [ urlencoded_to_flash urlencoded ] )))
    @@ let* participant =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound User)
         >>= fun user ->
         Participant.find
           tenant_db
           (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       let* events =
         let open CCResult.Infix in
         Command.UpdatePassword.(decode urlencoded >>= handle participant)
         |> Lwt_result.lift
       in
       Utils.Database.with_transaction tenant_db (fun () ->
           let%lwt () = Pool_event.handle_events tenant_db events in
           HttpUtils.(
             redirect_to_with_actions
               (path_with_language query_lang "/user/edit")
               [ Message.set ~success:[ Pool_common.Message.PasswordChanged ] ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;