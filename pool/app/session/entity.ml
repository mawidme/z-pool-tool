module Description = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Description
  let schema () = schema field ()
end

(* TODO [aerben] rename to contact *)
module ParticipantAmount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create amount =
    if amount < 0 then Error Pool_common.Message.(NegativeAmount) else Ok amount
  ;;

  let schema field =
    let decode str =
      let open CCResult in
      CCInt.of_string str
      |> CCOption.to_result Pool_common.Message.(NotANumber str)
      >>= create
    in
    Pool_common.(Utils.schema_decoder decode CCInt.to_string field)
  ;;
end

module Start = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let value m = m
  let compare = Ptime.compare

  let schema () =
    let decode str =
      let open CCResult in
      Pool_common.(Utils.Time.parse_time str >|= create)
    in
    Pool_common.(
      Utils.schema_decoder decode Ptime.to_rfc3339 Message.Field.Start)
  ;;
end

module Duration = struct
  include Pool_common.Model.PtimeSpan

  let create m =
    if Ptime.Span.abs m |> Ptime.Span.equal m
    then Ok m
    else Error Pool_common.Message.NegativeAmount
  ;;

  let field = Pool_common.Message.Field.Duration
  let schema = schema field create
end

module AssignmentCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.AssignmentCount)
    else Ok m
  ;;
end

module ShowUpCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.ShowUpCount)
    else Ok m
  ;;
end

module ParticipantCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.ParticipantCount)
    else Ok m
  ;;
end

module CancellationReason = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Reason

  let validate m =
    if CCString.is_empty m then Error Pool_common.Message.NoValue else Ok m
  ;;

  let schema = schema ?validation:(Some validate) field
end

type t =
  { id : Pool_common.Id.t
  ; follow_up_to : Pool_common.Id.t option
  ; has_follow_ups : bool
  ; start : Start.t
  ; duration : Ptime.Span.t
  ; description : Description.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; assignment_count : AssignmentCount.t
  ; show_up_count : ShowUpCount.t
  ; participant_count : ParticipantCount.t
  ; (* TODO [aerben] want multiple follow up session?
     * 1. Ja es gibt immer wieder Sessions mit mehreren Following Sessions
     * 2. Eigentlich ist es immer eine Hauptsession mit mehreren Following Sessions

     * Could this model as the following, just flatten tail of linked list
     *  : ; follow_up : t *)
    (* TODO [aerben] make type for canceled_at? *)
    closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

(* TODO [aerben] need insertion multiple session? *)
(* TODO [aerben] do session copying *)
(* TODO [aerben] write tests *)

let create
  ?id
  ?follow_up_to
  ?(has_follow_ups = false)
  start
  duration
  description
  location
  max_participants
  min_participants
  overbook
  reminder_lead_time
  =
  { id = id |> CCOption.value ~default:(Pool_common.Id.create ())
  ; follow_up_to
  ; has_follow_ups
  ; start
  ; duration
  ; description
  ; location
  ; max_participants
  ; min_participants
  ; overbook
  ; reminder_lead_time
  ; reminder_sent_at = None
  ; assignment_count = 0
  ; show_up_count = 0
  ; participant_count = 0
  ; closed_at = None
  ; canceled_at = None
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

let is_fully_booked (m : t) =
  m.assignment_count >= m.max_participants + m.overbook
;;

let has_assignments m = AssignmentCount.value m.assignment_count > 0

type assignments =
  { session : t
  ; assignments : Assignment.t list
  }

type notification_log =
  | Email of Sihl_email.t * Sihl_queue.instance
  | SMS of string * Sihl_queue.instance
(* TODO: update notification history with types, add equal and pp functions *)

type notification_history =
  { session : t
  ; queue_entries : (Sihl_email.t * Sihl_queue.instance) list
       [@equal fun _ _ -> true]
  }

let find_by_experiment (_ : string) : t list Lwt.t = Lwt.return []

let session_date_to_human (session : t) =
  session.start |> Start.value |> Pool_common.Utils.Time.formatted_date_time
;;

let compare_start s1 s2 = Start.compare s1.start s2.start

let add_follow_ups_to_parents groups (parent, session) =
  CCList.Assoc.update
    ~eq:Pool_common.Id.equal
    ~f:(fun s ->
      match s with
      | None -> None
      | Some (parent, ls) -> Some (parent, ls @ [ session ]))
    parent
    groups
;;

(* Group follow ups into main sessions and sort by start date *)
let group_and_sort sessions =
  let parents, follow_ups =
    sessions
    |> CCList.partition_filter_map (fun session ->
         match session.follow_up_to with
         | None -> `Left (session.id, (session, []))
         | Some parent -> `Right (parent, session))
  in
  follow_ups
  |> CCList.fold_left add_follow_ups_to_parents parents
  |> CCList.map (fun (_, (p, fs)) -> p, CCList.sort compare_start fs)
  |> CCList.sort (fun (f1, _) (f2, _) -> compare_start f1 f2)
;;

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; follow_up_to : Pool_common.Id.t option
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; description : Description.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]

  let is_fully_booked (m : t) =
    m.assignment_count >= m.max_participants + m.overbook
  ;;

  let compare_start (s1 : t) (s2 : t) = Start.compare s1.start s2.start

  let add_follow_ups_and_sort parents =
    let open CCFun in
    CCList.fold_left add_follow_ups_to_parents parents
    %> CCList.map (fun (_, (p, fs)) -> p, CCList.sort compare_start fs)
    %> CCList.sort (fun (f1, _) (f2, _) -> compare_start f1 f2)
  ;;

  (* Group follow ups into main sessions and sort by start date *)
  let group_and_sort sessions =
    let parents, follow_ups =
      sessions
      |> CCList.partition_filter_map (fun (session : t) ->
           match session.follow_up_to with
           | None -> `Left (session.id, (session, []))
           | Some parent -> `Right (parent, session))
    in
    add_follow_ups_and_sort parents follow_ups
  ;;

  let group_and_sort_keep_followups sessions =
    let parents, follow_ups =
      CCList.fold_left
        (fun (parents, follow_ups) (s : t) ->
          let add_parent (s : t) = parents @ [ s.id, (s, []) ], follow_ups in
          match s.follow_up_to with
          | None -> add_parent s
          | Some id
            when CCOption.is_some
                   (CCList.find_opt
                      (fun (parent, _) -> Pool_common.Id.equal parent id)
                      parents) -> parents, follow_ups @ [ id, s ]
          | Some _ -> add_parent s)
        ([], [])
        sessions
    in
    add_follow_ups_and_sort parents follow_ups
  ;;

  let get_session_end (session : t) =
    Ptime.add_span session.start session.duration
    |> CCOption.get_exn_or "Session end not in range"
  ;;
end

let to_public
  ({ id
   ; follow_up_to
   ; start
   ; duration
   ; description
   ; location
   ; max_participants
   ; min_participants
   ; overbook
   ; assignment_count
   ; canceled_at
   ; _
   } :
    t)
  =
  Public.
    { id
    ; follow_up_to
    ; start
    ; duration
    ; description
    ; location
    ; max_participants
    ; min_participants
    ; overbook
    ; assignment_count
    ; canceled_at
    }
;;

let email_text language start duration location =
  let format label text =
    Format.asprintf
      "%s: %s"
      (Pool_common.(Utils.field_to_string language label)
       |> CCString.capitalize_ascii)
      text
  in
  let start =
    format
      Pool_common.Message.Field.Start
      (Start.value start |> Pool_common.Utils.Time.formatted_date_time)
  in
  let duration =
    format
      Pool_common.Message.Field.Duration
      (Duration.value duration |> Pool_common.Utils.Time.formatted_timespan)
  in
  let location =
    format
      Pool_common.Message.Field.Location
      (Pool_location.to_string language location)
  in
  CCString.concat "\n" [ start; duration; location ]
;;

let to_email_text language { start; duration; location; _ } =
  email_text language start duration location
;;

let public_to_email_text
  language
  (Public.{ start; duration; location; _ } : Public.t)
  =
  email_text language start duration location
;;

let get_session_end session =
  Ptime.add_span session.start session.duration
  |> CCOption.get_exn_or "Session end not in range"
;;

let not_canceled session =
  let open Pool_common.Message in
  match session.canceled_at with
  | None -> Ok ()
  | Some canceled_at ->
    canceled_at
    |> Pool_common.Utils.Time.formatted_date_time
    |> sessionalreadycanceled
    |> CCResult.fail
;;

let not_closed session =
  let open Pool_common.Message in
  match session.closed_at with
  | None -> Ok ()
  | Some closed_at ->
    closed_at
    |> Pool_common.Utils.Time.formatted_date_time
    |> sessionalreadyclosed
    |> CCResult.fail
;;

(* Cancellable if before session ends *)
let is_cancellable session =
  let open CCResult.Infix in
  let* () = not_canceled session in
  let* () = not_closed session in
  if Ptime.is_later
       (session |> get_session_end |> Start.value)
       ~than:Ptime_clock.(now ())
  then Ok ()
  else Error Pool_common.Message.SessionInPast
;;

let is_deletable session follow_ups =
  let open CCResult.Infix in
  let* () = not_canceled session in
  let* () = not_closed session in
  let has_follow_ups = CCList.is_empty follow_ups |> not in
  let has_assignments = session.assignment_count |> AssignmentCount.value > 0 in
  match has_follow_ups, has_assignments with
  | true, _ -> Error Pool_common.Message.SessionHasFollowUps
  | _, true -> Error Pool_common.Message.SessionHasAssignments
  | false, false -> Ok ()
;;

(* Closable if after session ends *)
let is_closable session =
  let open CCResult.Infix in
  let open Pool_common.Message in
  let* () = not_closed session in
  let* () = not_canceled session in
  if Ptime.is_earlier session.start ~than:Ptime_clock.(now ())
  then Ok ()
  else Error SessionNotStarted
;;

let assignments_cancelable session =
  let open CCResult.Infix in
  let* () = not_canceled session in
  let* () = not_closed session in
  Ok ()
;;
