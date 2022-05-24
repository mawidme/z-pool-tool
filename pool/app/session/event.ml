open Entity

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_text : Pool_common.Reminder.Text.t option
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

(* TODO [aerben] experiment ID *)
type event =
  | Created of (base * Pool_common.Id.t)
  | Canceled of t
  | Deleted of t
  | Updated of (base * t)
[@@deriving eq, show]

let handle_event pool = function
  | Created (session, experiment_id) ->
    let sess =
      create
        session.start
        session.duration
        session.description
        session.max_participants
        session.min_participants
        session.overbook
        session.reminder_text
        session.reminder_lead_time
    in
    Repo.insert pool (Pool_common.Id.value experiment_id, sess)
  | Canceled session ->
    Repo.update pool { session with canceled_at = Some (Ptime_clock.now ()) }
  | Deleted session -> Repo.delete pool session.id
  | Updated
      ( { start
        ; duration
        ; description
        ; max_participants
        ; min_participants
        ; overbook
        ; reminder_text
        ; reminder_lead_time
        }
      , session ) ->
    Repo.update
      pool
      { session with
        start
      ; duration
      ; description
      ; max_participants
      ; min_participants
      ; overbook
      ; reminder_text
      ; reminder_lead_time
      }
;;
