type creatable_admin = Event.creatable_admin =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator
  | Root

val equal_creatable_admin : creatable_admin -> creatable_admin -> bool
val pp_creatable_admin : Format.formatter -> creatable_admin -> unit
val show_creatable_admin : creatable_admin -> string

type create = Event.create =
  { email : Common_user.Email.Address.t
  ; password : Common_user.Password.t
  ; firstname : Common_user.Firstname.t
  ; lastname : Common_user.Lastname.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type update = Event.update =
  { firstname : Common_user.Firstname.t
  ; lastname : Common_user.Lastname.t
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type 'a person_event = 'a Event.person_event =
  | DetailsUpdated of 'a Entity.t * update
  | PasswordUpdated of
      'a Entity.t * Common_user.Password.t * Common_user.PasswordConfirmed.t
  | Disabled of 'a Entity.t
  | Verified of 'a Entity.t

type event = Event.event =
  | Created of creatable_admin * create
  | RootDisabled of Entity.root Entity.t
  | RootEnabled of Entity.root Entity.t
  | AssistantEvents of Entity.assistant person_event
  | ExperimenterEvents of Entity.experimenter person_event
  | LocationManagerEvents of Entity.location_manager person_event
  | RecruiterEvents of Entity.recruiter person_event
  | OperatorEvents of Entity.operator person_event

val handle_person_event : 'a person_event -> unit Lwt.t
val handle_event : event -> unit Lwt.t
val equal_person_event : 'a person_event -> 'a person_event -> bool
val pp_person_event : Format.formatter -> 'a person_event -> unit
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

type person = Entity.person =
  { user : Sihl_user.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal_person : person -> person -> bool
val pp_person : Format.formatter -> person -> unit
val show_person : person -> string

type assistant = Entity.assistant
type experimenter = Entity.experimenter
type location_manager = Entity.location_manager
type recruiter = Entity.recruiter
type operator = Entity.operator
type root = Entity.root

type 'a t = 'a Entity.t =
  | Assistant : person -> assistant t
  | Experimenter : person -> experimenter t
  | LocationManager : person -> location_manager t
  | Recruiter : person -> recruiter t
  | Operator : person -> operator t
  | Root : person -> root t

val equal : 'person t -> 'person t -> bool
val pp : Format.formatter -> 'person t -> unit

type any = Entity.any = Any : 'a t -> any

val equal_any : any -> any -> bool
val pp_any : Format.formatter -> any -> unit
val user : 'person_function t -> Sihl_user.t

module Duplicate = Admin__Entity.Duplicate

val login : 'a -> email:'b -> password:'c -> 'd
val find_by_user : 'a -> 'b
val find_duplicates : 'a -> 'b
val find_root : Pool_common.Id.t -> (root t, string) result Lwt.t
val find_all_root : unit -> (root t list, string) result Lwt.t
