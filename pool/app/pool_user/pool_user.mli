module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t
  val to_sihl : t -> string

  val schema
    :  ?field:Pool_common.Message.Field.t
    -> unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Password : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val to_sihl : t -> string

  val schema
    :  ?field:Pool_common.Message.Field.t
    -> unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t

  val validate
    :  ?password_policy:(t -> (unit, Pool_common.Message.error) result)
    -> t
    -> (unit, Pool_common.Message.error) result

  val validate_current_password
    :  ?field:Pool_common.Message.Field.t
    -> Sihl_user.t
    -> t
    -> (unit, Pool_common.Message.error) result

  val validate_password_confirmation
    :  t
    -> PasswordConfirmed.t
    -> (unit, Pool_common.Message.error) result
end

module Firstname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t
  val value : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Lastname : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t
  val value : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Paused : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
end

module TermsAccepted : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t option -> t
  val create_now : unit -> t
  val value : t -> Ptime.t option
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t option -> t
  val create_now : unit -> t
  val value : t -> Ptime.t option
end

module EmailAddress : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_common.Message.error) result

  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val of_string : string -> t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module EmailVerified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t option -> t
  val create_now : unit -> t
  val value : t -> Ptime.t option
  val is_some : t -> bool
end

module Repo : sig
  module Paused : sig
    val t : bool Caqti_type.t
  end

  module Disabled : sig
    val t : bool Caqti_type.t
  end

  module TermsAccepted : sig
    val t : Ptime.t option Caqti_type.t
  end

  module Verified : sig
    val t : Ptime.t option Caqti_type.t
  end

  module EmailVerified : sig
    val t : Ptime.t option Caqti_type.t
  end

  module EmailAddress : sig
    val t : string Caqti_type.t
  end

  val user_caqti : Sihl_user.t Caqti_type.t
end
