module Message = Pool_common.Message
module Utils = Pool_common.Utils

type t =
  { error : Message.error list
  ; warning : Message.warning list
  ; success : Message.success list
  ; info : Message.info list
  }
[@@deriving eq, show, yojson]

let empty = { error = []; warning = []; success = []; info = [] }
let set_success txts message = { message with success = txts }
let set_warning txts message = { message with warning = txts }
let set_error txts message = { message with error = txts }
let set_info txts message = { message with info = txts }

let get_error message lang =
  message.error |> CCList.map (Utils.error_to_string lang)
;;

let get_warning message lang =
  message.warning |> CCList.map (Utils.warning_to_string lang)
;;

let get_success message lang =
  message.success |> CCList.map (Utils.success_to_string lang)
;;

let get_info message lang =
  message.info |> CCList.map (Utils.info_to_string lang)
;;

let of_string str =
  let json =
    try Some (Yojson.Safe.from_string str) with
    | _ -> None
  in
  match json with
  | Some json -> Some (t_of_yojson json)
  | None -> None
;;

let to_string t = yojson_of_t t |> Yojson.Safe.to_string

let set ?(error = []) ?(warning = []) ?(success = []) ?(info = []) res =
  let message =
    empty
    |> set_error error
    |> set_warning warning
    |> set_success success
    |> set_info info
    |> to_string
  in
  (* We use alerts for all messages *)
  Sihl.Web.Flash.set_alert message res
;;