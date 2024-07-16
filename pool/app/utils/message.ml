let html_to_plain_text html_string =
  let tokens =
    Markup.string html_string |> Markup.parse_html |> Markup.signals
  in
  (* TODO: Add line breaks div, br, *)
  let rec process_tokens acc = function
    | [] -> acc
    | `Comment _ :: rest -> process_tokens acc rest
    | `Doctype _ :: rest -> process_tokens acc rest
    | `End_element :: rest -> process_tokens acc rest
    | `PI (_, _) :: rest -> process_tokens acc rest
    | `Start_element (_, _) :: rest -> process_tokens acc rest
    | `Text text :: rest ->
      let text = text |> CCString.concat "" in
      process_tokens (acc ^ text) rest
    | `Xml _ :: rest -> process_tokens acc rest
    | _ :: rest -> process_tokens acc rest
  in
  process_tokens
    ""
    (Markup.fold (fun acc signal -> signal :: acc) [] tokens |> List.rev)
;;

(* TODO: Add tests cases *)
let is_markup str =
  let open Str in
  let tag_pattern = regexp "<[^>]+>[^<]*</[^>]+>" in
  let rec check_segments pos =
    if pos >= String.length str
    then true
    else if string_match tag_pattern str pos
    then (
      let matched_end = match_end () in
      check_segments matched_end)
    else false
  in
  check_segments 0
;;

let render_params ?cb ?(to_plain = false) data text =
  let () = Logs.info (fun m -> m "render_params") in
  let replace str k v =
    let regexp = Str.regexp @@ "{" ^ k ^ "}" in
    let is_markup = is_markup v in
    Str.global_replace
      regexp
      v
      (if is_markup && to_plain then html_to_plain_text str else str)
  in
  let rec render data value =
    match data with
    | [] -> value
    | (k, v) :: data ->
      (match cb with
       | None -> v
       | Some cb -> v |> cb)
      |> fun v -> render data @@ replace value k v
  in
  render data text
;;
