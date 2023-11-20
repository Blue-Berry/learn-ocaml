open Common
open Notty

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let outline attr =
  let w, h = 83, 10 in
  let background = I.uchar A.(fg black) (Uchar.of_int 0x2007) w h in
  let chr x = I.uchar attr x 1 1
  and hbar = I.uchar attr (Uchar.of_int 0x2500) (w - 2) 1
  and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 (h - 2) in
  let a, b, c, d =
    ( chr @@ Uchar.of_int 0x256d
    , chr @@ Uchar.of_int 0x256e
    , chr @@ Uchar.of_int 0x256f
    , chr @@ Uchar.of_int 0x2570 )
  in
  I.(
    grid [ [ a; hbar; b ]; [ vbar; I.void (w - 2) 1; vbar ]; [ d; hbar; c ] ]
    </> background)
  |> I.pad ~t:1 ~l:1
;;

let img_of_string s width attr =
  (* Split new lines *)
  let rec loop s =
    if String.length s <= width
    then [ s ]
    else (
      let first = String.sub s 0 width in
      let rest = String.sub s width (String.length s - width) in
      first :: loop rest)
  in
  let lines = String.split_on_char '\n' s |> List.map (fun s -> loop s) |> List.flatten in
  (* let lines = loop s in *)
  let img = List.map (fun s -> I.string attr s) lines |> I.vcat in
  img
;;

(*[remove at index str] function used for removing a character at a cursor position for backspace *)
let remove_at index str =
  let len = String.length str in
  if index < 0 || index >= len
  then str
  else (
    let before = String.sub str 0 index in
    let after = String.sub str (index + 1) (len - index - 1) in
    before ^ after)
;;

(** [insert_at index c str] inserts the character [c] at the index [index] in the string [str].
    **)
let insert_at index c str =
  let len = String.length str in
  if index = len + 1
  then str ^ String.make 1 c
  else if index < 0 || index > len + 1
  then str
  else (
    let before = String.sub str 0 index in
    let after = String.sub str index (len - index) in
    before ^ String.make 1 c ^ after)
;;

(** [lines_of_string s] splits a string into lines split on the newline character
    and those lines are split into lines of length 80 **)
let lines_of_string s =
  let paragraphs = String.split_on_char '\n' s in
  let rec lines str =
    if String.length str <= 80
    then [ str ]
    else (
      let first = String.sub str 0 80 in
      let rest = String.sub str 80 (String.length str - 80) in
      first :: lines rest)
  in
  List.map lines paragraphs |> List.flatten
;;

(** [calc_cursor_pos text prompt text_index]
    calculates the position of the cursor given teh arguments
    it takes into count line numbers and wrapping **)
let calc_cursor_pos text prompt text_index =
  (* TODO: clean this up, this is a mess but it works *)
  let prompt_lines = lines_of_string prompt in
  if String.length text = 0
  then 1, 1
  else (
    let current_line =
      String.sub text 0 text_index |> lines_of_string |> List.rev |> List.hd
    in
    if String.get text (min text_index (String.length text - 1)) = '\n'
    then (
      let y = String.sub text 0 text_index |> lines_of_string |> List.length in
      1, y + List.length prompt_lines)
    else (
      let y = String.sub text 0 text_index |> lines_of_string |> List.length in
      let x = String.length current_line in
      x + 2, y + List.length prompt_lines - 1))
;;

(* Loop to manage input and update the image to the screen *)
let text_input_loop (state : Common.state) text prompt =
  (* function used for looping and updating the image *)
  let rec input_loop_aux (state : Common.state) text prompt text_index =
    let index_change delta =
      let text_index = text_index + delta in
      if text_index > String.length text
      then String.length text
      else if text_index < 0
      then 0
      else text_index
    in
    let prompt_img = img_of_string prompt 80 A.(fg magenta) in
    let text_img = img_of_string text 80 A.(fg lightblue) in
    let text_img = I.vcat [ prompt_img; I.(void 1 0 <|> text_img) ] |> I.pad ~t:1 ~l:2 in
    let () = Term.image state.t I.(text_img </> outline A.(fg lightred) </> state.img) in
    let cursor_pos = calc_cursor_pos text prompt (index_change 0) in
    Term.cursor state.t (Some (fst cursor_pos + 2, snd cursor_pos + 1));
    let t = state.t in
    match Term.event t with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) ->
      Term.cursor state.t None;
      None
    | `Resize _ -> input_loop_aux state text prompt text_index
    | `Key (`ASCII 'Y', [ `Ctrl ]) ->
      Term.cursor state.t None;
      Some text
    | `Key (`ASCII c, _) ->
      if c >= 'a' && c <= 'z' && text = ""
      then (
        let c = Char.uppercase_ascii c in
        input_loop_aux state (insert_at (text_index + 1) c text) prompt (index_change 1))
      else
        input_loop_aux state (insert_at (text_index + 1) c text) prompt (index_change 1)
    | `Key (`Backspace, _) ->
      let len = String.length text in
      if len = 0
      then input_loop_aux state text prompt text_index
      else input_loop_aux state (remove_at text_index text) prompt (index_change (-1))
    | `Key (`Enter, _) ->
      input_loop_aux state (insert_at (text_index + 1) '\n' text) prompt (index_change 1)
    | `Key (`Arrow d, _) ->
      let calc_updown_index direction =
        let lines = lines_of_string text in
        let pos_in_line, line_index = calc_cursor_pos text "" text_index in
        let pre_line_len =
          min (line_index - 1) (List.length lines - 1) |> List.nth lines |> String.length
        in
        let next_line_len =
          min (line_index + 1) (List.length lines - 1) |> List.nth lines |> String.length
        in
        let current_line_len = List.nth lines (line_index - 1) |> String.length in
        match direction with
        | `Up ->
          (* BUG: going up is supper buggy sometimes don't know what's going on but too tired to fix
             It's probs something to do with the \n character*)
          -pos_in_line - pre_line_len + min pre_line_len pos_in_line
        | `Down -> -pos_in_line + current_line_len + 1 + min next_line_len pos_in_line
      in
      input_loop_aux state text prompt
      @@
        (match d with
        | `Left -> index_change (-1)
        | `Right -> index_change 1
        | `Up -> index_change (calc_updown_index `Up)
        | `Down -> index_change (calc_updown_index `Down))
    | _ -> input_loop_aux state text prompt text_index
  in
  input_loop_aux state text prompt 0
;;

let prompt_for_title_and_description (state : Common.state) title description =
  let title = text_input_loop state title "Enter the title for the new todo:" in
  match title with
  | None -> None
  | Some _ ->
    let description =
      text_input_loop state description "Enter the description for the new todo:"
    in
    (match title, description with
     | Some title, Some description -> Some (title, description)
     | _ -> None)
;;

let prompt_for_title t title = text_input_loop t title "Enter the title:"
