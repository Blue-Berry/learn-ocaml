open Notty

let change_todo_priority todo_list n delta =
  let todo = List.nth todo_list n in
  let prev_todo = List.nth todo_list (n + delta) in
  let updated_todos =
    List.mapi
      (fun i t -> if i = n then prev_todo else if i = n + delta then todo else t)
      todo_list
  in
  updated_todos
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

open Data

let print_todos selected_index todo_list =
  let todos_with_index = List.mapi (fun i todo -> i, todo) todo_list.todos in
  let image =
    List.map
      (fun (i, todo) ->
        let checkbox =
          if i = selected_index
          then if todo.completed then " ✓ " else " ○ "
          else if todo.completed
          then "✓ "
          else "○ "
        in
        let attr =
          if todo.completed
          then A.fg A.lightgreen
          else if i = selected_index
          then A.(fg red)
          else A.(fg blue)
        in
        let todo_img = img_of_string (checkbox ^ todo.title) 80 attr in
        if i = selected_index
        then (
          let desc_img = img_of_string todo.description 80 A.(fg lightmagenta) in
          I.vcat [ todo_img; I.(void 4 0 <|> desc_img) ])
        else todo_img)
      todos_with_index
    |> I.vcat
  in
  image
;;

open Common

let clear_screen t =
  let reset_terminal () =
    let t = Unix.tcgetattr Unix.stdin in
    let t' = { t with Unix.c_echo = true; Unix.c_icanon = true } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW t'
  in
  reset_terminal ();
  Sys.command "clear" |> ignore;
  let img = I.empty in
  Term.image t img
;;

(* Loop to manage input and update the image to the screen *)
let text_input_loop t text prompt =
  (* function used for removing a character at a cursor position *)
  let remove_at index str =
    let len = String.length str in
    if index < 0 || index >= len
    then str
    else (
      let before = String.sub str 0 index in
      let after = String.sub str (index + 1) (len - index - 1) in
      before ^ after)
  in
  (* function used for inserting a character at a cursor position *)
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
  in
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
  in
  (* function used for calculating the cursor position based off the text field *)
  (* TODO: clean this up, this is a mess but it works *)
  let calc_cursor_pos text prompt text_index =
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
  in
  (* function used for looping and updating the image *)
  let rec input_loop_aux t text prompt text_index =
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
    I.vcat [ prompt_img; I.(void 1 0 <|> text_img) ] |> Term.image t;
    Term.cursor t (Some (calc_cursor_pos text prompt (index_change 0)));
    match Term.event t with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> None
    | `Resize _ -> input_loop_aux t text prompt text_index
    | `Key (`ASCII 'Y', [ `Ctrl ]) -> Some text
    | `Key (`ASCII c, _) ->
      if c >= 'a' && c <= 'z' && text = ""
      then (
        let c = Char.uppercase_ascii c in
        input_loop_aux t (insert_at (text_index + 1) c text) prompt (index_change 1))
      else input_loop_aux t (insert_at (text_index + 1) c text) prompt (index_change 1)
    | `Key (`Backspace, _) ->
      let len = String.length text in
      if len = 0
      then input_loop_aux t text prompt text_index
      else input_loop_aux t (remove_at text_index text) prompt (index_change (-1))
    | `Key (`Enter, _) ->
      input_loop_aux t (insert_at (text_index + 1) '\n' text) prompt (index_change 1)
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
      input_loop_aux t text prompt
      @@
        (match d with
        | `Left -> index_change (-1)
        | `Right -> index_change 1
        | `Up -> index_change (calc_updown_index `Up)
        | `Down -> index_change (calc_updown_index `Down))
    | _ -> input_loop_aux t text prompt text_index
  in
  input_loop_aux t text prompt 0
;;

let prompt_for_title_and_description title description =
  let title =
    text_input_loop (Term.create ()) title "Enter the title for the new todo:"
  in
  match title with
  | None -> None
  | Some _ ->
    let description =
      text_input_loop
        (Term.create ())
        description
        "Enter the description for the new todo:"
    in
    (match title, description with
     | Some title, Some description -> Some (title, description)
     | _ -> None)
;;

let rec main_tui_loop t ((x, y) as pos) selected_index todo_list =
  let img =
    let todos_img = print_todos selected_index todo_list in
    let combined_img = todos_img in
    combined_img
  in
  Term.image t img;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) ->
    Data.store_todos todo_list
  | `Resize _ -> main_tui_loop t pos selected_index todo_list
  | `Key (`Arrow d, _) ->
    let max_index = List.length todo_list.todos - 1 in
    let new_index =
      match d with
      | `Up -> max 0 (selected_index - 1)
      | `Down -> min max_index (selected_index + 1)
      | _ -> selected_index
    in
    let new_pos =
      let selected_y = y + new_index - selected_index in
      match d with
      | `Up -> x, selected_y
      | `Down -> x, selected_y
      | `Left -> x, y
      | `Right -> x, y
    in
    main_tui_loop t new_pos new_index todo_list
  | `Key (`Enter, _) ->
    let todo = List.nth todo_list.todos selected_index in
    let updated_todo = { todo with completed = not todo.completed } in
    let updated_todos =
      List.mapi
        (fun i t -> if i = selected_index then updated_todo else t)
        todo_list.todos
    in
    let updated_todo_list = { todos = updated_todos } in
    main_tui_loop t pos selected_index updated_todo_list
    (* Delete selected item *)
  | `Key (`ASCII 'D', [ `Ctrl ]) ->
    let updated_todos = List.filteri (fun i _ -> i <> selected_index) todo_list.todos in
    let updated_todo_list = { todos = updated_todos } in
    main_tui_loop t pos selected_index updated_todo_list
    (* Create new todo item *)
  | `Key (`ASCII 'a', _) ->
    clear_screen t;
    (match prompt_for_title_and_description "" "" with
     | None -> main_tui_loop t pos selected_index todo_list
     | Some (title, description) ->
       let new_todo = { title; description; completed = false } in
       let updated_todos = new_todo :: todo_list.todos in
       let updated_todo_list = { todos = updated_todos } in
       Data.store_todos updated_todo_list;
       main_tui_loop (Term.create ()) pos 0 updated_todo_list)
    (* Shift item up or down *)
  | `Key (`ASCII 'j', _) ->
    if selected_index >= List.length todo_list.todos - 1
    then main_tui_loop t pos selected_index todo_list
    else (
      let updated_todos = change_todo_priority todo_list.todos selected_index 1 in
      let selected_index = selected_index + 1 in
      Data.store_todos { todos = updated_todos };
      main_tui_loop t pos selected_index { todos = updated_todos })
  | `Key (`ASCII 'k', _) ->
    if selected_index <= 0
    then main_tui_loop t pos selected_index todo_list
    else (
      let updated_todos = change_todo_priority todo_list.todos selected_index (-1) in
      let selected_index = selected_index - 1 in
      Data.store_todos { todos = updated_todos };
      main_tui_loop t pos selected_index { todos = updated_todos })
  | `Key (`ASCII 'e', _) ->
    clear_screen t;
    let todo = List.nth todo_list.todos selected_index in
    (match prompt_for_title_and_description todo.title todo.description with
     | None -> main_tui_loop t pos selected_index todo_list
     | Some (title, description) ->
       let new_todo = { todo with title; description } in
       let updated_todos =
         List.mapi (fun i t -> if i = selected_index then new_todo else t) todo_list.todos
       in
       let updated_todo_list = { todos = updated_todos } in
       Data.store_todos updated_todo_list;
       main_tui_loop (Term.create ()) pos 0 updated_todo_list)
  | _ -> main_tui_loop t pos selected_index todo_list
;;
