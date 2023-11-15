open Notty

let todo_list = Data.get_todos ()

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

let img_of_string s width colour =
  let rec loop s =
    if String.length s <= width
    then [ s ]
    else (
      let first = String.sub s 0 width in
      let rest = String.sub s width (String.length s - width) in
      first :: loop rest)
  in
  let lines = loop s in
  let img = List.map (fun s -> I.string A.(fg colour) s) lines |> I.vcat in
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
        let attr = if i = selected_index then A.(fg red) else A.(fg blue) in
        let todo_str = I.string attr (checkbox ^ todo.title) in
        if i = selected_index
        then (
          (* TODO: text wrap *)
          let desc_str = I.string A.(fg lightmagenta) todo.description in
          I.vcat [ todo_str; I.(void 4 0 <|> desc_str) ])
        else todo_str)
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

let rec text_input_loop t text prompt =
  (* TODO: text wrap *)
  let prompt_img = I.string A.(fg magenta) prompt in
  let cursor_img = I.string A.(fg lightred) "|" in
  (* let text_img = I.string A.(fg lightblue) text in *)
  let text_img = img_of_string text 80 lightblue in
  let text_img = I.(text_img <|> void 0 0 <|> cursor_img) in
  I.vcat [ prompt_img; I.(void 1 0 <|> text_img) ] |> Term.image t;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> None
  | `Resize _ -> text_input_loop t text prompt
  | `Key (`ASCII c, _) -> text_input_loop t (text ^ String.make 1 c) prompt
  | `Key (`Backspace, _) ->
    let len = String.length text in
    if len = 0
    then text_input_loop t text prompt
    else text_input_loop t (String.sub text 0 (len - 1)) prompt
  | `Key (`Enter, _) -> Some text
  | _ -> text_input_loop t text prompt
;;

let prompt_for_title_and_description () =
  let title = text_input_loop (Term.create ()) "" "Enter the title for the new todo:" in
  match title with
  | None -> None
  | Some _ ->
    let description =
      text_input_loop (Term.create ()) "" "Enter the description for the new todo:"
    in
    (match title, description with
     | Some title, Some description -> Some (title, description)
     | _ -> None)
;;

let rec main t ((x, y) as pos) selected_index todo_list =
  let img =
    let todos_img = print_todos selected_index todo_list in
    let combined_img = todos_img in
    combined_img
  in
  Term.image t img;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> Data.store_todos todo_list
  | `Resize _ -> main t pos selected_index todo_list
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
    main t new_pos new_index todo_list
  | `Key (`Enter, _) ->
    let todo = List.nth todo_list.todos selected_index in
    let updated_todo = { todo with completed = not todo.completed } in
    let updated_todos =
      List.mapi
        (fun i t -> if i = selected_index then updated_todo else t)
        todo_list.todos
    in
    let updated_todo_list = { todos = updated_todos } in
    main t pos selected_index updated_todo_list
    (* Delete selected item *)
  | `Key (`ASCII 'D', [ `Ctrl ]) ->
    let updated_todos = List.filteri (fun i _ -> i <> selected_index) todo_list.todos in
    let updated_todo_list = { todos = updated_todos } in
    main t pos selected_index updated_todo_list
    (* Create new todo item *)
  | `Key (`ASCII 'a', _) ->
    clear_screen t;
    (match prompt_for_title_and_description () with
     | None -> main t pos selected_index todo_list
     | Some (title, description) ->
       let new_todo = { title; description; completed = false } in
       let updated_todos = new_todo :: todo_list.todos in
       let updated_todo_list = { todos = updated_todos } in
       main (Term.create ()) pos 0 updated_todo_list)
    (* Shift item up or down *)
  | `Key (`ASCII 'k', _) ->
    if selected_index >= List.length todo_list.todos - 1
    then main t pos selected_index todo_list
    else (
      let updated_todos = change_todo_priority todo_list.todos selected_index 1 in
      let selected_index = selected_index + 1 in
      main t pos selected_index { todos = updated_todos })
  | `Key (`ASCII 'j', _) ->
    if selected_index <= 0
    then main t pos selected_index todo_list
    else (
      let updated_todos = change_todo_priority todo_list.todos selected_index (-1) in
      let selected_index = selected_index - 1 in
      main t pos selected_index { todos = updated_todos })
  | _ -> main t pos selected_index todo_list
;;

let () = main (Term.create ()) (0, 0) 0 todo_list
