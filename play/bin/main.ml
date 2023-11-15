open Notty
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

let filter_non_standard_chars str =
  let is_standard_char c =
    let ascii_val = int_of_char c in
    ascii_val >= 32 && ascii_val <= 126
  in
  let chars = List.of_seq (String.to_seq str) in
  let filtered_chars = List.filter is_standard_char chars in
  String.of_seq (List.to_seq filtered_chars)

let prompt_for_title_and_description () =
  print_endline "Enter the title for the new todo:";
  let title = filter_non_standard_chars @@ read_line () in
  print_endline "Enter the description for the new todo:";
  let description = filter_non_standard_chars @@ read_line () in
  (title, description)

let rec main t ((x, y) as pos) selected_index todo_list =
  let img =
    let todos_img = print_todos selected_index todo_list in
    let combined_img = todos_img in
    combined_img
  in
  Term.image t img;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) ->
      Data.store_todos todo_list
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
        | `Up -> (x, selected_y)
        | `Down -> (x, selected_y)
        | `Left -> (x, y)
        | `Right -> (x, y)
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
      let updated_todos =
        List.filteri (fun i _ -> i <> selected_index) todo_list.todos
      in
      let updated_todo_list = { todos = updated_todos } in
      main t pos selected_index updated_todo_list
      (* Create new todo item *)
  | `Key (`ASCII 'a', _) ->
      clear_screen t;
      let title, description = prompt_for_title_and_description () in
      let new_todo = { title; description; completed = false } in
      let updated_todos = new_todo :: todo_list.todos in
      let updated_todo_list = { todos = updated_todos } in
      main (Term.create ()) pos 0 updated_todo_list
      (* Shift item up or down *)
  | `Key (`ASCII 'k', _) ->
      if selected_index >= List.length todo_list.todos - 1 then
        main t pos selected_index todo_list
      else
        let updated_todos =
          change_todo_priority todo_list.todos selected_index 1
        in
        let selected_index = selected_index + 1 in
        main t pos selected_index { todos = updated_todos }
  | `Key (`ASCII 'j', _) ->
      if selected_index <= 0 then main t pos selected_index todo_list
      else
        let updated_todos =
          change_todo_priority todo_list.todos selected_index (-1)
        in
        let selected_index = selected_index - 1 in
        main t pos selected_index { todos = updated_todos }
  | _ -> main t pos selected_index todo_list

let () = main (Term.create ()) (0, 0) 0 todo_list
