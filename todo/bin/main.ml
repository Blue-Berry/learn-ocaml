open Notty

let todo_list =
  match Data.get_todos () with
  | Result.Ok todo_list -> todo_list
  | Result.Error _ -> failwith "Error while getting todo list"
;;

open Data

let print_todos selected_index =
  let todos_with_index = List.mapi (fun i todo -> i, todo) todo_list.todos in
  let image =
    List.map
      (fun (i, todo) ->
        let checkbox =
          if i = selected_index
          then if todo.completed then "[x] " else "[ ] "
          else if todo.completed
          then "✓ "
          else "○ "
        in
        let attr = if i = selected_index then A.(fg yellow) else A.(fg red) in
        I.string attr (checkbox ^ todo.description))
      todos_with_index
    |> I.vcat
  in
  image
;;

open Common

let rec main t ((x, y) as pos) selected_index =
  let img =
    let dot = I.string A.(bg lightred ++ fg black) "✓" |> I.pad ~l:x ~t:y
    and txt = I.strf ~attr:A.(fg lightblack) "@(%d, %d)" x y in
    let todos_img = print_todos selected_index in
    let combined_img = I.(todos_img <-> txt </> dot) in
    combined_img
  in
  Term.image t img;
  Term.cursor t (Some pos);
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) -> ()
  | `Resize _ -> main t pos selected_index
  | `Mouse ((`Press _ | `Drag), pos, _) -> main t pos selected_index
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
      | `Left -> x - 1, y
      | `Right -> x + 1, y
    in
    main t new_pos new_index
  | `Key (`Enter, _) ->
    let todo = List.nth todo_list.todos selected_index in
    let updated_todo = { todo with completed = not todo.completed } in
    let updated_todos =
      List.mapi
        (fun i t -> if i = selected_index then updated_todo else t)
        todo_list.todos
    in
    let _ = { todo_list with todos = updated_todos } in
    main t pos selected_index
  | _ -> main t pos selected_index
;;

let () = main (Term.create ()) (0, 0) 0
