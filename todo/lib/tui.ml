open Notty

type display_list =
  | Todo of (Data.todo * int)
  | Folder of (Data.folder * int)

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

(* function used for removing a character at a cursor position *)
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
let text_input_loop t text prompt =
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

let print_todos selected_index (todos : Data.todo list) =
  let todos_with_index = List.mapi (fun i todo -> i, todo) todos in
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

let display_list_of_folders folders =
  let rec aux folders acc indent =
    match folders with
    | [] -> acc
    | folder :: rest ->
      if folder.is_open = false
      then aux rest (acc @ [ Folder (folder, indent) ]) indent
      else
        aux
          rest
          (acc
           @ [ Folder (folder, indent) ]
           @ aux folder.folders [] (indent + 1)
           @ List.map (fun todo -> Todo (todo, indent + 1)) folder.todos)
          indent
  in
  aux folders [] 0
;;

(* BUG: todo's not working properly probs a n-1 somewhere*)
let toggle_display_list_nth n folders =
  let rec aux folders acc n =
    match folders, n with
    | [], _ ->
      let () = Printf.printf "Empty array \n" in
      List.rev acc, n
    | folder :: rest, 0 ->
      let () = Printf.printf "folder n=0 \n" in
      List.rev acc @ [ { folder with is_open = not folder.is_open } ] @ rest, 0
    | ({ is_open = false; _ } as folder) :: rest, n ->
      let () = Printf.printf "folder closed \n" in
      aux rest (folder :: acc) (n - 1)
    | ({ folders = []; _ } as folder) :: rest, n when n <= List.length folder.todos ->
      let () =
        Printf.printf
          "Empty sub folders and n, lst %d < %d\n"
          n
          (List.length folder.todos)
      in
      let new_todos =
        List.mapi
          (fun i t -> if i = n - 1 then { t with completed = not t.completed } else t)
          folder.todos
      in
      List.rev acc @ [ { folder with todos = new_todos } ] @ rest, 0
    | ({ folders = []; _ } as folder) :: rest, n ->
      let () =
        Printf.printf
          "Empty sub folders and n, lst %d >= %d\n"
          n
          (List.length folder.todos)
      in
      aux rest (folder :: acc) (n - List.length folder.todos)
    | ({ is_open = true; folders = sub_folders; _ } as folder) :: rest, n ->
      let () = Printf.printf "folder open with n: %d \n" n in
      let new_sub_folders, n = aux sub_folders [] (n - 1) in
      if n = 0
      then List.rev acc @ [ { folder with folders = new_sub_folders } ] @ rest, 0
      else if n < List.length folder.todos
      then (
        let new_todos =
          List.mapi
            (fun i t -> if i = n - 1 then { t with completed = not t.completed } else t)
            folder.todos
        in
        List.rev acc @ [ { folder with todos = new_todos } ] @ rest, 0)
      else aux rest (folder :: acc) (n - List.length folder.todos)
  in
  let folders, _ = aux folders [] n in
  folders
;;

let img_of_display_list list selected_index =
  let rec aux list selected_index acc n =
    match list with
    | [] -> acc
    | Todo (todo, indent) :: rest ->
      let checkbox = if todo.completed then " ✓ " else " ○ " in
      let attr =
        if todo.completed
        then A.fg A.lightgreen
        else if n = selected_index
        then A.(fg red)
        else A.(fg blue)
      in
      let string_img = img_of_string (checkbox ^ todo.title) 80 attr in
      let img = I.hpad indent 0 string_img in
      aux rest selected_index (I.vcat [ acc; img ]) (n + 1)
    | Folder (folder, indent) :: rest ->
      (* TODO: update this character *)
      let prefix = if folder.is_open then " + " else " - " in
      let attr =
        if n = selected_index
        then A.(fg red)
        else if folder.is_open
        then A.(fg lightmagenta)
        else A.(fg lightblue)
      in
      let string_img = img_of_string (prefix ^ folder.name) 80 attr in
      let img = I.hpad indent 0 string_img in
      aux rest selected_index (I.vcat [ acc; img ]) (n + 1)
  in
  aux list selected_index I.empty 0
;;

let rec main_tui_loop t ((x, y) as pos) selected_index (folders : Data.folders) =
  let img = img_of_display_list (display_list_of_folders folders) selected_index in
  Term.image t img;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) ->
    Data.store_todos folders
  | `Resize _ -> main_tui_loop t pos selected_index folders
  | `Key (`Arrow d, _) ->
    (* let max_index = List.length folders.todos - 1 in *)
    (* let max_index = *)
    (*   List.fold_left *)
    (*     (fun acc f -> acc + if f.is_open = false then 1 else 1 + List.length f.todos) *)
    (*     0 *)
    (*     folders *)
    let max_index = List.length (display_list_of_folders folders) - 1 in
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
    main_tui_loop t new_pos new_index folders
  | `Key (`Enter, _) ->
    main_tui_loop t pos selected_index (toggle_display_list_nth selected_index folders)
  (*TODO: Delete selected item *)
  (* | `Key (`ASCII 'D', [ `Ctrl ]) -> *)
  (*   let updated_todos = List.filteri (fun i _ -> i <> selected_index) folders.todos in *)
  (*   let updated_todo_list = { todos = updated_todos } in *)
  (*   main_tui_loop t pos selected_index updated_todo_list *)
  (*TODO: Create new todo item *)
  (* | `Key (`ASCII 'a', _) -> *)
  (*   clear_screen t; *)
  (*   (match prompt_for_title_and_description "" "" with *)
  (*    | None -> main_tui_loop t pos selected_index folders *)
  (*    | Some (title, description) -> *)
  (*      let new_todo = { title; description; completed = false } in *)
  (*      let updated_todos = new_todo :: folders.todos in *)
  (*      let updated_todo_list = { todos = updated_todos } in *)
  (*      Data.store_todos updated_todo_list; *)
  (*      main_tui_loop (Term.create ()) pos 0 updated_todo_list) *)
  (*   (* Shift item up or down *) *)
  (* | `Key (`ASCII 'j', _) -> *)
  (*   if selected_index >= List.length folders.todos - 1 *)
  (*   then main_tui_loop t pos selected_index folders *)
  (*   else ( *)
  (*     let updated_todos = change_todo_priority folders.todos selected_index 1 in *)
  (*     let selected_index = selected_index + 1 in *)
  (*     Data.store_todos { todos = updated_todos }; *)
  (*     main_tui_loop t pos selected_index { todos = updated_todos }) *)
  (* | `Key (`ASCII 'k', _) -> *)
  (*   if selected_index <= 0 *)
  (*   then main_tui_loop t pos selected_index folders *)
  (*   else ( *)
  (*     let updated_todos = change_todo_priority folders.todos selected_index (-1) in *)
  (*     let selected_index = selected_index - 1 in *)
  (*     Data.store_todos { todos = updated_todos }; *)
  (*     main_tui_loop t pos selected_index { todos = updated_todos }) *)
  (* | `Key (`ASCII 'e', _) -> *)
  (*   clear_screen t; *)
  (*   let todo = List.nth folders.todos selected_index in *)
  (*   (match prompt_for_title_and_description todo.title todo.description with *)
  (*    | None -> main_tui_loop t pos selected_index folders *)
  (*    | Some (title, description) -> *)
  (*      let new_todo = { todo with title; description } in *)
  (*      let updated_todos = *)
  (*        List.mapi (fun i t -> if i = selected_index then new_todo else t) folders.todos *)
  (*      in *)
  (*      let updated_todo_list = { todos = updated_todos } in *)
  (*      Data.store_todos updated_todo_list; *)
  (*      main_tui_loop (Term.create ()) pos 0 updated_todo_list) *)
  | _ -> main_tui_loop t pos selected_index folders
;;
