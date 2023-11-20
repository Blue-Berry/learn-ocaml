open Notty

type display_list =
  | Todo of (Data.todo * int)
  | Folder of (Data.folder * int)

(* unused *)
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

(* [clear_screen term] Clears the previous state of the screen to allow for the prompt probably remove once a popup prompt is made *)
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

let prompt_for_title title = text_input_loop (Term.create ()) title "Enter the title:"

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

(** [toggle_display_list_nth n folders folder_map todo_map] **)
let map_display_list_nth n folders folder_map todo_map =
  let rec aux folders acc n =
    match folders, n with
    | [], _ ->
      (* NOTE: rev acc? *)
      acc, n
    | folder :: rest, 0 -> List.rev acc @ folder_map folder @ rest, 0
    | ({ is_open = false; _ } as folder) :: [], n -> List.rev acc @ folder_map folder, n
    | ({ is_open = false; _ } as folder) :: rest, n -> aux rest (folder :: acc) (n - 1)
    | ({ folders = []; _ } as folder) :: rest, n when n <= List.length folder.todos ->
      let new_todos = todo_map folder.todos n in
      List.rev acc @ [ { folder with todos = new_todos } ] @ rest, 0
    | ({ folders = []; _ } as folder) :: rest, n when n >= List.length folder.todos ->
      aux rest (folder :: acc) (n - List.length folder.todos - 1)
    | ({ is_open = true; folders = sub_folders; _ } as folder) :: rest, n ->
      let new_sub_folders, n = aux sub_folders [] (n - 1) in
      if n = 0
      then List.rev acc @ [ { folder with folders = new_sub_folders } ] @ rest, 0
      else if n < List.length folder.todos + 1
      then (
        let new_todos = todo_map folder.todos n in
        List.rev acc @ [ { folder with todos = new_todos } ] @ rest, 0)
      else aux rest (folder :: acc) (n - List.length folder.todos - 1)
  in
  let folders, _ = aux folders [] n in
  folders
;;

let toggle_folder_status folder = [ { folder with is_open = not folder.is_open } ]

let toggle_todo_status todos n =
  let new_todos =
    List.mapi
      (fun i t -> if i = n - 1 then { t with completed = not t.completed } else t)
      todos
  in
  new_todos
;;

let delete_todo todos n =
  let new_todos = List.filteri (fun i _ -> i <> n - 1) todos in
  new_todos
;;

let delete_folder _ = []

let add_new_todo title description todos _ =
  let new_todo = { title; description; completed = false } in
  new_todo :: todos
;;

let add_new_todo_in_folder title description folder =
  let new_todo = { title; description; completed = false } in
  [ { folder with todos = new_todo :: folder.todos } ]
;;

let add_new_folder name folder =
  let new_folder = { name; is_open = false; todos = []; folders = [] } in
  [ new_folder; folder ]
;;

let edit_folder_title folder =
  match prompt_for_title folder.name with
  | None -> [ folder ]
  | Some name ->
    let new_folder = { folder with name } in
    [ new_folder ]
;;

let edit_todo todos n =
  let new_todos =
    List.mapi
      (fun i t ->
        if i = n - 1
        then (
          match prompt_for_title_and_description t.title t.description with
          | None -> t
          | Some (title, description) -> { t with title; description })
        else t)
      todos
  in
  new_todos
;;

(* [move_todo delta todos n] *)
let move_todo delta todos n =
  let n = n - 1 in
  let n' = n + delta in
  if n' < 0 || n' >= List.length todos
  then todos
  else (
    let todo = List.nth todos n in
    let prev_todo = List.nth todos n' in
    let updated_todos =
      List.mapi (fun i t -> if i = n then prev_todo else if i = n' then todo else t) todos
    in
    updated_todos)
;;

let img_of_display_list list selected_index =
  let rec aux list selected_index acc n =
    match list with
    | [] -> acc
    | Todo (todo, indent) :: rest ->
      let checkbox =
        match todo.completed, n with
        | true, _ when n = selected_index -> " ✓ "
        | true, _ -> " ✓ "
        | false, _ when n = selected_index -> " ○ "
        | false, _ -> " ○ "
      in
      let attr =
        match todo.completed, n with
        | true, _ when n = selected_index -> A.fg A.cyan
        | true, _ -> A.fg A.lightgreen
        | false, _ when n = selected_index -> A.fg A.yellow
        | false, _ -> A.fg A.blue
      in
      let title_img = img_of_string (checkbox ^ todo.title) 80 attr in
      let desc_img =
        match selected_index with
        | _ when n = selected_index -> img_of_string todo.description 80 attr
        | _ -> I.empty
      in
      let todo_img = I.vcat [ title_img; I.(void 4 0 <|> desc_img) ] in
      let img = I.hpad indent 0 todo_img in
      aux rest selected_index (I.vcat [ acc; img ]) (n + 1)
    | Folder (folder, indent) :: rest ->
      let prefix =
        match folder.is_open, n with
        | true, _ when n = selected_index -> "  "
        | true, _ -> "  "
        | false, _ when n = selected_index -> "  "
        | false, _ -> "  "
      in
      let attr = if n = selected_index then A.(fg yellow) else A.(fg red) in
      let string_img = img_of_string (prefix ^ folder.name) 80 attr in
      let img = I.hpad indent 0 string_img in
      aux rest selected_index (I.vcat [ acc; img ]) (n + 1)
  in
  aux list selected_index I.empty 0
;;

(* TODO: move state variables into a record *)
let rec main_tui_loop t ((x, y) as pos) selected_index (folders : Data.folders) =
  let img = img_of_display_list (display_list_of_folders folders) selected_index in
  Term.image t img;
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) ->
    Data.store_todos folders
  | `Resize _ -> main_tui_loop t pos selected_index folders
  | `Key (`Arrow d, _) ->
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
    let () = Data.store_todos folders in
    main_tui_loop
      t
      pos
      selected_index
      (map_display_list_nth
         selected_index
         folders
         toggle_folder_status
         toggle_todo_status)
  | `Key (`ASCII 'D', [ `Ctrl ]) ->
    let () = Data.store_todos folders in
    main_tui_loop
      t
      pos
      selected_index
      (map_display_list_nth selected_index folders delete_folder delete_todo)
  (* add new folder *)
  | `Key (`ASCII 'n', _) ->
    clear_screen t;
    let item = List.nth (display_list_of_folders folders) selected_index in
    (match item with
     | Todo _ ->
       (match prompt_for_title_and_description "" "" with
        | None -> main_tui_loop t pos selected_index folders
        | Some (title, description) ->
          let () = Data.store_todos folders in
          main_tui_loop
            t
            pos
            selected_index
            (map_display_list_nth
               selected_index
               folders
               (add_new_folder title)
               (add_new_todo title description)))
     | Folder _ ->
       (match prompt_for_title "" with
        | None -> main_tui_loop t pos selected_index folders
        | Some title ->
          let folders =
            map_display_list_nth
              selected_index
              folders
              (add_new_folder title)
              (fun todo _ -> todo)
          in
          let () = Data.store_todos folders in
          main_tui_loop t pos selected_index folders))
  | `Key (`ASCII 'a', _) ->
    clear_screen t;
    (* TODO: check if selected item is a folder or todo *)
    (match prompt_for_title_and_description "" "" with
     | None -> main_tui_loop t pos selected_index folders
     | Some (title, description) ->
       let folders =
         map_display_list_nth
           selected_index
           folders
           (add_new_todo_in_folder title description)
           (add_new_todo title description)
       in
       let () = Data.store_todos folders in
       main_tui_loop t pos selected_index folders)
  (* Shift item up or down *)
  | `Key (`ASCII 'j', _) ->
    if selected_index >= List.length (display_list_of_folders folders) - 1
    then main_tui_loop t pos selected_index folders
    else (
      match
        ( List.nth (display_list_of_folders folders) selected_index
        , List.nth (display_list_of_folders folders) (selected_index + 1) )
      with
      | Folder _, _ | Todo _, Folder _ -> main_tui_loop t pos selected_index folders
      | Todo _, Todo _ ->
        let folders =
          map_display_list_nth selected_index folders (fun t -> [ t ]) (move_todo 1)
        in
        let selected_index = selected_index + 1 in
        Data.store_todos folders;
        main_tui_loop t pos selected_index folders)
  | `Key (`ASCII 'k', _) ->
    if selected_index <= 0
    then main_tui_loop t pos selected_index folders
    else (
      match
        ( List.nth (display_list_of_folders folders) selected_index
        , List.nth (display_list_of_folders folders) (selected_index - 1) )
      with
      | Folder _, _ | Todo _, Folder _ -> main_tui_loop t pos selected_index folders
      | Todo _, Todo _ ->
        let folders =
          map_display_list_nth selected_index folders (fun t -> [ t ]) (move_todo (-1))
        in
        let selected_index = selected_index - 1 in
        Data.store_todos folders;
        main_tui_loop t pos selected_index folders)
  | `Key (`ASCII 'e', _) ->
    clear_screen t;
    let item = List.nth (display_list_of_folders folders) selected_index in
    (match item with
     | Todo _ ->
       let folders =
         map_display_list_nth selected_index folders (fun t -> [ t ]) edit_todo
       in
       let () = Data.store_todos folders in
       main_tui_loop t pos selected_index folders
     | Folder _ ->
       let folders =
         map_display_list_nth selected_index folders edit_folder_title (fun todo _ ->
           todo)
       in
       let () = Data.store_todos folders in
       main_tui_loop t pos selected_index folders)
  | _ -> main_tui_loop t pos selected_index folders
;;
