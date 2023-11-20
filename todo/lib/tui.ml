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
  match Input.prompt_for_title folder.name with
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
          match Input.prompt_for_title_and_description t.title t.description with
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
       (match Input.prompt_for_title_and_description "" "" with
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
       (match Input.prompt_for_title "" with
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
    (match Input.prompt_for_title_and_description "" "" with
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
