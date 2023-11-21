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

(* TODO: set up default function for folder_map and todo_map *)
(** [toggle_display_list_nth n folders folder_map todo_map] **)
let map_display_list_nth n folders folder_map todo_map =
  let rec aux folders acc n =
    match folders, n with
    | [], _ -> acc, n
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

let add_new_todo state todos _ =
  let input = Input.prompt_for_title_and_description state "" "" in
  match input with
  | None -> todos
  | Some (title, description) ->
    let new_todo = { title; description; completed = false } in
    new_todo :: todos
;;

let add_new_todo_in_folder state folder =
  let input = Input.prompt_for_title_and_description state "" "" in
  match input with
  | None -> [ folder ]
  | Some (title, description) ->
    let new_todo = { title; description; completed = false } in
    [ { folder with todos = new_todo :: folder.todos } ]
;;

let add_new_folder state folder =
  match Input.prompt_for_title state "" with
  | None -> [ folder ]
  | Some name ->
    let new_folder = { name; is_open = false; todos = []; folders = [] } in
    [ new_folder; folder ]
;;

let add_sub_folder state folder : Data.folder list =
  match Input.prompt_for_title state "" with
  | None -> [ folder ]
  | Some name ->
    let new_folder : Data.folder = { name; is_open = false; todos = []; folders = [] } in
    [ { folder with folders = new_folder :: folder.folders } ]
;;

let edit_folder_title t folder =
  match Input.prompt_for_title t folder.name with
  | None -> [ folder ]
  | Some name ->
    let new_folder = { folder with name } in
    [ new_folder ]
;;

let edit_todo state todos n =
  let new_todos =
    List.mapi
      (fun i todo ->
        if i = n - 1
        then (
          match
            Input.prompt_for_title_and_description state todo.title todo.description
          with
          | None -> todo
          | Some (title, description) -> { todo with title; description })
        else todo)
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

let img_of_display_list list selected_index state =
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
        | true, _ when n = selected_index ->
          A.fg
            (A.rgb_888
               ~r:state.scheme.comp_selected.r
               ~g:state.scheme.comp_selected.g
               ~b:state.scheme.comp_selected.b)
        | true, _ ->
          A.fg
            (A.rgb_888
               ~r:state.scheme.completed.r
               ~g:state.scheme.completed.g
               ~b:state.scheme.completed.b)
        | false, _ when n = selected_index ->
          A.fg
            A.(
              rgb_888
                ~r:state.scheme.selected.r
                ~g:state.scheme.selected.g
                ~b:state.scheme.selected.b)
        | false, _ ->
          A.fg
            A.(
              rgb_888 ~r:state.scheme.todo.r ~g:state.scheme.todo.g ~b:state.scheme.todo.b)
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
      let attr =
        if n = selected_index
        then
          A.(
            fg
              A.(
                rgb_888
                  ~r:state.scheme.selected.r
                  ~g:state.scheme.selected.g
                  ~b:state.scheme.selected.b))
        else
          A.(
            fg
              A.(
                rgb_888
                  ~r:state.scheme.folder.r
                  ~g:state.scheme.folder.g
                  ~b:state.scheme.folder.b))
      in
      let string_img = img_of_string (prefix ^ folder.name) 80 attr in
      let img = I.hpad indent 0 string_img in
      aux rest selected_index (I.vcat [ acc; img ]) (n + 1)
  in
  aux list selected_index I.empty 0
;;

open Common

let rec main_tui_loop (state : Common.state) =
  let state =
    { state with
      img =
        img_of_display_list
          (display_list_of_folders state.folders)
          state.selected_index
          state
    }
  in
  Term.image state.t state.img;
  match Term.event state.t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) ->
    Data.store_todos state.folders;
    let target_folder = Data.get_home_directory () ^ "/.todo" in
    let old_dir = Unix.getcwd () in
    Unix.chdir target_folder;
    let channel = Unix.open_process_in "git status" in
    let _ = input_line channel in
    let output = input_line channel in
    let _ = Unix.close_process_in channel in
    let contains s1 s2 =
      let re = Str.regexp_string s2 in
      try
        ignore (Str.search_forward re s1 0);
        true
      with
      | Not_found -> false
    in
    let _ =
      if contains output "ahead"
      then Sys.command "git add . && git commit --amend --no-edit"
      else Sys.command "git add . && git commit -m 'update'"
    in
    Unix.chdir old_dir
  | `Resize _ -> main_tui_loop state
  | `Key (`Arrow d, _) ->
    let max_index = List.length (display_list_of_folders state.folders) - 1 in
    let new_index =
      match d with
      | `Up -> max 0 (state.selected_index - 1)
      | `Down -> min max_index (state.selected_index + 1)
      | _ -> state.selected_index
    in
    let y = state.pos |> snd in
    let x = state.pos |> fst in
    let new_pos =
      let selected_y = y + new_index - state.selected_index in
      match d with
      | `Up -> x, selected_y
      | `Down -> x, selected_y
      | `Left -> x, y
      | `Right -> x, y
    in
    let state = { state with selected_index = new_index; pos = new_pos } in
    main_tui_loop state
  | `Key (`Enter, _) ->
    let () = Data.store_todos state.folders in
    main_tui_loop
      { state with
        folders =
          map_display_list_nth
            state.selected_index
            state.folders
            toggle_folder_status
            toggle_todo_status
      }
  | `Key (`ASCII 'D', [ `Ctrl ]) ->
    let state =
      { state with
        folders =
          map_display_list_nth
            state.selected_index
            state.folders
            delete_folder
            delete_todo
      }
    in
    let () = Data.store_todos state.folders in
    main_tui_loop state
  (* add new folder *)
  | `Key (`ASCII 'n', _) ->
    let item = List.nth (display_list_of_folders state.folders) state.selected_index in
    (match item with
     | Todo _ -> main_tui_loop state
     | Folder _ ->
       let folders =
         map_display_list_nth
           state.selected_index
           state.folders
           (add_sub_folder state)
           (fun todo _ -> todo)
       in
       let () = Data.store_todos folders in
       main_tui_loop { state with folders })
  (* TODO: Allow for creating sub dirs *)
  | `Key (`ASCII 'N', [ `Ctrl ]) ->
    let item = List.nth (display_list_of_folders state.folders) state.selected_index in
    (match item with
     | Todo _ ->
       let state =
         { state with
           folders =
             map_display_list_nth
               state.selected_index
               state.folders
               (add_new_folder state)
               (add_new_todo state)
         }
       in
       let () = Data.store_todos state.folders in
       main_tui_loop state
     | Folder _ ->
       let folders =
         map_display_list_nth
           state.selected_index
           state.folders
           (add_new_folder state)
           (fun todo _ -> todo)
       in
       let () = Data.store_todos folders in
       main_tui_loop { state with folders })
  | `Key (`ASCII 'a', _) ->
    let folders =
      map_display_list_nth
        state.selected_index
        state.folders
        (add_new_todo_in_folder state)
        (add_new_todo state)
    in
    let () = Data.store_todos folders in
    main_tui_loop { state with folders }
  (* Shift item up or down *)
  | `Key (`ASCII 'j', _) ->
    if state.selected_index >= List.length (display_list_of_folders state.folders) - 1
    then main_tui_loop state
    else (
      match
        ( List.nth (display_list_of_folders state.folders) state.selected_index
        , List.nth (display_list_of_folders state.folders) (state.selected_index + 1) )
      with
      | Folder _, _ | Todo _, Folder _ -> main_tui_loop state
      | Todo _, Todo _ ->
        let folders =
          map_display_list_nth
            state.selected_index
            state.folders
            (fun t -> [ t ])
            (move_todo 1)
        in
        let selected_index = state.selected_index + 1 in
        Data.store_todos folders;
        main_tui_loop { state with selected_index; folders })
  | `Key (`ASCII 'k', _) ->
    if state.selected_index <= 0
    then main_tui_loop state
    else (
      match
        ( List.nth (display_list_of_folders state.folders) state.selected_index
        , List.nth (display_list_of_folders state.folders) (state.selected_index - 1) )
      with
      | Folder _, _ | Todo _, Folder _ -> main_tui_loop state
      | Todo _, Todo _ ->
        let folders =
          map_display_list_nth
            state.selected_index
            state.folders
            (fun t -> [ t ])
            (move_todo (-1))
        in
        let selected_index = state.selected_index - 1 in
        Data.store_todos folders;
        main_tui_loop { state with selected_index; folders })
  | `Key (`ASCII 'e', _) ->
    let item = List.nth (display_list_of_folders state.folders) state.selected_index in
    (match item with
     | Todo _ ->
       let folders =
         map_display_list_nth
           state.selected_index
           state.folders
           (fun t -> [ t ])
           (edit_todo state)
       in
       let () = Data.store_todos folders in
       main_tui_loop { state with folders }
     | Folder _ ->
       let folders =
         map_display_list_nth
           state.selected_index
           state.folders
           (edit_folder_title state)
           (fun todo _ -> todo)
       in
       let () = Data.store_todos folders in
       main_tui_loop { state with folders })
  | _ -> main_tui_loop state
;;
