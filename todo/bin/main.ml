let is_git_repository folder_path =
  let git_folder = Filename.concat folder_path ".git" in
  Sys.file_exists git_folder && Sys.is_directory git_folder
;;

let init_git_repository folder_path =
  if not (is_git_repository folder_path)
  then (
    Printf.printf "Initializing Git repository in %s.\n" folder_path;
    (* Change the working directory to the target folder *)
    let old_dir = Unix.getcwd () in
    Unix.chdir folder_path;
    let git_init_command = "git init" in
    let result = Sys.command git_init_command in
    Unix.chdir old_dir;
    (* Restore the original working directory *)
    match result with
    | 0 -> Printf.printf "Git repository initialized successfully in %s.\n" folder_path
    | _ -> Printf.printf "Failed to initialize Git repository in %s.\n" folder_path)
  else Printf.printf "Git repository already exists in %s.\n" folder_path
;;

let () =
  let target_folder = Lib.Data.get_home_directory () ^ "/.todo" in
  init_git_repository target_folder
;;

let folders = Lib.Data.get_todos ()

let state : Lib.Common.state =
  { folders
  ; t = Notty_unix.Term.create ()
  ; pos = 0, 0
  ; selected_index = 0
  ; img = Notty.I.empty
  ; scheme = Lib.Colours.Colourscheme.get_scheme ()
  }
;;

let () = Lib.Tui.main_tui_loop state
