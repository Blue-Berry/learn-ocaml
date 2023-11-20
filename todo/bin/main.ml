(* request the data in folders therefore creating them before the git init *)
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

let exec_command command =
  let result = Sys.command command in
  match result with
  | 0 -> Some 0
  | _ -> None
;;

(* git init *)
let is_git_repository folder_path =
  let git_folder = Filename.concat folder_path ".git" in
  Sys.file_exists git_folder && Sys.is_directory git_folder
;;

let init_git_repository folder_path =
  let old_dir = Unix.getcwd () in
  Unix.chdir folder_path;
  let git_init_command = "git init" in
  let result_init = Sys.command git_init_command in
  (* Restore the original working directory *)
  (match result_init with
   | 0 ->
     Printf.printf "Git repository initialized successfully in %s.\n" folder_path;
     let git_add_command = "git add ." in
     let result = exec_command git_add_command in
     let result =
       Option.bind result (fun _ -> exec_command "git commit -m \"Initial commit\"")
     in
     (match result with
      | Some _ ->
        Printf.printf "Git repository initialized successfully in %s.\n" folder_path
      | _ -> Printf.printf "Failed to initialize Git repository in %s.\n" folder_path)
   | _ -> Printf.printf "Failed to initialize Git repository in %s.\n" folder_path);
  Unix.chdir old_dir
;;

let set_git_remote folder_path remote_url =
  let old_dir = Unix.getcwd () in
  Unix.chdir folder_path;
  let git_remote_command = "git remote add origin " ^ remote_url in
  let result = exec_command git_remote_command in
  let result = Option.map (fun _ -> exec_command "git branch -M main") result in
  let result = Option.bind result (fun _ -> exec_command " git push -u origin main") in
  if Option.is_some result
  then Printf.printf "Git remote successfully set to %s.\n" remote_url
  else Printf.printf "Failed to set Git remote to %s.\n" remote_url;
  Unix.chdir old_dir;
  exit 0
;;

(* Check if the folder is a git repository, if not, initialize it. *)
let () =
  let target_folder = Lib.Data.get_home_directory () ^ "/.todo" in
  if not (is_git_repository target_folder) then init_git_repository target_folder
;;

(* Set the git remote URL if provided *)
let () =
  if Array.length Sys.argv > 1
  then
    if Sys.argv.(1) <> "--git_remote"
       || Array.length Sys.argv = 2
       || Array.length Sys.argv > 3
    then (
      let () =
        Printf.printf "Usage: %s --git_remote <remote_url> <target_folder>\n" Sys.argv.(0)
      in
      exit 1)
    else (
      let remote_url = Sys.argv.(2) in
      set_git_remote (Lib.Data.get_home_directory () ^ "/.todo") remote_url)
;;

(* starting the TUI loop *)
let () = Lib.Tui.main_tui_loop state
