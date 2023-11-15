type todo =
  { title : string
  ; description : string
  ; completed : bool
  }
[@@deriving yojson]

type todos = { todos : todo list } [@@deriving yojson]

let get_home_directory () =
  try Sys.getenv "HOME" with
  (*TODO: change this to use a result and pass the result up *)
  | Not_found -> failwith "Unable to retrieve home directory"
;;

let get_json_file_path () =
  let home_directory = get_home_directory () in
  let todo_directory = Filename.concat home_directory ".todo" in
  let json_file = Filename.concat todo_directory "todos.json" in
  if not (Sys.file_exists todo_directory)
  then (
    Unix.mkdir todo_directory 0o700;
    ())
  else ();
  if not (Sys.file_exists json_file)
  then (
    let _ = Unix.openfile json_file [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600 in
    ())
  else ();
  json_file
;;

let json_of_todos todos = todos_to_yojson todos
let todos_of_json json = todos_of_yojson json

let store_todos todos =
  let json_file = get_json_file_path () in
  let json = json_of_todos todos in
  let out_channel = open_out json_file in
  let _ = Yojson.Safe.pretty_to_channel out_channel json in
  close_out out_channel
;;

let get_todos () =
  let json_file = get_json_file_path () in
  let in_channel = open_in json_file in
  try
    let json = Yojson.Safe.from_channel in_channel in
    let todos = todos_of_json json in
    close_in in_channel;
    match todos with
    | Ok todos -> todos
    | Error _ ->
      store_todos { todos = [] };
      { todos = [] }
  with
  | Yojson.Json_error _ ->
    close_in in_channel;
    store_todos { todos = [] };
    { todos = [] }
  | _ ->
    close_in in_channel;
    store_todos { todos = [] };
    failwith "Unable to read todos"
;;
