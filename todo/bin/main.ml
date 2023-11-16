let todo_list = Data.get_todos ()
let () = Tui.main_tui_loop (Notty_unix.Term.create ()) (0, 0) 0 todo_list
