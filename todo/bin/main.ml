let folders = Lib.Data.get_todos ()
let () = Lib.Tui.main_tui_loop (Notty_unix.Term.create ()) (0, 0) 0 folders
