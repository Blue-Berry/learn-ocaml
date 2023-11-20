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
