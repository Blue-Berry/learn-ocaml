(* let folders = Lib.Data.get_todos () *)
let folders : Lib.Data.folders =
  [ { name = "test folder"
    ; is_open = true
    ; folders =
        [ { name = "test folder"
          ; is_open = true
          ; folders = []
          ; todos =
              [ { title = "test todo"; description = "test todo"; completed = false } ]
          }
        ]
    ; todos =
        [ { title = "test todo"; description = "test todo"; completed = false }
        ; { title = "test todo"; description = "test todo"; completed = false }
        ]
    }
  ; { name = "test folder"
    ; is_open = true
    ; folders =
        [ { name = "test folder"
          ; is_open = true
          ; folders = []
          ; todos =
              [ { title = "test todo"; description = "test todo"; completed = false } ]
          }
        ]
    ; todos =
        [ { title = "test todo"; description = "test todo"; completed = false }
        ; { title = "test todo"; description = "test todo"; completed = false }
        ]
    }
  ]
;;

let () = Lib.Tui.main_tui_loop (Notty_unix.Term.create ()) (0, 0) 0 folders
