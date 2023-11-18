(* let folders = Lib.Data.get_todos () *)
let folders : Lib.Data.folders =
  [ { name = "parent1"
    ; is_open = true
    ; folders =
        [ { name = "child1"
          ; is_open = true
          ; folders = []
          ; todos =
              [ { title = "test todo"; description = "test todo"; completed = false }
              ; { title = "test todo"; description = "test todo"; completed = false }
              ]
          }
        ]
    ; todos =
        [ { title = "test todo"; description = "test todo"; completed = false }
        ; { title = "test todo"; description = "test todo"; completed = false }
        ]
    }
  ; { name = "parent2"
    ; is_open = true
    ; folders =
        [ { name = "child2"
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
