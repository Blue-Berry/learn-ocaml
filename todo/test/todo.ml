open OUnit2

let insert_at_tests =
  "test suite for insert_at"
  >::: [ ("insert in the middle"
          >:: fun _ -> assert_equal "testthis" (Lib.Tui.insert_at 5 'h' "testtis"))
       ; ("insert at the end"
          >:: fun _ -> assert_equal "testthis" (Lib.Tui.insert_at 8 's' "testthi"))
       ; ("insert at the beginning"
          >:: fun _ -> assert_equal "testthis" (Lib.Tui.insert_at 0 't' "estthis"))
       ]
;;

(* Lib.Tui.remove_at *)
let remove_at_tests =
  "test suite for remove_at"
  >::: [ ("remove in the middle"
          >:: fun _ -> assert_equal "testtis" (Lib.Tui.remove_at 5 "testthis"))
       ; ("remove at the end"
          >:: fun _ -> assert_equal "testthi" (Lib.Tui.remove_at 7 "testthis"))
       ; ("remove at the beginning"
          >:: fun _ -> assert_equal "estthis" (Lib.Tui.remove_at 0 "testthis"))
       ]
;;

(* Lib.Tui.calc_cursor_pos *)
let string_of_int_pair (x, y) = Printf.sprintf "(%d, %d)" x y

let calc_cursor_pos_tests =
  "test suite for calc_cursor_pos"
  >::: [ ("calc_cursor_pos with empty string"
          >:: fun _ -> assert_equal (1, 1) (Lib.Tui.calc_cursor_pos "" "" 0))
       ; ("calc_cursor_pos with string"
          >:: fun _ ->
          assert_equal
            (1, 1)
            (Lib.Tui.calc_cursor_pos "test" "" 0)
            ~printer:string_of_int_pair)
       ]
;;

let _ = run_test_tt_main insert_at_tests
let _ = run_test_tt_main remove_at_tests
let _ = run_test_tt_main calc_cursor_pos_tests
