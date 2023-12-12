open OUnit2

let insert_at_tests =
  "test suite for insert_at"
  >::: [ ("insert in the middle"
          >:: fun _ ->
          assert_equal "testthis" (Lib.Input.insert_at 5 'h' "testtis") ~printer:(fun x ->
            x))
       ; ("insert at the end"
          >:: fun _ ->
          assert_equal "testthis" (Lib.Input.insert_at 8 's' "testthi") ~printer:(fun x ->
            x))
       ; ("insert at the beginning"
          >:: fun _ ->
          assert_equal "testthis" (Lib.Input.insert_at 0 't' "estthis") ~printer:(fun x ->
            x))
       ]
;;

(* Lib.Input.remove_at *)
let remove_at_tests =
  "test suite for remove_at"
  >::: [ ("remove in the middle"
          >:: fun _ -> assert_equal "testtis" (Lib.Input.remove_at 5 "testthis"))
       ; ("remove at the end"
          >:: fun _ -> assert_equal "testthi" (Lib.Input.remove_at 7 "testthis"))
       ; ("remove at the beginning"
          >:: fun _ -> assert_equal "estthis" (Lib.Input.remove_at 0 "testthis"))
       ]
;;

(* Lib.Tui.calc_cursor_pos *)
let string_of_int_pair (x, y) = Printf.sprintf "(%d, %d)" x y
let rec repeat_string s n = if n = 0 then "" else s ^ repeat_string s (n - 1)

let rec repeat_string_with_num s n =
  if n = 0 then "" else string_of_int n ^ s ^ repeat_string_with_num s (n - 1)
;;

(* TODO: add tests for cursor position with wrapping etc *)
let calc_cursor_pos_tests =
  "test suite for calc_cursor_pos"
  >::: [ ("calc_cursor_pos with empty string"
          >:: fun _ ->
          assert_equal
            (1, 1)
            (Lib.Input.calc_cursor_pos "" "" 0)
            ~printer:string_of_int_pair)
       ; ("calc_cursor_pos with string"
          >:: fun _ ->
          assert_equal
            (2, 1)
            (Lib.Input.calc_cursor_pos "test" "" 0)
            ~printer:string_of_int_pair)
       ; ("calc_cursor_pos with string and cursor at the end"
          >:: fun _ ->
          assert_equal
            (6, 1)
            (Lib.Input.calc_cursor_pos "test" "" 4)
            ~printer:string_of_int_pair)
       ; ("calc_cursor_pos with '\n' in string"
          >:: fun _ ->
          assert_equal
            (1, 2)
            (Lib.Input.calc_cursor_pos "test\n" "" 4)
            ~printer:string_of_int_pair)
       ; ("calc_cursor_pos with '\n' and string wrapping of 80"
          >:: fun _ ->
          assert_equal
            (1, 6)
            (Lib.Input.calc_cursor_pos "test\n" (repeat_string "test\n" 4) 4)
            ~printer:string_of_int_pair)
       ]
;;

(* Lib.Input.lines_of_string *)
let lines_of_string_tests =
  "test suite for lines_of_string"
  >::: [ ("lines_of_string with empty string"
          >:: fun _ ->
          assert_equal [ "" ] (Lib.Input.lines_of_string "") ~printer:(fun x ->
            String.concat "\n" x))
       ; ("lines_of_string with string"
          >:: fun _ ->
          assert_equal [ "test" ] (Lib.Input.lines_of_string "test") ~printer:(fun x ->
            String.concat "\n" x))
       ; ("lines_of_string with string and newline"
          >:: fun _ ->
          assert_equal
            [ "test"; "" ]
            (Lib.Input.lines_of_string "test\n")
            ~printer:(fun x -> String.concat "\n" x))
       ; ("lines_of_string with string and newline and wrapping"
          >:: fun _ ->
          assert_equal
            [ "10test"
            ; "9test"
            ; "8test"
            ; "7test"
            ; "6test"
            ; "5test"
            ; "4test"
            ; "3test"
            ; "2test"
            ; "1test"
            ; ""
            ]
            (Lib.Input.lines_of_string (repeat_string_with_num "test\n" 10))
            ~printer:(fun x -> String.concat "\n" x))
       ]
;;

(* Lib.Tui.move_todo *)
(* index n starts at 1 *)
let move_todo_tests =
  "test suite for move_todo"
  >::: [ ("move_todo with empty list"
          >:: fun _ -> assert_equal [] (Lib.Tui.Maps.move_todo 0 [] 0))
       ; ("swamp first two"
          >:: fun _ ->
          assert_equal
            [ "two"; "one" ]
            (Lib.Tui.Maps.move_todo 1 [ "one"; "two" ] 1)
            ~printer:(fun x -> String.concat "; " x))
       ; ("swamp last two"
          >:: fun _ ->
          assert_equal
            [ "one"; "three"; "two" ]
            (Lib.Tui.Maps.move_todo 1 [ "one"; "two"; "three" ] 2)
            ~printer:(fun x -> String.concat "; " x))
       ; ("Move item backwards"
          >:: fun _ ->
          assert_equal
            [ "one"; "three"; "two" ]
            (Lib.Tui.Maps.move_todo (-1) [ "one"; "two"; "three" ] 3))
       ]
;;

let _ = run_test_tt_main insert_at_tests
let _ = run_test_tt_main remove_at_tests
let _ = run_test_tt_main calc_cursor_pos_tests
let _ = run_test_tt_main lines_of_string_tests
let _ = run_test_tt_main move_todo_tests
