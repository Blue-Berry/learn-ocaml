(* Tail of a List *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: v -> last v
;;

(* Last Two Elements of a List *)
let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: v -> last_two v
;;

(* N'th Element of a List *)
let rec nth lst n =
  match lst, n with
  | [], _ -> failwith "Out of bounds"
  | x :: _, 0 -> x
  | _ :: v, n -> nth v (n - 1)
;;

(* Length of a List *)
let length lst =
  let rec aux n = function
    | [] -> n
    | _ :: v -> aux (n + 1) v
  in
  aux 0 lst
;;

(* Reverse a List *)
let rev lst =
  let rec aux nlst = function
    | [] -> nlst
    | x :: v -> aux (x :: nlst) v
  in
  aux [] lst
;;

(* Palindrome *)
let is_palindrome lst = lst = rev lst

(* Duplicate the Elements of a List *)
(* let duplicate lst = *)
(*   let rec aux acc = function *)
(*     | [] -> acc *)
(*     | x :: v -> aux (x :: x :: acc) v *)
(*   in *)
(*   List.rev (aux [] lst) *)
(* ;; *)

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split lst n =
  let rec aux acc n = function
    | [] -> acc, []
    | x :: v as t -> if n = 0 then acc, t else aux (x :: acc) (n - 1) v
  in
  aux [] n lst |> fun (a, b) -> List.rev a, b
;;

(* Flatten a List *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One h :: t -> aux (h :: acc) t
    | Many sub_lst :: t -> aux (aux acc sub_lst) t
  in
  List.rev (aux [] lst)
;;

(* Eliminate Duplicates *)
let rec compress = function
  | h1 :: (h2 :: _ as t) -> if h1 = h2 then compress t else h1 :: compress t
  | x -> x
;;

(* Pack Consecutive Duplicates *)
(* # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];; *)
(* - : string list list = *)
(* [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; *)
(*  ["e"; "e"; "e"; "e"] *)
let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | h1 :: (h2 :: _ as t) ->
      if h1 = h2 then aux (h1 :: current) acc t else aux [] ((h1 :: current) :: acc) t
  in
  List.rev (aux [] [] lst)
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a

(* Run-Length Encoding *)
let encode lst =
  let create_tuple count elem = if count = 1 then One elem else Many (count, elem) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] lst)
;;

(* Decode a Run-Length Encoded List *)
(* #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)
(* - : string list = *)
(* ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

let decode lst =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One a :: t -> aux (a :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  aux [] lst
;;

(* Duplicate the Elements of a List *)
let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  aux [] lst
;;

(* Replicate the Elements of a List a Given Number of Times *)
let replicate lst n =
  let rec many acc a = function
    | 0 -> acc
    | n -> many (a :: acc) a (n - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (many acc h n) t
  in
  aux [] (List.rev lst)
;;

Printf.printf "Tail of a List: %d\n" (Option.get (last [ 1; 2; 3; 4; 5 ]));
Printf.printf
  "Last Two Elements of a List: %d\n"
  (let a, b = Option.get (last_two [ 1; 2; 3; 4; 5 ]) in
   a + b);
Printf.printf "N'th Element of a List: %d\n" (nth [ 1; 2; 3; 4; 5 ] 2);
Printf.printf "Length of a List: %d\n" (length [ 1; 2; 3; 4; 5 ]);
Printf.printf "Reverse a List: %s\n" (String.concat "" (rev [ "a"; "b"; "c" ]));
Printf.printf "Palindrome: %b\n" (is_palindrome [ 1; 2; 3; 2; 1 ]);
Printf.printf
  "Duplicate the Elements of a List: %s\n"
  (String.concat "" (duplicate [ "a"; "b"; "c" ]));
Printf.printf
  "Split a List Into Two Parts; The Length of the First Part Is Given: %s\n"
  (let a, b = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k" ] 3 in
   String.concat "" a ^ " " ^ String.concat "" b);
Printf.printf
  "Flatten a List: %s\n"
  (String.concat
     ""
     (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]));
Printf.printf
  "Eliminate Duplicates: %s\n"
  (String.concat
     ""
     (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e" ]));
Printf.printf
  "Pack Consecutive Duplicates: %s\n"
  (String.concat
     ""
     (pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e" ]
      |> List.map (String.concat "")));
Printf.printf
  "Run-Length Encoding: %s\n"
  (String.concat
     ""
     (encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e" ]
      |> List.map (function
        | One x -> x
        | Many (n, x) -> string_of_int n ^ x)));
Printf.printf
  "Decode a Run-Length Encoded List: %s\n"
  (String.concat
     ""
     (decode
        [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]));
Printf.printf
  "Duplicate the Elements of a List: %s\n"
  (String.concat "" (duplicate [ "a"; "b"; "c" ]));
Printf.printf
  "Replicate the Elements of a List a Given Number of Times: %s\n"
  (String.concat "" (replicate [ "a"; "b"; "c" ] 3))

(* Drop Every N'th Element From a List *)
(* # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
let drop lst n =
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> if i = n then aux acc 1 t else aux (h :: acc) (i + 1) t
  in
  aux [] 1 lst
;;

Printf.printf
  "Drop Every N'th Element From a List: %s\n"
  (String.concat "" (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3))

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
(* # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3;; *)
(* - : string list * string list = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "k"]) *)

let split lst n =
  let rec aux acc i = function
    | [] -> acc, []
    | h :: t as l -> if i = n then acc, l else aux (h :: acc) (i + 1) t
  in
  aux [] 1 lst
;;

Printf.printf
  "Split a List Into Two Parts; The Length of the First Part Is Given: %s\n"
  (let a, b = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k" ] 3 in
   String.concat "" a ^ " " ^ String.concat "" b)

(* Extract a Slice From a List *)
(* # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k"] 3 7;; *)
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)

let slice lst start stop =
  let rec aux acc i = function
    | [] -> acc
    | h :: t ->
      if i >= start && i <= stop then aux (h :: acc) (i + 1) t else aux acc (i + 1) t
  in
  List.rev (aux [] 1 lst)
;;

Printf.printf
  "Extract a Slice From a List: %s\n"
  (String.concat "" (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "k" ] 3 7))

(* Rotate a List N Places to the Left *)
(* # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;; *)
(* - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)
(* # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);; *)
(* - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
