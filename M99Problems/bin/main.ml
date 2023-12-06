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

(* Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

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

(* Duplicate the Elements of a List *)
let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | x :: v -> aux (x :: x :: acc) v
  in
  List.rev (aux [] lst)
;;

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

(* Decode a Run-Length Encoded List *)
(* #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];; *)
(* - : string list = *)
(* ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

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
