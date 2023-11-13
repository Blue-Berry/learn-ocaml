let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: v -> last v
;;

let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: v -> last_two v
;;

let rec nth lst n =
  match lst, n with
  | [], _ -> failwith "Out of bounds"
  | x :: _, 0 -> x
  | _ :: v, n -> nth v (n - 1)
;;

let length lst =
  let rec aux n = function
    | [] -> n
    | _ :: v -> aux (n + 1) v
  in
  aux 0 lst
;;

let rev lst =
  let rec aux nlst = function
    | [] -> nlst
    | x :: v -> aux (x :: nlst) v
  in
  aux [] lst
;;

let is_palindrome lst = lst = rev lst

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

let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | x :: v -> aux (x :: x :: acc) v
  in
  List.rev (aux [] lst)
;;

let split lst n =
  let rec aux acc n = function
    | [] -> acc, []
    | x :: v as t -> if n = 0 then acc, t else aux (x :: acc) (n - 1) v
  in
  aux [] n lst |> fun (a, b) -> List.rev a, b
;;
