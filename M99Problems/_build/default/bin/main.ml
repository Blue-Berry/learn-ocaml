let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: v -> last v
;;
