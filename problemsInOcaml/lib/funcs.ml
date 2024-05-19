let rec last = function
  | [] -> None
  | [ h ] -> Some h
  | _ :: tl -> last tl
;;

let rec last_n_penultimate = function
  | [] | [ _ ] -> None
  | [ l; p ] -> Some (l, p)
  | _ :: tl -> last_n_penultimate tl
;;

let rec at n = function
  | [] -> None
  | h :: tl -> if n = 1 then Some h else at (n - 1) tl
;;

let length list =
  let rec aux acc = function
    | [] -> acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 0 list
;;

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: tl -> aux (h :: acc) tl
  in
  aux [] list
;;

let is_palin list = list = rev list
