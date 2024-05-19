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

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten (list : 'a node list) : 'a list =
  let rec aux acc = function
    | [] -> acc
    | One h :: tl -> aux (h :: acc) tl
    | Many l :: tl -> aux (aux acc l) tl
  in
  aux [] list |> List.rev
;;

let compress l =
  let rec aux prev acc = function
    | [] -> acc
    | h :: tl -> if h = prev then aux prev acc tl else aux h (h :: acc) tl
  in
  try
    let head = List.hd l in
    aux head [ head ] (List.tl l) |> List.rev
  with
  | Failure _ -> []
  | _ -> failwith "Something not expected happened"
;;

(* TODO: Not tail recursive, implement a tail recursive version *)
let pack l =
  let only_returns_elem elem _ = elem in
  let rec aux elem cnt_same = function
    | [] -> [ List.init cnt_same @@ only_returns_elem elem ]
    | h :: tl ->
      if h = elem
      then aux elem (cnt_same + 1) tl
      else List.init cnt_same (only_returns_elem elem) :: aux h 1 tl
  in
  try aux (List.hd l) 1 (List.tl l) with
  | Failure _ -> []
  | _ -> failwith "Something not expected happened"
;;

let encode l = List.map (fun x -> List.length x, List.hd x) @@ pack l

type 'a rle =
  | One of 'a
  | Many of int * 'a

let rle_encode l =
  List.map (function
    | 1, e -> One e
    | n, e -> Many (n, e))
  @@ encode l
;;

let rec rle_decode = function
  | [] -> []
  | One e :: tl -> e :: rle_decode tl
  | Many (n, e) :: tl -> List.init n (fun _ -> e) @ rle_decode tl
;;

let dup_elems l = List.map (fun x -> [ x; x ]) l |> List.concat
(* Another solution:
   let rec dup_elems2 = function
   | [] -> []
   | h :: tl -> h :: h :: dup_elems2 tl
   ;; *)

(* This function could resolve the previous one too *)
let replicate l qtd = List.map (fun x -> List.init qtd (fun _ -> x)) l |> List.concat

let drop_nth l nth =
  let rec aux acc cnt = function
    | [] -> acc
    | h :: tl -> if cnt = 0 then aux acc (nth - 1) tl else aux (h :: acc) (cnt - 1) tl
  in
  aux [] (nth - 1) l |> List.rev
;;

let split_at_pos l pos =
  let rec aux cnt acc = function
    | [] -> List.rev acc, []
    | h :: tl as li -> if cnt = 0 then List.rev acc, li else aux (cnt - 1) (h :: acc) tl
  in
  aux pos [] l
;;

let sla = 1
