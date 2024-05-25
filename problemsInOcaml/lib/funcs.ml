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

let slice_from_list l start last =
  let len = last + 1 - start in
  if len < 0
  then raise @@ Failure "The start position cannot be bigger than the last position"
  else (
    let _, after_start = split_at_pos l start in
    let slice, _ = split_at_pos after_start len in
    slice)
;;

let rotate_left l qtd_rot =
  let len = List.length l in
  let rot = qtd_rot mod len in
  let split_at = if rot < 0 then len + rot else rot in
  let left, right = split_at_pos l split_at in
  right @ left
;;

let remove_at pos l =
  let rec aux cnt acc = function
    | [] -> List.rev acc
    | h :: tl -> if cnt = pos then List.rev acc @ tl else aux (cnt + 1) (h :: acc) tl
  in
  aux 0 [] l
;;

let insert_at value pos l =
  let rec aux cnt acc = function
    | [] -> List.rev @@ if cnt <= pos then value :: acc else acc
    | h :: tl as li ->
      if cnt = pos then (List.rev @@ (value :: acc)) @ li else aux (cnt + 1) (h :: acc) tl
  in
  aux 0 [] l
;;

let range start last =
  let big, small, op = if last > start then last, start, ( + ) else start, last, ( - ) in
  let size = big - small + 1 in
  List.init size (fun x -> op start x)
;;

let extract_randomly l qtd =
  let qtd_to_extract = min qtd @@ List.length l in
  let rec aux cnt acc rest =
    if cnt < qtd_to_extract
    then (
      let to_remove = List.length rest |> Random.int in
      aux (cnt + 1) (List.nth rest to_remove :: acc) (remove_at to_remove rest))
    else List.rev acc
  in
  aux 0 [] l
;;

let lotto_select ~qtd ~end_bound = extract_randomly (range 1 end_bound) qtd
let permutation l = extract_randomly l @@ List.length l

let rec combination k l =
  if k <= 0
  then [ [] ]
  else (
    match l with
    | [] -> []
    | h :: tl ->
      let withoutCur = combination k tl in
      let withCur = List.map (fun x -> h :: x) @@ combination (k - 1) tl in
      withCur @ withoutCur)
;;

(* TODO: Try to solve this with map instead of List.iter and ref []*)
let group l groups =
  let rec aux remaining_mem = function
    | [] -> [ [] ]
    | h :: tl ->
      let new_groups = combination h remaining_mem in
      let res = ref [] in
      List.iter
        (fun choosed_g ->
          let rest =
            List.filter (fun mem -> not @@ List.mem mem choosed_g) remaining_mem
          in
          let returned_groups = aux rest tl in
          List.iter (fun new_g -> res := (choosed_g :: new_g) :: !res) returned_groups)
        new_groups;
      !res
  in
  aux l groups
;;

let rec sort ?(cmp = fun (x : 'a) (y : 'a) -> x > y) (l : 'a list) =
  let rec insert h = function
    | [] -> [ h ]
    | g :: tl as li -> if cmp h g then h :: li else g :: insert h tl
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: tl -> aux (insert h acc) tl
  in
  aux [] l

and sort_list_length l = sort ~cmp:(fun x y -> List.length x > List.length y) l

and sort_list_frequency l =
  let freq_of_size li =
    let size = List.length li in
    List.fold_left (fun acc x -> if List.length x = size then acc + 1 else acc) 0 l
  in
  sort
    ~cmp:(fun x y ->
      let fx = freq_of_size x in
      let fy = freq_of_size y in
      if fx = fy then List.length x < List.length y else fx > fy)
    l
;;
