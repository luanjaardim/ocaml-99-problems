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

let is_prime n =
  let n = abs n in
  let rec aux cur = cur * cur > n || (n mod cur <> 0 && aux (cur + 1)) in
  n <> 1 && aux 2
;;

let gcd m n =
  let rec aux m n = if n = 0 then m else aux n (m mod n) in
  aux (max m n) (min m n)
;;

let coprime m n = 1 = gcd m n

let phi n =
  let rec aux cur cnt =
    if cur < n then aux (cur + 1) (cnt + if coprime n cur then 1 else 0) else cnt
  in
  if n < 1 then 0 else aux 2 1
;;

let factors n =
  let rec aux acc cur_div num =
    if num = 1
    then List.rev acc
    else if num mod cur_div = 0
    then aux (cur_div :: acc) cur_div (num / cur_div)
    else aux acc (cur_div + 1) num
  in
  aux [] 2 n
;;

let factors_and_qtd n =
  let rec aux acc cur_div num =
    if num = 1
    then List.rev acc
    else if num mod cur_div = 0
            && (not (List.is_empty acc))
            && List.hd acc |> fst = cur_div
    then (
      let _, cnt = List.hd acc in
      aux ((cur_div, cnt + 1) :: List.tl acc) cur_div (num / cur_div))
    else if num mod cur_div = 0
    then aux ((cur_div, 1) :: acc) cur_div (num / cur_div)
    else aux acc (cur_div + 1) num
  in
  aux [] 2 n
;;

let phi_improved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m) :: tl ->
      let p, m = float_of_int p, float_of_int m in
      aux (acc *. ((p -. 1.) *. (p ** (m -. 1.)))) tl
  in
  factors_and_qtd n |> aux 1. |> int_of_float
;;

(*Function to measure the execution time of some function f*)
let timeit f a =
  let t0 = Unix.gettimeofday () in
  ignore (f a);
  let t1 = Unix.gettimeofday () in
  t1 -. t0
;;

let all_primes low high =
  let rec aux acc cnt =
    if cnt = high
    then List.rev acc
    else aux (if is_prime cnt then cnt :: acc else acc) (cnt + 1)
  in
  aux [] low
;;

let goldbach n =
  let rec aux cur =
    if is_prime cur && is_prime (n - cur) then cur, n - cur else aux (cur + 1)
  in
  if n mod 2 <> 0 || n < 2 then failwith "A number even positive number must be passsed";
  aux 2
;;

let goldbach_list low high =
  let rec aux acc cur =
    if cur > high then List.rev acc else aux ((cur, goldbach cur) :: acc) (cur + 2)
  in
  aux [] @@ if low mod 2 <> 0 then low + 1 else low
;;

let goldbach_limit low high limit =
  let l = goldbach_list low high in
  List.filter (fun (_, (f, s)) -> f > limit && s > limit) l
;;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 a b expr =
  let rec aux (var_a : bool) (var_b : bool) (cur_expr : bool_expr) : bool =
    let f = aux var_a var_b in
    match cur_expr with
    | Var v when v = a -> var_a
    | Var v when v = b -> var_b
    | Var v ->
      failwith
      @@ "Variable: "
      ^ v
      ^ " is not allowed, only variable 'a' and 'b' are valid."
    | And (left, right) -> f left && f right
    | Or (left, right) -> f left || f right
    | Not inner -> not @@ f inner
  in
  List.map
    (fun (x, y) ->
      let res = aux x y expr in
      x, y, res)
    [ true, true; true, false; false, true; false, false ]
;;

let table vars expr =
  let variables_values = List.length vars |> Hashtbl.create in
  let results = ref [] in
  let rec eval_expr expr : bool =
    match expr with
    | Var v ->
      (match Hashtbl.find_opt variables_values v with
       | None -> failwith @@ "The variable: " ^ v ^ " is unknown."
       | Some v -> v)
    | And (l, r) -> eval_expr l && eval_expr r
    | Or (l, r) -> eval_expr l || eval_expr r
    | Not inner -> not @@ eval_expr inner
  in
  let rec aux = function
    | [] ->
      results
      := (Hashtbl.to_seq variables_values |> List.of_seq, eval_expr expr) :: !results
    | h :: tl ->
      Hashtbl.replace variables_values h false;
      aux tl;
      Hashtbl.replace variables_values h true;
      aux tl
  in
  aux vars;
  !results
;;

let gray n =
  let limit = 2. ** float_of_int n |> int_of_float in
  let convert_to_gray num = num lxor (num lsr 1) in
  let bitsToString number =
    let l = ref "" in
    let number = ref number in
    for _ = 1 to n do
      l := (!number land 1 |> string_of_int) ^ !l;
      number := !number lsr 1
    done;
    !l
  in
  let rec loop cur acc =
    if cur = limit
    then List.rev acc
    else loop (cur + 1) ((convert_to_gray cur |> bitsToString) :: acc)
  in
  loop 0 []
;;

type ('a, 'b) huffman_node =
  | Node of 'b * ('a, 'b) huffman_node * ('a, 'b) huffman_node
  | Leaf of 'a * 'b

let show_huffman_tree tree =
  let rec aux = function
    | Node (x, l, r) -> "Node(" ^ string_of_int x ^ ", " ^ aux l ^ ", " ^ aux r ^ ")"
    | Leaf (x, y) -> "Leaf(" ^ string_of_int y ^ " , " ^ x ^ ")"
  in
  aux tree
;;

let getFrequency = function
  | Node (x, _, _) -> x
  | Leaf (_, x) -> x
;;

let huffman (fs : ('a * 'b) list) =
  let fs_sorted =
    let l =
      List.sort (fun (_, x) (_, y) -> if x = y then 0 else if x > y then 1 else -1) fs
    in
    List.map (fun (x, y) -> Leaf (x, y)) l
  in
  let aux (fq : (string, int) huffman_node list) (sq : (string, int) huffman_node list) =
    let rec get2least first_queue second_queue acc =
      if List.length acc = 2
      then first_queue, second_queue, acc
      else (
        match first_queue, second_queue with
        | [], [] -> failwith "nothing to remove"
        | [], h :: hs -> get2least [] hs (h :: acc)
        | g :: gs, [] -> get2least gs [] (g :: acc)
        | (g :: gs as gl), (h :: hs as hl) ->
          if getFrequency g <= getFrequency h
          then get2least gs hl (g :: acc)
          else get2least gl hs (h :: acc))
    in
    let fq, sq, choosen = get2least fq sq [] in
    let l, r = List.nth choosen 0, List.nth choosen 1 in
    fq, sq @ [ Node (getFrequency l + getFrequency r, l, r) ]
  in
  let rec loop fq sq =
    if List.length fq + List.length sq <> 1
    then (
      let fq, sq = aux fq sq in
      loop fq sq)
    else if List.is_empty fq
    then sq
    else fq
  in
  let huffman_tree = loop fs_sorted [] in
  let table = Hashtbl.create @@ List.length fs in
  let rec calculateCodes acc tree =
    match tree with
    | Leaf (x, _) -> Hashtbl.add table x acc
    | Node (_, l, r) ->
      calculateCodes (acc ^ "1") l;
      calculateCodes (acc ^ "0") r
  in
  calculateCodes "" @@ List.hd huffman_tree;
  List.map (fun (x, _) -> x, Hashtbl.find table x) fs
;;
