(* Fall 14 Midterm *)
type expr =
  | Const of int
  | Var of string
  | Op of string * expr * expr;;

let rec rename_var e n1 n2 =
  match e with
   | Const a -> e
   | Var s   -> if s = n1 then Var n2 else e
   | Op (s, e1, e2) ->   let e1' = rename_var e1 n1 n2 in
                         let e2' = rename_var e2 n1 n2 in
                              if s = n1 then Op (n2, e1', e2') else Op (s, e1', e2');;

let to_str e =
  let rec str_helper e top_level =
    match e with
     | Const a       -> string_of_int a
     | Var s         -> s
     | Op (s, e1, e2)-> if top_level then (str_helper e1 false)^s^(str_helper e2 false)
                                     else "("^(str_helper e1 false)^s^(str_helper e2 false)^")"
  in str_helper e true;;

let average_if f l =
  let folding_fn (sum, count) elem =
    if f elem then (sum+elem, count+1) else (sum, count) in
  let base = (0, 0) in
  let (sum, count) = List.fold_left folding_fn base l in
    if count = 0 then 0 else sum/count;;

let length l =
  let folding_fn sum elem = sum+1 in
  List.fold_left folding_fn 0 l;;

let length_2 l =
  List.fold_left (+) 0 (List.map length l);;

let length_3 l =
  List.fold_left (+) 0 (List.map length_2 l);;

(* Fall 13 Midterm *)
let count l x =
  let folding_fn acc elem = if elem = x then acc+1 else acc in
  List.fold_left folding_fn 0 l;;

let make_palyndrome l =
  let folding_fn acc elem = elem::acc in
  List.fold_left folding_fn l l;;

let fold_2 f b l =
  let folding_fn (acc, indx) elem = (f acc elem indx, indx+1) in
  let (res,_) = List.fold_left folding_fn (b,0) l in
  res;;

let rec ith l i d =
  let folding_fn res elem indx =
    if indx = i then elem else res in
  fold_2 folding_fn d l;;

type 'a fun_tree =
  | Leaf of ('a -> 'a)
  | Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
  match t with
   | Leaf f -> f x
   | Node (left, right) -> apply_all right (apply_all left x);;

let rec compose t1 t2 =
  match (t1,t2) with
    | (Leaf f1, Leaf f2) -> Leaf (fun x -> f1 f2 x)
    | (Node (l1, r1), Node (l2, r2)) -> Node (compose l1 l2, compose r1 r2);;

(* Spring 13 *)

let length l =
  let folding_fn acc elem = acc+1 in
  List.fold_left folding_fn 0 l;;

let remove l x =
  let folding_fn acc elem = if elem = x then acc else acc@[elem] in
  List.fold_left folding_fn [] l;;

let rec ith l i d =
  match l with
  | [] ->  d
  | h::t -> if i=0 then h else ith t (i-1) d;;

let rec update l i n =
  match l with
  | [] -> []
  | h::t -> if i=0 then n::t else h::(update t (i-1) n);;

let rec update2 l i n d =
  match l with
    | [] -> if i=0 then [n] else update2 [d] i n d
    | h::t -> if i=0 then n::t else h::(update2 t (i-1) n d);;

let categorize f l =
  let base = [] in
  let folding_fn acc elem =
    let target = f elem in
    let old_bin = ith acc target [] in
    let new_bin = old_bin@[elem] in
    update2 acc target new_bin [] in
  List.fold_left folding_fn base l;;

(* Winter 13 *)
type 'a maybe =
| None
| Some of 'a;;

let first f l =
  let folding_fn acc elem =
  if acc = None then if f elem then Some elem else None
                else acc                                  in
  List.fold_left folding_fn None l;;

let rec zip l1 l2 =
    match (l1, l2) with
      | ([],[]) -> []
      | ([],h::t) -> []
      | (h::t,[]) -> []
      | (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2);;

let map2 f l1 l2 =
  let helper (a,b) = f a b in
  List.map helper (zip l1 l2);;

let map3 f l1 l2 l3 =
  let helper ((a,b),c) = a+b+c in
  List.map helper (zip (zip l1 l2) l3);;

let rec unzip l =
  match l with
    | [] -> ([],[])
    | (a,b)::t -> let (l1, l2) = unzip t in (a::l1, b::l2);;

(* Winter 12 *)
let rec split l =
  let cut = (length l)/2 in
  let base = (1,[],[]) in
  let folding_fn (i,l1,l2) elem = if i<=cut then (i+1, l1@[elem], l2) else (i+1, l1, l2@[elem]) in
  let (_, l1, l2) = List.fold_left folding_fn base l in
  (l1,l2);;

let rec merge l1 l2 =
  match (l1, l2) with
    | ([], l) -> l
    | (l, []) -> l
    | (h1::t1, h2::t2) -> if h1<=h2 then h1::(merge t1 l2) else h2::(merge l1 t2);;

let rec merge_sort l =
  match l with
    | [] -> []
    | h::t -> if t=[] then [h] else
                                  let (l,r) = split l in
                                  let l_sorted = merge_sort l in
                                  let r_sorted = merge_sort r in
                                  merge l_sorted r_sorted;;

let change s =
  let l = explode s in
  let helper acc elem = if elem = '-' then acc@[' '] else acc@[elem] in
  let res = List.map helper l in
  implode res;;

let app l x =
  let helper f = f x in
  List.map helper l;;
