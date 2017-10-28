(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)
(* sqsum : int list -> int
********************************************************************************
What it does is as follows:
base stores temporary result we have acquired
helper function f adds a new element's square into temporary result and return
the new temporary result. After we finish calculating the whole list, the f
function would automatically return the result we have acquired.
What is important to mention is that at first round the input of f has the
following match:
a --- base
x --- the first element in xs
and it returns the new a for the next round.
********************************************************************************
*)
let sqsum xs =
  let f a x = a+x*x in
  let base = 0 in
    List.fold_left f base xs;;

(* pipe : ('a -> 'a) list -> ('a -> 'a)
********************************************************************************
What is does is the following:
Using List.fold_left, we need to design a helper funtion f. Since we would return
a value, and this value has same type with the input y, so it is a good idea to
set base as y. Again, the helper funtion has to return the same type element, and
by checking the goal of this problem, we can know that we it needs to accomplish
at each iteration is to use that function list. And thanks to the List.fold_left，
the elements in the fs list would come one by one in order, so we now know the f
function should be defined as follows.
********************************************************************************
*)
let pipe fs y =
  let f a x = x a in
  let base = y in
    List.fold_left f base fs;;

(* sepConcat : string -> string list ->  string
********************************************************************************
What is does is the following:
We first analyse what is the final output: string. Therefore, we need a string
type element to be the base, and none of the input can be used to be that base.
So we just set base as "", an empty string. Then think about what would go into
the helper function as input one by one: that is exactly sl, hence l should be
set to sl. In this way, our helper function would work. So what does the helper
function actually do? It takes the new element and connect it with sep. Here we
have to consider some edge cases. If the temporary result is empty, namely we
just start the work and we don't need sep to connect it, we just let a be equal
to that element and that's all.
********************************************************************************
*)
let rec sepConcat sep sl =
match sl with
  | [] -> ""
  | h :: t ->
      let f a x = if a="" then x else a^sep^x in
      let base = "" in
      let l = sl in
        List.fold_left f base l;;

(* stringOfList : ('a -> string) -> 'a list -> string
********************************************************************************
What this function does is the following:
we would first apply the function f to every element in the list, which would be
super easy to implement if we use List.map. Then we concatenate the results list
using the seperating mark "; ", which can also be done by calling the function
that we have implemented above: sepConcat. Finally, we use a "[]" to embrace the
whole string.
********************************************************************************
*)
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)
(* clone : 'a -> int -> 'a list
********************************************************************************
We do this in a tail-recursion format. We establish a helper function to save
temporary results. What are the temporary results? One is the half-done list, and
the other is the index cur_n. If the index goes to 0, indicating that we have
finished the clone task, then we can output the temporary results. Otherwise, we
keep adding one element into temporary list and subtract 1 from cur_n.
********************************************************************************
*)
let rec clone x n = if n<0 then []
                    else let rec helper l cur_n =
                          if cur_n=0 then l
                          else helper (x::l) (cur_n-1) in helper [] n;;

(* callen : 'a list -> int
********************************************************************************
This function is an auxiliary function to tackle the padZero problem. What it
does is to calculate the length of a list.
********************************************************************************
*)
let rec callen l =
  let rec helper cur_len remaining_l =
    match remaining_l with
    | [] -> cur_len
    | h::t -> helper (cur_len+1) t in helper 0 l;;

(* padZero : int list -> int list -> int list * int list
********************************************************************************
With the callen function, this problem can be solved in a minute. We just create
a helper function with input of length of the first list and the length of the
second list. We compare it and finds the shorter list, we first create a train of
0s which is the length of the difference of len1 and len2, and we add it to the
corresponding shorter list. What needs to be paid attention to is the output
format, it should be a tuple with two lists.
********************************************************************************
*)
let rec padZero l1 l2 =
  let helper len1 len2 =
    if len1<len2 then ((clone 0 (len2-len1))@l1,l2)
    else (l1,(clone 0 (len1-len2))@l2) in helper (callen l1) (callen l2);;

(* removeZero : int list -> int list
********************************************************************************
What it does is the following:
if the input list is an empty list, return it
else we check the head of the list, if it's 0, ignore it and call removeZero for
the rest of the list, Otherwise, we just meet the first non-zero element, we
need to end the process and return l.
********************************************************************************
*)
let rec removeZero l =
  match l with
  | [] -> l
  | h::t -> if h=0 then removeZero t else l;;

(* bigAdd : int list -> int list -> int list
********************************************************************************
We first analyse the pattern. We can see that the output of List.fold_left is a
tuple. Besides, the second element is teh result of add function! It indicates
that we also need a tuple as base with the second to store temporary result! let
us figure out what the first element is. Since it is addition, we should be aware
that there is a very important parameter: carry! We might as well set carry in
base, which would yield convenience. Ok, the base is (0,[]) now. what would be
the list of input of f? We know for every step, we calculate every digit from 1s
to 10s and 100s, so we can use List.combine to create a tuple for every digit.
Besides, we need to reverse them because we would always calculate the smallest.
For each step, we just do the math and add some values into the result.
********************************************************************************
*)
let bigAdd l1 l2 =
  let add (l1, l2) =
    let f a x =
      match x with
      | (x1,x2) -> match a with
                   | (c, l) -> match l with
                               | [] -> ((c+x1+x2)/10,[(c+x1+x2)/10;c+x1+x2-(c+x1+x2)/10*10])
                               | h::t -> ((h+x1+x2)/10,[(h+x1+x2)/10;(h+x1+x2)-(h+x1+x2)/10*10]@t)
     in
    let base = (0, []) in
    let args = List.combine (List.rev l1) (List.rev l2) in
    let (_, res) = List.fold_left f base args in
      res
  in
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list
********************************************************************************
multiply a small number by a large number can be transformed as multiple additions.
So treat that small interger as a counter, we just do i times of addition. each
addition is bigAdd function.
********************************************************************************
*)
let rec mulByDigit i l =
  let rec helper cur_res cur_i =
    if cur_i=0 then cur_res else helper (bigAdd cur_res l) (cur_i-1)
    in helper [] i;;

(* bigMul : int list -> int list -> int list
********************************************************************************
This is actually a polynomial calculation. We calculate the first digit times a
big number, save it as temporary result. Then we times 10 with the temporary result
to indicate a shift to left in decimal calculation. Then we add another digit times
a big number to be the temporary reuslt again. At last, we would get the final
results.
********************************************************************************
*)
let bigMul l1 l2 =
  let f a x =  match a with
               | (l,r) ->match l with
                         | [] -> a
                         | h::t -> (t, bigAdd (mulByDigit 10 r) (mulByDigit h l2))
  in
  let base = (l1,[]) in
  let args = l2 in
  let (_, res) = List.fold_left f base args in
    res
