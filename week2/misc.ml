(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* assoc : 'a * 'b * ('b * 'a) list -> 'a
********************************************************************************
What we do in the code is the following:
if l is empty, then it means we have searched the whole list of l and no findings.
therefore, we should output d;
else: l is not empty, then we take out the first element, check if the key is
equal to a, if so, we output the corresponding value, and if not, we continue the
search using the rest of l.
********************************************************************************
*)
let rec assoc (d,k,l) =
  match l with
    | [] -> d
    | (a,b)::t -> if a = k then b
                           else assoc (d,k,t)

(* removeDuplicates : 'a list -> 'a list
********************************************************************************
What the code does is the following:
First reverse the list using List.rev. Then we create a helper function in order
to realize this function in tail recursion fashion. What helper function does is
the following:
it maintains two list, one list "seen" is used to save distinct elements that we
have visited, the other list "rest" is used to save the remaining list we haven't
visited yet. We first take one element from rest to see if there's any. If not,
meaning we have reached the end of the list, mission completed, so we output seen
as result. If there is at least an element, we take first element out, check if
the value of this element has been in seen, if so, we do nothing, if not, we put
this value into seen, and do the next helper function.
********************************************************************************
*)
let removeDuplicates l =
  let rec helper (seen,rest) =
      match rest with
      | [] -> seen
      | h::t ->
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in
	  helper (seen',rest')
  in
      List.rev (helper ([],l))


(* wwhile : 'a -> 'a * bool) * 'a -> 'a
********************************************************************************
What the code does is the following:
We examine the pair of current answers of f b, if the returning boolean value is
true, then as demanded, we should repeat the function again, so call wwhile(f,
value) where value is the value of f b. If the returning boolean is already false,
we output value immediately.
********************************************************************************
*)
let rec wwhile (f,b) =
  let (value, bool_val) = f b in
    if bool_val = true then wwhile (f, value)
    else value;;

(* fixpoint : 'a -> 'a) * 'a -> 'a
********************************************************************************
fixpoint is a function that repeatedly updates b with f(b) until b=f(b) and then
returns b. Since it utilizes wwhile function, then we need to setisfy the input
type of wwhile, which requires a function 'a -> 'a * bool.Our goal is to create
this function.
So use let... in ... to name a local function. It takes as input an integer, so
we directly pass b as its input. As output, we need a tuple including an integer
and a boolean value. Due to these two elements in the tuple has something to do
with the result of f b, we might as well name it as a local variable as well.
So the code presents as follows.
********************************************************************************
*)
let fixpoint (f,b) = wwhile (let ff b = let t = f b in (t, t!=b) in ff, b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) =
  if low>high
  then ()
  else let _ = f low in ffor (low+1,high,f)

(************** Add Testing Code Here ***************)
