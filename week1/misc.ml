(* sumList : int list -> int
   *****************************************************************************
   sumList is a recursive function that adds the first element, namely the head,
   in a list to the result if not empty and call sumList for the rest of the
   list. If the list is empty, then simply add 0 and end the recursion.
   *****************************************************************************
*)
let rec sumList l =
match l with
| [] -> 0
| h::t -> h+(sumList t);;



let abs = fun a -> if a>0 then a else -a;;

(* appendEnd : 'a list -> 'a -> 'a list
   *****************************************************************************
   appendEnd is a function that adds an element to the end of a list. It works
   as follows:
   if the list is empty, then simply return the list that contains the element;
   else we extract its head and use :: operator to connect it to the list that
   waits to be completed. Here the list is a appendEnd function output with tail
   and that element as input.
   *****************************************************************************
*)
let rec appendEnd l a =
match l with
| [] -> [a]
| h::t -> h::(appendEnd t a)

(* digitsOfInt : int -> int list
   *****************************************************************************
   digitsOfInt is a recursive function. What it does is as follows:
   if the input number is 0, then no need to add an element into the list;
   else: calculate the last digit, and add it to the end of the list; meanwhile,
   remove the last digit from n and call digitsOfInt function for the new n.
   *****************************************************************************
 *)
let rec digitsOfInt n =
if n=0 then [] else appendEnd (digitsOfInt (n/10)) (n-n/10*10);;
(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)

let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
   *****************************************************************************
   additivePersistence is a function that returns the additive persistence of an
   integer. What it does is as follows:
   if the input integer is already a one-digit number, then it would not add one
   to the result;
   else: the input has more than one digit, then we need to sum up all its
   digits using function digits to split its digit and using function sumList to
   add all digits up. Finally, use this sum as input of a recursive call of
   additivePersistence function, and add 1 to the result.
   *****************************************************************************
*)
let rec additivePersistence n =
if n/10=0 then 0
else (additivePersistence (sumList (digits n)))+1

(* digitalRoot : int -> int
   *****************************************************************************
   digitalRoot is a function that returns the digital root of an integer. It
   works nearly the same with additivePersistence function, below is the logic:
   if the input is already a one-digit number, then return this value;
   else: the input has more than one digit, therefore we have to sum up its
   digits and use sum as input of a recursive call of digitalRoot function.
   *****************************************************************************
*)
let rec digitalRoot n =
if n/10=0 then n
else (digitalRoot (sumList (digits n)))

(* listReverse : 'a list -> 'a list
   *****************************************************************************
   listReverse is a function that reverses a list and output the reversed list.
   It works as follows:
   if the list is empty, then simply return [], no need to reverse at all
   else: there is at least one element in the list, so we extract the first
   element, namely the head, and put it at the end of the output list. For the
   rest part of the list, we call listReverse function again to form a recursion.
   *****************************************************************************
*)
let rec listReverse l =
match l with
| [] -> []
| h::t -> appendEnd (listReverse t) h

(* explode : string -> char list
 * (explode s) is the list of characters in the string s in the order in
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s =
  let rec _exp i =
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
   *****************************************************************************
   palindrome is a function to determine whether a string is palindromic or not.
   It works as follows:
   Get the original string, split it into a char list; and then, get the
   reversed version of the input string, split it into another char list; the
   last step is to compare these two char list. If they are the same, then it is
   a palindrome, else it is not.
   *****************************************************************************
*)
let palindrome w =
if (explode w)=listReverse (explode w) then true else false;;
