(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alexander Chatron-Michaud, Id Number: 260611509 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  *)

(* Question 1 *) (* Do not edit this line. *)

(* val sumlist : l:float list -> float *)
let rec sumlist (l: float list) =
 match l with
 | [] -> 0.0
 | x::xs -> x + sumlist xs

(* val squarelist : l:float list -> float list *)
let rec squarelist (l: float list) =
 match l with
 | [] -> []
 | x::xs -> (x*x) :: squarelist xs

(* val mean : l:float list -> float *)
let mean (l: float list) =
 let rec len lz =
  match lz with
  | [] -> 0.0
  | x::xs -> 1.0 + len xs
 (sumlist l)/(len l) 

(* val mean_diffs : l:float list -> float list *)
let mean_diffs (l: float list) =
 let m = mean l
 let rec diffz lz = 
  match lz with
  | [] -> []
  | x::xs -> (x-m) :: diffz xs
 diffz l

(* val variance : l:float list -> float *)
let variance (l: float list) = 
 mean (squarelist (mean_diffs l))

(* End of question 1 *) (* Do not edit this line. *)



(* Question 2 *) (* Do not edit this line. *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof (a,l) =
 match l with
 | [] -> false
 | x::xs -> if (x = a) then true else memberof (a,xs)

(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove (a,l) =
 match l with
 | [] -> []
 | x::xs -> if (x=a) then remove(a,xs) else x::remove(a,xs)

(* End of question 2 *) (* Do not edit this line *)



(* Question 3 *) (* Do not edit this line *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l =
 match l with
 | [] -> []
 | x::xs -> if (memberof(x,xs)) then x::isolate(remove(x,xs)) else x::isolate(xs)

(* End of question 3 *) (* Do not edit this line *)



(* Question 4 *) (* Do not edit this line *)

(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common (a,b) =
 match a with
 | [] -> []
 | x::xs -> if (memberof(x,b)) then x::(common(xs,b)) else (common(xs,b))

(* End of question 4 *) (* Do not edit this line *)




(* Question 5 *) (* Do not edit this line *)

(* val split : l:'a list -> 'a list * 'a list *)
let split l =
 let rec half (l,l1,l2) =
  match l with 
  | [] -> (l1,l2)
  | [x] -> (x::l1,l2)
  | x::y::xs -> half(xs,x::l1,y::l2)
 half(l,[],[])
 
(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge (l1,l2)=
 match (l1,l2) with
 | ([], y) -> y
 | (x, []) -> x
 | (x::xs, y::ys) -> if x < y then x::merge(xs,y::ys) else y::merge(x::xs,ys)

(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l =
 match l with
 | [] -> []
 | [l] -> [l]
 | ls -> let (l1,l2) = split(l)
         merge(mergesort(l1),mergesort(l2)) 
 

(* End of question 5 *) (* Do not edit this line *)

