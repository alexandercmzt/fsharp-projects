module hw4

(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Alexander Chatron-Michaud, Id Number: 260611509 *) (* Edit this line. *)


type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau: typExp) : bool =
 match tau with
 | TypInt -> false
 | TypVar a -> if a = v then true else false
 | Arrow (a,b) -> if ((occurCheck v a) || (occurCheck v b)) then true else false
 | Lst a -> if occurCheck v a then true else false

(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  | TypInt -> TypInt
  | TypVar a -> if a = v then (if occurCheck a tau1 then failwith "Failed occurs check" else tau1) else TypVar a
  | Arrow (a,b) -> Arrow (substitute tau1 v a, substitute tau1 v b)
  | Lst a -> Lst(substitute tau1 v a)

let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold (fun acc (c,e) -> substitute e c acc) tau sigma
(* This is a one-line program *)

let unify (tau1: typExp) (tau2:typExp) : substitution =
 let rec helper (tau1: typExp) (tau2:typExp) (subst:substitution) :substitution =
  if tau1 = tau2 then subst else
  match (tau1,tau2) with
  | (TypInt, TypInt) -> subst
  | (TypVar t0, b) -> if occurCheck t0 b then failwith "Failed occurs check" else if List.exists (fun elem -> elem = (t0,b)) subst then subst 
                       else (t0,b)::subst
  | (b, TypVar t1) -> if occurCheck t1 b then failwith "Failed occurs check" else if List.exists (fun elem -> elem = (t1,b)) subst then subst 
                       else (t1,b)::subst
  | (Lst t0, Lst t1) -> helper t0 t1 subst
  | (Arrow (t0,t1), Arrow (t2,t3)) -> (helper t1 t3 (helper t0 t2 subst))
  | (_,_) -> failwith "Clash in principal type constructor"
 let subst = helper tau1 tau2 []
 let substr = List.rev subst
 if (applySubst subst tau1) = (applySubst subst tau2) then subst 
  else if (applySubst substr tau1) = (applySubst substr tau2) then substr
   else failwith "Not unifiable"


  
