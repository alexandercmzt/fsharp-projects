(* Student name: Alexander Chatron-Michaud, Id Number: 260611509 *)

(* testing grounds *)

let ff (xx:float)  = (xx*xx) - 16.0

(* Question 1 *)

let deriv (f, dx:float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton (f, guess:float, tol:float, dx:float) =
 let prime = deriv(f, dx) (*prime is the f' function*)
 let (ftnv:float) = (guess - (f(guess)/prime(guess))) (*ftnv is x1*)
 if (f(ftnv) < tol && f(ftnv) > 0.0-tol) then ftnv else newton(f, ftnv, tol, dx) (*if f(ftnv) is within tolerance we can return, else we recursively calculate x1*)

(* Question 2 *)

type term = float * int
type poly = term list
exception EmptyList

let rec mtp(t:term, p:poly) :poly = 
 match (p,t) with
 | ([],(ff,ii)) -> raise EmptyList (*if p is empty we raise the exception*)
 | ([(ff,ii)],(ft,it)) -> [(ff*ft, ii+it)] (*if there is one element in the list (base case), we multiply the coefficients and add the exponents*)
 | ((ff,ii)::xs,(0.0,i)) -> [(0.0,0)] (*if we are multiplying by a term with a zero coefficient we give the empty polynomial*)
 | ((ff,ii)::xs,(ft,it)) -> (ff*ft, ii+it)::mtp(t, xs) (*if we have a term and a list, we multiply the current term in the list by the term and then do the rest of the list recursively*)

let rec atp(t:term, p:poly) :poly = 
 match (p,t) with
 | ([],(ff,ii)) -> raise EmptyList
 | ((ff,ii)::xs,(0.0,i)) -> p (*if we are adding 0, don't change anything*)
 | ([(ff,ii)],(ft,it)) -> (*base case*)
  if (ii = it) then 
   if ff + ft = 0 then [] (*if terms cancel eachother*)
   else [(ff + ft, ii)] (*if the terms are the same power, add coefficients*)
  else if (ii > it) then (ff,ii)::[(ft,it)] (*if power is less, add it after current term*)
  else t::p (*if power is greater, add it before current term*)
 | ((ff,ii)::xs,(ft,it)) -> (*recursive case*)
  if (ii = it) then 
   if ff + ft = 0 then xs (*if terms cancel eachother*)
   else (ff + ft, ii)::xs (*if the terms are the same power, add coefficients*)
  else if (ii > it) then (ff,ii)::atp(t,xs) (*if power is less, add it after current term*)
  else t::p (*if power is greater, add it before current term*)

let rec addpolys(p1:poly, p2:poly) :poly =
 match p1 with
 | [] -> raise EmptyList (*atp accounts for empty polynomials but not no terms to use*)
 | [(ff,ii)] -> atp((ff,ii),p2) (*base case, atp adds the term to p2*)
 | t::ts -> atp(t,addpolys(ts,p2)) (* add the term t to the polynomial with the recursively added ts*)

let rec multpolys(p1:poly, p2:poly) :poly =
 match p1 with
 | [] -> raise EmptyList (*mtp accounts for empty polynomials but not no terms to use*)
 | [(ff,ii)] -> mtp((ff,ii),p2) (*base case, mtp multiplies to p2 by the term*)
 | t::ts -> addpolys(mtp(t,p2), multpolys(ts,p2)) (*we take the sum of the p2 multiplied by each term*)

let rec exp(b:float, e:int) :float =
 match (b,e) with
 | (b,0) -> 1.0
 | (b,e) -> b*exp(b,e-1)

let evalterm v t :float =
 match t with
 | (ff,ii) -> ff*exp(v,ii)

let evalpoly(p:poly,v:float) :float = List.fold (fun acc t -> acc + evalterm v t) 0.0 p












