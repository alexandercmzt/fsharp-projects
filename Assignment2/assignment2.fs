module hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Alexander Chatron-Michaud, Id Number: 260611509 *)
(* This work is mine and mine alone. If there are any concerns or issues with
   my program or its functionality, please email me at achatr@cim.mcgill.ca *)


(* Question 1 *)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(* val deriv : f:(float -> float) * dx:float -> x:float -> float *)

let rec newton (f, guess:float, tol:float, dx:float) =
 let prime = deriv(f, dx) (*prime is the f' function*)
 let (ftnv:float) = (guess - (f(guess)/prime(guess))) (*ftnv is x1*)
 if (f(ftnv) < tol && f(ftnv) > 0.0-tol) then ftnv else newton(f, ftnv, tol, dx) (*if f(ftnv) is within tolerance we can return, else we recursively calculate x1*)
(* val newton : f:(float -> float) * guess:float * tol:float * dx:float -> float *)

(* For testing 
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c);;
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001);;
*)

(* Question 2 *)

type term = float * int
type poly = term list

exception EmptyList

(* Multiply a term by a polynomial. *)
let rec mtp(t:term, p:poly) :poly = 
 match (p,t) with
 | ([],(ff,ii)) -> raise EmptyList (*if p is empty we raise the exception*)
 | ([(ff,ii)],(ft,it)) -> [(ff*ft, ii+it)] (*if there is one element in the list (base case), we multiply the coefficients and add the exponents*)
 | ((ff,ii)::xs,(0.0,i)) -> [(0.0,0)] (*if we are multiplying by a term with a zero coefficient we give the empty polynomial*)
 | ((ff,ii)::xs,(ft,it)) -> (ff*ft, ii+it)::mtp(t, xs) (*if we have a term and a list, we multiply the current term in the list by the term and then do the rest of the list recursively*)
(* val mtp : t:term * p:poly -> poly *)

(* Add a term to a polynomial. *)
let rec atp(t:term, p:poly) :poly = 
 match (p,t) with
 | ([],(ff,ii)) -> raise EmptyList
 | ((ff,ii)::xs,(0.0,i)) -> p (*if we are adding 0, don't change anything*)
 | ([(ff,ii)],(ft,it)) -> (*base case*)
  if (ii = it) then 
   if ff + ft = 0.0 then [(0.0,0)] (*if terms cancel eachother*)
   else [(ff + ft, ii)] (*if the terms are the same power, add coefficients*)
  else if (ii > it) then (ff,ii)::[(ft,it)] (*if power is less, add it after current term*)
  else t::p (*if power is greater, add it before current term*)
 | ((ff,ii)::xs,(ft,it)) -> (*recursive case*)
  if (ii = it) then 
   if ff + ft = 0.0 then xs (*if terms cancel eachother*)
   else (ff + ft, ii)::xs (*if the terms are the same power, add coefficients*)
  else if (ii > it) then (ff,ii)::atp(t,xs) (*if power is less, add it after current term*)
  else t::p (*if power is greater, add it before current term*)
(* val atp : t:term * p:poly -> poly *)

(* Add two polynomials.  The result must be properly represented. This means you
cannot have more than one term with the same exponent, you should not have a
term with a zero coefficient, except when the whole polynomial is zero and the
terms should be decreasing order of exponents.   Thus, for example,
5.2 x^7 - 3.8 x^4 +2.0 x - 1729.0 should be represented as
[(5.2,7);(-3.8,4);(2.0,1);(-1729.0,0)] *)

let rec addpolys(p1:poly, p2:poly) :poly =
 match p1 with
 | [] -> raise EmptyList (*atp accounts for empty polynomials but not no terms to use*)
 | [(ff,ii)] -> atp((ff,ii),p2) (*base case, atp adds the term to p2*)
 | t::ts -> atp(t,addpolys(ts,p2)) (* add the term t to the polynomial with the recursively added ts*)
(* val addpolys : p1:poly * p2:poly -> poly *)

(* Multiply two polynomials.  All the remarks above apply here too. Raise an
exception if one of the polynomials is the empty list. *)
let rec multpolys(p1:poly, p2:poly) :poly =
 match p1 with
 | [] -> raise EmptyList (*mtp accounts for empty polynomials but not no terms to use*)
 | [(ff,ii)] -> mtp((ff,ii),p2) (*base case, mtp multiplies to p2 by the term*)
 | t::ts -> addpolys(mtp(t,p2), multpolys(ts,p2)) (*we take the sum of the p2 multiplied by each term*)
(* val multpolys : p1:poly * p2:poly -> poly *)

(* This is the tail-recursive version of Russian peasant exponentiation.  I have
done it for you.  You will need it for the next question.  *)
let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

(* Here is how you evaluate a term. *)
let evalterm (v:float) ((c,e):term) = if (e=0) then c else c * exp(v,e)

(* Evaluate a polynomial viewed as a function of the indeterminate.  Use the function
above and List.fold and List.map and a dynamically created function for a one-line
answer.  *)
let evalpoly(p:poly,v:float) :float = List.fold (fun acc t -> acc + evalterm v t) 0.0 p
(* val evalpoly : p:poly * v:float -> float *)

(* Compute the derivative of a polynomial as a symbolic representation.  Do NOT use
deriv defined above.  I want the answer to be a polynomial represented as a list.
I have done a couple of lines so you can see how to raise an exception.  *)
let rec diff (p:poly):poly = 
  match p with
    | [] -> raise EmptyList (* if list is empty *)
    | [(ff,ii)] -> (* base case *)
     if (ii = 0) then [(0.0,0)] (* if there is one constant, its derivative is zero *)
     else [(ff*float ii,ii-1)] (* otherwise we follow d/dx rules for exponents *)
    | (ff,ii)::ts -> 
     if (ii = 0) then diff(ts) (* skip term which becomes zero*)
     else (ff*float ii,ii-1)::diff(ts) (* otherwise we follow d/dx rules for exponents *)
(*  val diff : p:poly -> poly *)
    
(* Question 3 *)
(* Most of these functions are only one or two lines.  One of them, the longest is
about 5 lines.  However, they require some thought.  They are short because I used
the Set library functions wherever I could.  I especially found Set.fold useful. *)

type Country = string;;
type Chart = Set<Country*Country>;;
type Colour = Set<Country>;;
type Colouring = Set<Colour>;;

(* This is how you tell that two countries are neghbours.  It requires a chart.*)
let areNeighbours ct1 ct2 chart =
  Set.contains (ct1,ct2) chart || Set.contains (ct2,ct1) chart;;
(* val areNeighbours :
  ct1:'a -> ct2:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
  *)

(* The colour col can be extended by the country ct when they are no neighbours
according to chart.*)
  
let canBeExtBy col ct chart :bool=
 Set.forall (fun x -> not (areNeighbours ct x chart)) col (* we just check if there are neighbors for anything in col *)
(*
   val canBeExtBy :
  col:Set<'a> -> ct:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
*)

(* Here you have to extend a colouring by a fixed country. *)
let rec extColouring (chart: Chart) (colours : Colouring) (country : Country) =
  if Set.isEmpty colours then Set.add (Set.add country Set.empty) colours
  else 
   let col = Set.minElement colours (* take a color *)
   if (canBeExtBy col country chart) then (* if we can add the country to col *)
    let modcolours = Set.remove col colours (*remove the col from the set*)
    Set.add (Set.add country col) modcolours (* add col back with country added*)
   else Set.add col (extColouring chart (Set.remove col colours) country) (* if we cant add the country, remove this country and search through the rest, but take the union later to add it back *)
(*
val extColouring :
  chart:Chart -> colours:Colouring -> country:Country -> Set<Set<Country>>
*)

(* This collects the names of the countries in the chart.  A good place
to use Set.fold *) 
let countriesInChart (chart : Chart) = 
 Set.fold (fun (acc:Colour) (ct1,ct2) -> Set.add ct2 (Set.add ct1 acc)) Set.empty chart 
(* val countriesInChart : chart:Chart -> Set<Country> *)

(* Here is the final function.  It is also most conveniently done with Set.fold *)
let colourTheCountries (chart: Chart)  :Colouring=
 Set.fold (fun (acc:Colouring) (country:Country) -> (extColouring chart acc country)) (Set.empty) (countriesInChart(chart))
(* val colourTheCountries : chart:Chart -> Colouring *)

(* Question 4 *)

(* These functions are a bit longer but easier to code than Q3.  It is very similar
to the evaluator that I showed in class.  However I have used the Option type so that
the program gracefully returns None if no value is found.  This can be preferred to
raising an exception in some situations.  Learn option types from the web.  *)

type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree;;

type Bindings = (string * int) list;;

(* The bindings are stored in a list rather than a BST for simplicity.  The
list is sorted by name, which is a string. *)
let rec lookup(name:string, env: Bindings) = 
 match env with
 | [] -> None
 | (ss,ii)::bs -> if (name = ss) then Some ii else lookup(name,bs)
(* val lookup : name:string * env:Bindings -> int option *)

(* Insert a new binding.  If the name is already there then the new binding should
be put in front of it so that the lookup finds the latest binding.  *)
let rec insert(name:string, value: int, b: Bindings) = 
 match b with
 | [] -> [(name,value)]
 | (ss,ii)::bs ->
  if name > ss then insert(name,value,b)
  else (name,value)::b
(* val insert : name:string * value:int * b:Bindings -> (string * int) list*)

(* The recursive evaluator.  You have to match on the exp.  If a variable is not
found return None.  If you are applying an operator to None and something else the
answer is None.  This leads to a program of about 20 lines but it is conceptually
very easy.  *)

let rec eval(exp:Exptree, env:Bindings) = 
 match exp with 
 | Const n -> Some n
 | Var v -> lookup(v,env)
 | Add (t1,t2) -> 
   match eval(t1,env), eval(t2,env) with (*unboxing*)
   | t ,None -> None
   | None, t -> None
   | Some t1, Some t2 -> Some (t1 + t2)
 | Mul (t1,t2) -> 
   match eval(t1,env), eval(t2,env) with (*unboxing*)
   | t ,None -> None
   | None, t -> None
   | Some t1, Some t2 -> Some (t1 * t2)
(* val eval : exp:Exptree * env:Bindings -> int option  *)


