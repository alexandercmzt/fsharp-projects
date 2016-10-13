module hw3

(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Alexander Chatron-Michaud, Id Number: 260611509 *) (* Edit this line. *)
(* This work is mine and mine alone*)

(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

(* For testing.  Uncomment if you want to use them. 
let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}
*)

(* This converts an RList to an ordinary list. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This may be useful.  You don't have to use it.*)
let cellToRList (c:Cell):RList = ref (Some c)

(* Example for testing. *)
let bigger(x:int, y:int) = (x > y)

let insert comp (item: int) (list: RList) =
  let rec helper comp (item: int) (list: RList) = (*We need a helper function so we can let the list point to the new solution after the operation*)
    match !list with
    | None -> cellToRList({data = item; next = ref None})
    | Some {data = d; next = l} -> if comp(item,d) then cellToRList({data=item; next= ref (!list)}) (*if item goes before, we add it to the front of the list*)
                                    else cellToRList({data = d; next = (helper comp item l)}) (*otherwise recursively add the first item in the list to the rest of the list with the item recursively added in*)
  list := !(helper comp item list) (*We have to update our original Rlist*)

(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance

let make_protected_account(opening_balance: int, password: string) =
  let balance = ref opening_balance
  fun (p, t: transaction) -> (*This was almost the same as the code looked at in class, the only change is first checking if the password is correct before allowing any operations*)
      if p = password then
        match t with
          | Withdraw(m) ->  if (!balance > m)
                            then
                              balance := !balance - m
                              printfn "The new balance is %i\n" !balance
                            else
                              printfn "Insufficient funds.\n"
          | Deposit(m) -> (balance := !balance + m; (printf "The new balance is %i\n" !balance))
          | CheckBalance -> (printf "The balance is %i\n" !balance)
      else printfn "Incorrect password\n"  

(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

exception EmptyTree

let bfIter f ltr =
  let todo = Queue<ListTree<'a>> ()
  let rec helper f (todo:Queue<ListTree<'a>>)= 
    if (not (todo.Count = 0)) then (*avoids trying to dequeue on empty queue*)
      let temp = todo.Dequeue()
      match temp with
      | (Node(n,[])) -> f n; helper f todo; () (*in this case we perform the function and then repeat helper on what is left in the queue*)
      | (Node(n,lt)) -> f n; List.iter (fun u -> todo.Enqueue(u)) lt; helper f todo; (*in this case we also have to add the children of the node into the queue.*)
  todo.Enqueue(ltr)
  helper f todo


 

    






        


        

      
