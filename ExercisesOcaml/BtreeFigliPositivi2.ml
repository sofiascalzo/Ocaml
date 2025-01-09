(*con predicato*)

type btree =
| Void
| Node of int * btree * btree


let positivo x = match x with
| Void -> false
| Node (i,_,_) -> i>0

let rec count bt positivo =
  match bt with
  | Void -> 0
  | Node (v, bl, br) -> if (positivo bl &&  positivo br) then (count bl positivo)+ (count br positivo) + 1 else (count bl positivo) + (count br positivo) ;;
                            

let bt = Node (3,
  Node (5,Void,Void),
  Node (-4,
    Node(6,Void,Void),
    Node(8,Void,Void)
)
)

let figlipositivi = count bt ;;

