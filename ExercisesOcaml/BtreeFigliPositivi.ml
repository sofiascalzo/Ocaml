(*senza predicato*)

type btree =
| Void
| Node of int * btree * btree

let rec count bt =
  match bt with
  | Void -> 0
  | Node (v, bl, br) -> match bl with
                       | Void -> 0
                       | Node (vl, bll, brl) -> if vl > 0 then  match br with
                                                  | Void -> 0
                                                  | Node (vr, blr, brr) -> if v > 0 then 1 + (count bl) + (count br) else (count bl) + (count br)
                                                else (count bl) + (count br) ;;

let bt = Node (3,
  Node (5,Void,Void),
  Node (-4,
    Node(6,Void,Void),
    Node(8,Void,Void)
)
)

let figlipositivi = count bt ;;
