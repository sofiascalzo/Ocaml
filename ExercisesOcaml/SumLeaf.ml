type ntree = 
  | Node of int * ntree list
  | Leaf of int;;

let rec compact ntree =
  let isLeaf ntree =
    match ntree with
    | Leaf _ -> true
    | _ -> false
  in
  let rec sumLeaf lis =
    match lis with
    | Leaf n :: lis' -> n + sumLeaf lis'
    | [] ->  0
    | Node (_, _)::_ -> failwith "unexpected value"
  in
  match ntree with 
  | Node (n, ntree') -> 
      if List.for_all isLeaf ntree' then 
        Node (n,[Leaf (sumLeaf ntree')]) 
      else 
       Node (n, List.map compact ntree')
  | Leaf n -> Node (n,[Leaf n]) ;;


let ris1 = compact (Node(10,[Leaf 2; Leaf 3; Leaf 4])) ;;
let ris2 =compact (Node(10,[Leaf 2; Node (3, [Leaf 5; Leaf 3]); Leaf 4])) ;;
let ris3 =compact (Node(10,[])) ;;