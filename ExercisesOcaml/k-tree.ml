type ntree =
| Node of int * ntree list



let rec contains n kt =
  match kt with 
  | Node (v, []) -> v = n 
  | Node (v, k_lis) -> if v = n then true 
                  else List.exists (contains n) k_lis ;;

(* addchild prende due interi n e m e un albero t, e restituisce un albero ottenuto aggiungendo ad ogni nodo etichettato con n in t, un figlio foglia etichettato con m. 
Se nessun nodo `e etichettato con n, il metodo addchild restituisce un albero identico a t.*)

let rec addchild n m kt =  
  match kt with
  | Node (v, k_lis) ->  if n = v then  Node (v, Node(m, []) :: List.map (addchild n m)k_lis )
                        else Node (v, List.map (addchild n m) k_lis );;


(* Esempio di utilizzo *)
let tree = 
  Node (1, [
    Node (2, []);
    Node (3, [Node (4, []); Node (5, [])])
  ]);;

let _ =
  print_endline (string_of_bool (contains 3 tree));; (* true *)
  print_endline (string_of_bool (contains 6 tree));;(* false *)                                                          



let tree_with_child = addchild 3 7 tree;;

let _ =
  print_endline (string_of_bool (contains 3 tree)); (* true *)
  print_endline (string_of_bool (contains 6 tree)); (* true *)
  (* Stampa l'albero aggiornato *)
  let rec print_tree kt depth =
    match kt with
    | Node (v, k_lis) ->
        Printf.printf "%sNode(%d)\n" (String.make depth ' ') v;
        List.iter (fun child -> print_tree child (depth + 2)) k_lis
  in
  print_tree tree_with_child 0;;