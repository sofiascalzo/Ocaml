type btree =
  | Void
  | Node of int * btree * btree

let flat t =
  (* Funzione ausiliaria per ottenere una lista di nodi con il livello *)
  let rec nodo_livello t liv =
    match t with
    | Void -> []
    | Node (n, f1, f2) ->
        (n, liv) :: (nodo_livello f1 (liv + 1)) @ (nodo_livello f2 (liv + 1))
  in

  (* Lista di nodi e livelli *)
  let lista_livelli = nodo_livello t 0 in

  (* Funzione per separare i nodi per livello *)
  let rec crea_lis lista_livelli lis0 lis1 lis2 lis3 =
    match lista_livelli with
    | (n, 0) :: lista_livelli' -> crea_lis lista_livelli' (n :: lis0) lis1 lis2 lis3
    | (n, 1) :: lista_livelli' -> crea_lis lista_livelli' lis0 (n :: lis1) lis2 lis3
    | (n, 2) :: lista_livelli' -> crea_lis lista_livelli' lis0 lis1 (n :: lis2) lis3
    | (n, 3) :: lista_livelli' -> crea_lis lista_livelli' lis0 lis1 lis2 (n :: lis3)
    | [] -> (List.rev lis0, List.rev lis1, List.rev lis2, List.rev lis3)
  in

  (* Chiamata alla funzione di creazione delle liste per livelli *)
  crea_lis lista_livelli [] [] [] []

(* Esempio di albero *)
let bt =
  Node (3,
    Node (5,
      Node (1, Void, Void),
      Void
    ),
    Node (-4,
      Node (6, Void, Void),
      Node (8, Void, Void)
    )
  )

(* Esecuzione della funzione flat sull'albero di esempio *)
let ris = flat bt;;
