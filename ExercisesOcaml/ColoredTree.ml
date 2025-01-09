type 'a colored_tree =
  | Black of 'a
  | Red of 'a * 'a colored_tree * 'a colored_tree
  | Blue of 'a * 'a colored_tree * 'a colored_tree ;;

let raccogli t =
  
  let rec raccogli_Black t =
    match t with
    | Black v -> [v]
    | Red (_, f1, f2) -> (raccogli_Black f1) @ (raccogli_Black f2)
    | Blue (_, f1, f2) -> (raccogli_Black f1) @ (raccogli_Black f2)
  in
  
  let rec raccogli_Blue t =
    match t with
    | Black _ -> []
    | Red (_, f1, f2) -> (raccogli_Blue f1) @ (raccogli_Blue f2)
    | Blue (v, f1, f2) -> [v] @ (raccogli_Blue f1) @ (raccogli_Blue f2)
  in
  
  let rec raccogli_Red t =
    match t with
    | Black _ -> []
    | Red (v, f1, f2) -> [v] @ (raccogli_Red f1) @ (raccogli_Red f2)
    | Blue (_, f1, f2) -> (raccogli_Red f1) @ (raccogli_Red f2)
  in
  (raccogli_Black t, raccogli_Blue t, raccogli_Red t) ;;


let tree = 
  Red ("a",
    Blue ("b",
      Black "c",
      Red ("d", Black "e", Black "f")),
    Red ("g",
      Blue ("h", Black "i", Black "j"),
      Black "k")) ;;

(* Test della funzione *)
let ris = raccogli tree ;;