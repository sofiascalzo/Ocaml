let rec fold_right f lis a =
  match lis with
  | []->a
  | x:: lis' -> f x (fold_right f lis' a) ;;


let massimo_minimo lis =
  let max x mass =
    if x > mass then x else mass
  in
  let min x minn =
    if x < minn then x else minn
  in 
    ( fold_right max lis 0 ,fold_right min lis 999 ) ;;
  

let lista = [2; 4; 6; 8; 0] ;;
let max_min = massimo_minimo lista ;; 

let () =
  Printf.printf "Massimo e Minimo: (%d, %d)\n" (fst max_min) (snd max_min)