let rec fold_right f lis a =
  match lis with
  | []->a
  | x::lis' -> f x (fold_right f lis' a) ;;


let stringa_piu_lunga lis =
  let maggiore_tra_2 s1 s2 =
    if String.length s1 > String.length s2 then s1 else s2 
  in fold_right maggiore_tra_2 lis ""

let maggiore = stringa_piu_lunga ["ciao"; "aaaaaaaaa"; "bu"; "pippo"] ;;

print_endline (maggiore)