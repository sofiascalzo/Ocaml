let rec somme_liste lis1 lis2 =
  match (lis1, lis2) with
  | ([], []) -> [] 
  | (x :: lis1', y :: lis2') -> (x + y) :: somme_liste lis1' lis2'
  | (x :: lis1', []) -> []
  | ([], y :: lis2') -> []
;;

let print_list lst =
  print_endline ("[" ^ (String.concat "; " (List.map string_of_int lst)) ^ "]")
;;

let x = [3;4;5]
let y = [3;4;5]
let ris= somme_liste x y ;;

print_list ris ;;



