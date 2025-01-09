
let p x = x > 5 ;;

let split lis p =
  let aux x (lis1, lis2) =
    if p x then (x :: lis1, lis2) else (lis1, x :: lis2)
  in
  List.fold_right aux lis ([], []) ;;

let l = [2; 3; 9; 2; 4; 8; 5; 3; 2; 1] ;;


let lis = split l p ;;
