type colored_int =
| Black of int
| Red of int 



let rec swap_colors ct= 
  match ct with
  | [] -> []
  | Black n::ct' ->Red n:: swap_colors ct'
  | Red n::ct' -> Black n:: swap_colors ct'


let seq_len ct =
  let contaB acc lis =
  match lis with
  | [] -> 0
  | Black n-> acc+1
  | Red n-> 0
in let b = List.fold_right contaB 0 ct in 

  let contaR acc lis =
  match lis with
  | [] -> 0
  | Black n-> 0
  | Red n-> acc+1
in let r = List.fold_right contaR 0 ct

in if b > r then b else r


let coloredTree = [Black 10; Red 5; Red 2; Black 4; Black 1; Black 7; Red 2]

let ris = swap_colors coloredTree