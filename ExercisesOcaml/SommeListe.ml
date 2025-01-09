(*con fold-right*)

let sommeliste lis1 lis2 =
  let p lis1 acc = 
    match acc with 
    | ([],[]) -> []
    | (x::lis1,y::lis2) -> x+y::[]
in 
  List.fold_right p lis1 []