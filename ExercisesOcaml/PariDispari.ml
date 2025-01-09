let rec pairs lis =
  match lis with
  | [] -> []
  | x::[] -> []
  | x::y::lis' -> (x,y):: ( pairs lis') ;;

let lis1 = [1;2;3;4;5;6;7;8;9] ;;

let ris = pairs lis1 ;;


let rec split lis =
  match lis with
  | [] -> ([],[])
  | x::lis' -> match x with | (i,j)-> let ris = split lis' in match ris with | (list1,list2) ->  (i::list1, j::list2) ;;

let ris2 = split ris ;;


let rec paridispari lis =
  match lis with
| [] -> ([],[])
| x::[]-> (x::[], [])
| x::y::lis' -> let ris =  paridispari lis' in match ris with |(lis1,lis2) -> (x::lis1, y::lis2) ;;

let ris3 = paridispari lis1 ;;