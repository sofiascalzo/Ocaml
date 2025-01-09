let parentesi_bilanciate lis = 
  
  let rec count_parentesi lis count =
    match lis with
    | x::lis'  -> if x = '(' then (count+1) + (count_parentesi lis' count) else if count > 0 then (count_parentesi lis' count) + (count-1) else -1
    | [] -> 0 

in 
if count_parentesi lis 0 <> 0 then false else true ;;



let lis = [')';'('; '('; ')'; '('; ')'; '('; ')'] ;;

let ris = parentesi_bilanciate lis ;;

print_endline (string_of_bool(ris)) ;;



