let rec somma_positivi lista =
match lista with
| x::lista' -> if x > 0 then  x+(somma_positivi lista') else somma_positivi lista'
| [] -> 0 ;;

let x= [3;0;-1;-2;4] ;;
let ris=somma_positivi x;;

print_endline(string_of_int(ris))