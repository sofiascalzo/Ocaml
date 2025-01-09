type valuta =
| Euro of float
| Dollari of float;;


let inEuro tasso moneta = 
  match moneta with 
  | Euro n -> n 
  | Dollari n -> n *. tasso

let rec somma_valute tasso v_lis =
  match v_lis with
  | [] -> 0.
  | Euro n::v_lis' -> (somma_valute tasso v_lis') +. n
  | Dollari n::v_lis' -> ( somma_valute tasso v_lis') +. inEuro tasso (Dollari n)


let rec separa_valute v_lis =
  match v_lis with
  | [] -> ([],[]) 
  | Euro n::v_lis' -> let (euros, dollars ) = separa_valute v_lis' in (n::euros, dollars)
  | Dollari n::v_lis' -> let (euros, dollars )= separa_valute v_lis' in (euros, n::dollars)



(* Esempio *)

let valute = [
  Euro 50.0;
  Dollari 100.0;
  Euro 20.0;
  Dollari 50.0
]

let _ =
  let tasso = 0.9 in
  let somma = somma_valute tasso valute in
  Printf.printf "Somma in euro: %.2f\n" somma;

  let (euros, dollars) = separa_valute valute in
  Printf.printf "Euro: [%s]\n" (String.concat "; " (List.map string_of_float euros));
  Printf.printf "Dollari: [%s]\n" (String.concat "; " (List.map string_of_float dollars))
