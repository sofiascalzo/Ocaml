type punto = {x:float; y:float}
type rettangolo = { id:int; p1:punto; p2:punto};;

let base r = r.p2.x -. r.p1.x;;
let altezza r = r.p2.y -. r.p1.y;;
let area r = base r *. altezza r;;


let r1 = {id=123; p1={x=1.;y=1.}; p2={x=6.;y=2.}};;
let r2 = {id=44; p1={x=1.;y=2.}; p2={x=2.;y=3.}};;
let r3 = {id=332; p1={x=4.;y=2.}; p2={x=6.;y=4.}};;

area r1;; (* restituisce 5. *)
area r2;; (* restituisce 1. *)
area r3;; (* restituisce 4. *)

let  check_crescenti lis =
  let rec f rett_lis =
    match rett_lis with
    | rett1::rett2::rett_lis' -> if area rett1> area rett2 then f (rett2::rett_lis') else false 
    | [] | [_] -> true 
in  f lis;;
  

check_crescenti [r1; r2; r3];;
