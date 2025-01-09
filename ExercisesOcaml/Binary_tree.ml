type btree = 
| Node of int * btree * btree
| Leaaf of int

let rec profondita bt =
match bt with
| Leaaf -> 0
| (n, bl, br) -> if profondita bl  + 1 > profondita br + 1 then profondita bl  + 1 else profondita br + 1


let rec max_profondita bt_lis =
  match bt_lis with
  | []-> 0
  | bt:: bt_lis' -> if profondita bt > max_profondita bt_lis' then  profondita bt else max_profondita bt_lis'



let conta_max bt_lis =

  let rec conta bt_lis count = 
    | []->0
    | bt :: bt_lis' -> if profondita bt = max then (conta bt_lis')  + 1 else (conta bt_lis' )
     
    match bt_lis with
    | []->0
    | bt :: bt_lis' ->  if curr == max then conta bt_lis' bt else 


  let max= max_profondita bt_lis 
    in conta bt_lis' 0
  

