let rec coppie_da_lista lis =
   let rec pair lis y =
    match lis with
    |x::lis' -> if x<>y then (x,y)::(pair lis' y)  else (pair lis' y)
    |[]-> []
   in
   List.map (fun x -> pair lis x) lis
  (*
  match lis with
  |x::lis-> pair lis x
  |[]-> pair [] 0 ;;*)



coppie_da_lista [1;2;3;4] ;; (*[(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)]*)