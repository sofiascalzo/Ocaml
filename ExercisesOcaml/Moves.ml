type point = 
  | Init of int * int
  | Up of int*point 
  | Down of int*point
  | Left of int*point
  | Right of int*point 


let rec eval exp =
  match exp with
  | Init (x,y)   -> (x,y)
  | Up (y1, p) -> let (x, y) = eval p in (x, y + y1)                                        
  | Down (y1, p) -> let (x, y) = eval p in (x, y - y1)
  | Left (x1, p)   ->  let (x,y) = eval p in (x-x1, y)
  | Right  (x1, p) -> let (x, y) = eval p in (x+x1, y) ;;

let exp = Down (2, Left (4, Up (3, Init (3, 2)))) ;;

let ris = eval exp ;;
  