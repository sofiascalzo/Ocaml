let rec duemax lis =
  match lis with
  | [] ->  None
  | x::[] -> Some (x,x)
  | x::y::[] -> if x > y then Some (x,y) else Some (y,x)
  | x::lis' -> match duemax lis' with
                | Some (a,y)  -> if x> a then Some (x,a) 
                            else
                                if x>y then Some (a,x)
                                else Some (a,y)
                 | None -> None ;;



let es1=duemax [2;3;1;5] 