type point_2d_3d =
| P2dim of float*float
| P3dim of float*float*float ;;
type 'a box =
| Empty
| Full of 'a ;;
type 'a colored_tree =
| Black of 'a
| Red of 'a*('a colored_tree)*('a colored_tree)
| Blue of 'a*('a colored_tree)*('a colored_tree) ;;

(Some 10)::(None)::[] ;;

(Full 10)::(Empty)::[] ;;

let x = P2dim (3. , 5.) in Full x;;

let rec f ct = match ct with
| Black n -> 0
| Red (n,ct1,ct2) -> n+(f ct1)+(f ct2)
| Blue (n,ct1,ct2) -> (f ct1)+(f ct2) ;;

let g p1 p2 = match p1,p2 with
| P2dim (x1,y1), P2dim (x2,y2) ->
Some (P2dim ((x1+.x2)/.2.,(y1+.y2)/.2.))
| P3dim (x1,y1,z1) , P3dim (x2,y2,z2) ->
Some (P3dim ((x1+.x2)/.2.,(y1+.y2)/.2.,(z1+.z2)/.2.))
| _,_ -> None ;;



type point_2d_3d = P2dim of float * float | P3dim of float * float * float

type 'a box = Empty | Full of 'a

type 'a colored_tree =
    Black of 'a
  | Red of 'a * 'a colored_tree * 'a colored_tree
  | Blue of 'a * 'a colored_tree * 'a colored_tree
- : int option list = [Some 10; None]
- : int box list = [Full 10; Empty]
- : point_2d_3d box = Full (P2dim (3., 5.))

val f : int colored_tree -> int = <fun>

val g : point_2d_3d -> point_2d_3d -> point_2d_3d option = <fun>