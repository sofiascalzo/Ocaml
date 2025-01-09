let print_list lst =
    print_endline ("[" ^ (String.concat "; " (List.map string_of_int lst)) ^ "]")
  ;;


(* funzione delta *)

let calcola_delta a b c =
    let delta = b*.b - 4.0*.a*.c
    in 
    ((-.b + sqrt delta) / (2.*a)), ((-.b - sqrt delta) / (2.*a)) ;;
  

(* implementazone @ *)

let rec append lis1 lis2 =
    match lis1 with
    | []      -> []
    | x::lis' -> x::(append lis' lis2) ;;


(* implementazione rev *)

let rec rev lis =
    match lis with
    | [] -> []
    | x::lis' -> (rev lis')@[x] ;;


(* ACCUMULATORE*)
(*piu` efficente in quanto permette di fa eseguire uno record di attivazione er ogni chiamata*)
(* implementazione di rev con accumulatore *)

let rev lis =
    let rec rev_acc lis acc =
        match lis with
        | []  -> acc
        | x::lis' -> rev_acc lis' x::acc
    in rev_acc lis [] ;;


(* FUNZIONI HIGH ORDER SU LISTE *)
(* se la funzione e` di tipo se condizione allora true / false*)
(* alora posso prendere in input una funzione con con la condizine da rispettare in modo da poterla usare in molti piu casi*)
(* esempi*)

let contiene_0 lis =
    match lis with
    | [] -> false
    | x::lis' -> if x==0 then true else contiene_0 lis' ;;

let contine_positivo lis =
    match lis with
    | [] -> false
    | x::lis' -> if x>=0 then true else contiene_positivo lis' ;;

let contiene_pari lis =
    match lis with
    | [] -> false
    | x::lis' -> if x mod 2 = 0 then true else contiene_pari lis' ;;

(* possono essere con un unica funzione che prende il predicato*)

let contiene p lis =
    match lis with
    | [] -> false
    | x::lis' -> if p x then true else contiene_pari p lis' ;;

let contiene_0        = contiene (fun x -> x = 0 ) lis'
let contiene_positivo     = contiene (fun x -> x >= 0 ) lis'
let contiene_pari   = contiene (fun x -> xmod 2 = 0 ) lis'

(* CHIUSURE DEL PREDICATO *)

let uguale x y = x = y ;;
let contiene_el x lis = contiene (uguale p) lis ;; 

(* implementazione di funzioni su liste che esistono in libreria List con metodo high-order*)
(* implementazione forall *)

let rec forall p lis=
    match lis with
    | [] -> true
    | x::lis' -> if p x then forall p lis' else false ;;


(* implementazione filter *)

let rec filter p lis=
    match lis with
    | [] -> []
    | x::lis' -> if p x then x::(filter p lis') else filter p lis' ;;


(* implementazione map*)

let rec map p lis=
    match lis with
    | [] -> []
    | x::lis' -> (p x)::map p lis' ;;



(*FOLD-Right*)
(* si usa nelle funzioni in cui mi serve sapere anche il risultato degli elementi calcolati prima *)
(* p predicato, lis argomento, a risultato parziale*)

let rec fold_right p lis a =
    match lis with
    | [] -> a
    | x::lis' -> p x (fold_right p lis' a)

let somma lis = fold_right (+) lis 0 ;;
let concat s1 s2 = fold_right (^) lis "" ;;

(*FOLD-Left*)
(*  *)

let rec fold_left p lis a =
    match lis with
    | [] -> a
    | x::lis' -> fold_left f lis' (f a x)




























































































































  
  
(* funzione delta con parametro funzione op*)
  
let calcola_delta a b c =
  let delta = b*.b - 4.0*.a*.c
  in 
  let calcola_x op
    ( op (-.b + sqrt delta) / (2.*a))
  in (calcola_ x (+.), calcola_x (-.))
  

(*definizioni di nuovi tipi unione, usare la maiuscola in Txt  Num è obbligatorio*)
(*x e y saranno due variabili di tipo numero_testo*)

type numero_test=
    | Txt of string
    | Num of int ;;

let x= Txt "34";;
let y= Num ;;



(*definizione di tipi ricorsivi, utile per creare alberi*)
type lista_di_int=
    | Nil
    | Elem of int * lista_di_int

Nil

Elem (5, Nil)

Elem (5, Nil, (3, Nil))



(*le foglie sono interi, gli altri nodi hanno un valore interi e due sottoalberi*)
type albero_bin =
    | Node of int *albero_bin*albero_bin
    | Foglia pf int ;;

let n1 = Foglia 4;;
let n2 = Foglia 2;;
let n3 = Nodo (3,n1,n2) ;;



(*espressioni aritmetiche su interi espressa come una tripla ricorsiva (UMin è il segno -)*)
type op= Add | Sub | Mul | Div | Mod ;;

type exp = 
    | Val of int
    | Op of op*exp*exp
    | UMin of exp ;;

let exp1 = Op ( Sub, (Op ( Mul, Val 3, Val 7)), Val 5) ;;
let exp2 = Op ( Mul, Umin (Val 3), (Op ( Sub, Val 7 , Val 5));;


(*stampa espressioni*)
(*se è un valore ritorno il valore, se è un'operazione apro parentesi e la stampo come stringa, se è un operazione unaria la stampo come stringa con le parentesi*)

let symbol o = 
    match o with
    | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod" 
    let rec to_string e =
    match e with
    | Val n -> string_of_int n
    | Op (o,e1,e2) ->  "("^ (to_string e1) ^ (symbol o) ^ (to_string) ")"
    | Umin e' -> "(-"^ (to_string e')^")" ;;



(*valutazioni espressioni*)

let rec eval e =
    match e with
        | Val n->n1
        | Op (o,e1,e2) ->
            match o with
            | Add -> ( eval e1) + (eval e2)
            | Sub -> ( eval e1) - (eval e2)
            | Mul -> ( eval e1) * (eval e2)
            | Div -> ( eval e1) / (eval e2)
            | Mod -> ( eval e1) mod (eval e2)
        |  Umin e' -> ( eval e' ) ;;


(*alberi k-ari*)
(*anziche una coppia di gli ho una lista di alberi quindi 'alberi list', c'è il rischio che la llista sia vuota e si confonde con la foglia*)
(*allora posso usare la funzione contains che mi dice se è presente *)

type albero =
        | Nodo of int*lbero list
        | Foglia of int ;;

(* rierca di un numero nell'albero senza uso di funzioni di libreria*)
let rec contains n t =
    match t with
        | Foglia m -> m=n (* t è foglia m?  se m = n allora ho il risultato*)
        | Nodo (n, lis)  -> if m=n then true else (*ne n non è il primo elemento di lis vado a cercare dentro la lista ricorsivamente (mi serve una nuova funzione ricorsiva)*)

                                            let rec f l =
                                                match lis with 
                                                | [] -> false (*caso base, di sicuro n non è nella lista vuota*)
                                                | t' :: lis' if contains n t' than true (*t' è il sottoalbero quindi chimo contains per t' mentre per lis' uso la funzione f di ricerca nella lista*)
                                                             else f l'

                                            in f lis ;;

let prova = Nodo (3, [Fogia, 2; Nodo (4, [Foglia 5; Folgia 6])]) ;;

contains 2 prova ;;


(*ricerca di un elemento all'interno del albero con funzioni di libria *) 
let contains2 n t = 
    match t with
        | Foglia m -> m=n
        | Nodo (m,lis) ->if m=n than true
                        else List.exists (contains2 n) lis;;
                        (*else List.exists (fun t' -> contains2 n t') lis;;*) 

let prova = Nodo (3, [Fogia, 2; Nodo (4, [Foglia 5; Folgia 6])]) ;;
contains2 2 prova ;;
                        

(**)
(**)
(**)
(**)