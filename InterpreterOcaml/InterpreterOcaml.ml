(* Identificatori *)
type ide = string;;

(* I tipi *)
type tname =  TInt | TBool | TString | TClosure | TRecClosure | TUnBound
              | TBiclosure (* bifun *)
              | Tintlist (* intlist *)

(* Abstract Expressions = espressioni nella sintassi astratta, 
   quanti e cosa ci si aspetta come argomenti nell'Albero di Sintassi Astratta *)
type exp = 
    | EInt of int
    | CstTrue 
    | CstFalse
    | EString of string
    | Den of ide
    (* Operatori binari da interi a interi*)
    | Sum of exp * exp
    | Diff of exp * exp
    | Prod of exp * exp
    | Div of exp * exp
    (* Operatori da interi a booleani *)
    | IsZero of exp
    | Eq of exp * exp
    | LessThan of exp * exp
    | GreaterThan of exp * exp
    (* Operatori su booleani *)
    | And of exp * exp
    | Or of exp * exp
    | Not of exp
    (* Controllo del flusso, funzioni *)
    | IfThenElse of exp * exp * exp
    | Let of ide * exp * exp
    | Letrec of ide * ide  * exp * exp
    | Fun of ide * exp
    | Apply of exp * exp
    (* bifun *)
    | Bifun of ide * ide * exp
    | Biapp of exp * exp * exp
    (* inlist *)
    | Intlist of int list
    | Cons of int * exp
    | Length of exp
    | Sumlist of exp
    | EmptyList
    (* IntCollection *)
    | EmptyCollection 
    | Add of exp * exp 
    | Remove of exp * exp 
    | Exists of exp * exp
    (* pairs *)
    | Pair of exp * exp
    | First of exp
    | Second of exp
    | Composeapply of exp * exp
    (* bifun *)
    | FunLim of ide * exp * exp * exp 
    (* multi insieme *)
    | EmptyMulti
    | RemoveMulti of exp * exp
    | Howmany of exp * exp
    | AddMulti of exp * exp

type evT =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ide * exp * evT env
  | RecClosure of ide * ide * exp * evT env
  | UnBound
  (* bifun *)
  | Biclosure of ide * ide * exp * evT env
  (* inlist *)
  | Tintlist of int list
  (* IntCollection *) 
  | IntCollection of int list
  (* pairs *)
  | Pair of evT * evT
  (* limfun *)
  | LimClosure of ide * int * int * exp * evT env
  (* Multiinsieme *)
  | Multiint of int list

and 'v env = ide -> 'v;;

(* Ambiente vuoto *)
let emptyenv = function x -> UnBound

(* Binding fra una stringa x e un valore primitivo evT *)
let bind (s: evT env) (x: ide) (v: evT) = 
    function (i: ide) -> if (i = x) then v else (s i)

(* Type Checking Funzione da evT a tname che associa a ogni valore il suo descrittore di tipo  *)
let getType (x: evT) : tname =
    match x with
    | Int(n) -> TInt
    | Bool(b) -> TBool
    | String(s) -> TString
    | Closure(i,e,en) -> TClosure
    | RecClosure(i,j,e,en) -> TRecClosure
    | UnBound -> TUnBound
    | Biclosure(i,j,e,en) -> TBiclosure
    | Tintlist(_) -> Tintlist
    | IntCollection(_) -> Tintlist
    | Pair(_,_) -> TClosure (* pairs non tipizzati separatamente *)
    | LimClosure(_,_,_,_,_) -> TClosure
    | Multiint(_) -> Tintlist

(* Type-checking *)
let typecheck ((x, y) : (tname * evT)) = 
    match x with
    | TInt -> (match y with Int(_) -> true | _ -> false)
    | TBool -> (match y with Bool(_) -> true | _ -> false)
    | TString -> (match y with String(_) -> true | _ -> false)
    | TClosure -> (match y with Closure(_,_,_) -> true | _ -> false)
    | TRecClosure -> (match y with RecClosure(_,_,_,_) -> true | _ -> false)
    | TUnBound -> (match y with UnBound -> true | _ -> false)
    | TBiclosure -> (match y with Biclosure(_,_,_,_) -> true | _ -> false)
    | Tintlist -> (match y with Tintlist(_) | IntCollection(_) -> true | _ -> false)

(* Errori a runtime *)
exception RuntimeError of string

(* Operazioni primitive *)
let is_zero(x) = match (typecheck(TInt,x),x) with
    | (true, Int(v)) -> Bool(v = 0)
    | (_, _) -> raise (RuntimeError "Wrong type")

let int_eq(x,y) =   
    match (typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Bool(v = w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let int_plus(x, y) = 
    match(typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v + w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let int_sub(x, y) = 
    match(typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v - w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let int_times(x, y) =
    match(typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v * w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let int_div(x, y) =
    match(typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> 
                    if w<>0 then Int(v / w)
                            else raise (RuntimeError "Division by zero")
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let less_than(x, y) = 
    match (typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Bool(v < w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let greater_than(x, y) = 
    match (typecheck(TInt,x), typecheck(TInt,y), x, y) with
    | (true, true, Int(v), Int(w)) -> Bool(v > w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let bool_and(x,y) = 
    match (typecheck(TBool,x), typecheck(TBool,y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v && w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let bool_or(x,y) = 
    match (typecheck(TBool,x), typecheck(TBool,y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v || w)
    | (_,_,_,_) -> raise (RuntimeError "Wrong type")

let bool_not(x) = 
    match (typecheck(TBool,x), x) with
    | (true, Bool(v)) -> Bool(not(v))
    | (_,_) -> raise (RuntimeError "Wrong type")

(* Funzioni ausiliarie per liste *)
let rec sumlist l =
    match l with
    | [] -> 0
    | x::xs -> x + sumlist xs

let rec length l =
    match l with
    | [] -> 0
    | _::xs -> 1 + length xs

(* IntCollection *)
let add el coll = el :: coll

let remove el coll = List.filter (fun x -> x <> el) coll

let exists p coll = List.exists p coll

(* Funzione eval completa *)
let rec eval (e:exp) (s:evT env) : evT = 
    match e with
    | EInt(n) -> Int(n)
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | EString(s) -> String(s)
    | Den(i) -> (s i)
    | Prod(e1,e2) -> int_times((eval e1 s), (eval e2 s))
    | Sum(e1, e2) -> int_plus((eval e1 s), (eval e2 s))
    | Diff(e1, e2) -> int_sub((eval e1 s), (eval e2 s))
    | Div(e1, e2) -> int_div((eval e1 s), (eval e2 s))
    | IsZero(e1) -> is_zero (eval e1 s)
    | Eq(e1, e2) -> int_eq ((eval e1 s), (eval e2 s))
    | LessThan(e1, e2) -> less_than((eval e1 s), (eval e2 s))
    | GreaterThan(e1, e2) -> greater_than((eval e1 s), (eval e2 s))
    | And(e1, e2) -> bool_and((eval e1 s), (eval e2 s))
    | Or(e1, e2) -> bool_or((eval e1 s), (eval e2 s))
    | Not(e1) -> bool_not(eval e1 s)
    | IfThenElse(cond, e1, e2) ->
        let g = eval cond s in
        (match (typecheck(TBool, g), g) with
        | (true, Bool(true)) -> eval e1 s
        | (true, Bool(false)) -> eval e2 s
        | (_, _) -> raise (RuntimeError "Non boolean condition in IfThenElse"))
    | Let(i, e, body) -> eval body (bind s i (eval e s))
    | Fun(arg, body) -> Closure(arg, body, s)
    | Apply(f, arg) ->
        let fclosure = eval f s in
        (match fclosure with
        | Closure(param, body, declenv) ->
            eval body (bind declenv param (eval arg s))
        | _ -> raise (RuntimeError "Not a function"))
    | Bifun(param1, param2, body) -> Biclosure(param1, param2, body, s)
    | Biapp(f, arg1, arg2) ->
        let bclosure = eval f s in
        (match bclosure with
        | Biclosure(param1, param2, body, declenv) ->
            let env1 = bind declenv param1 (eval arg1 s) in
            let env2 = bind env1 param2 (eval arg2 s) in
            eval body env2
        | _ -> raise (RuntimeError "Not a bifunction"))
    | _ -> raise (RuntimeError "Expression not implemented")

(* Esempi con alberi di sintassi astratta *)

(* Esempio 1: Somma *)
let example1 = Sum(EInt(5), EInt(3));;
(* Albero di sintassi astratta:
       Sum
      /   \
   EInt(5) EInt(3)
*)
eval example1 emptyenv;; (* Output atteso: Int(8) *)

(* Esempio 2: Condizionale *)
let example2 = IfThenElse(IsZero(EInt(0)), EInt(1), EInt(2));;
(* Albero di sintassi astratta:
          IfThenElse
          /    |    \
      IsZero EInt(1) EInt(2)
         |
       EInt(0)
*)
eval example2 emptyenv;; (* Output atteso: Int(1) *)

(* Esempio 3: Funzione e applicazione *)
let example3 = Let("f", Fun("x", Sum(Den("x"), EInt(10))), Apply(Den("f"), EInt(5)));;
(* Albero di sintassi astratta:
          Let
         / |  \
      "f" Fun Apply
           |     /    \
        "x"  Den("f") EInt(5)
            \
             Sum
            /   \
         Den("x") EInt(10)
*)
eval example3 emptyenv;; (* Output atteso: Int(15) *)

(* Esempio 4: Bifunzione *)
let example4 = Let("add", Bifun("x", "y", Sum(Den("x"), Den("y"))), Biapp(Den("add"), EInt(7), EInt(3)));;
(* Albero di sintassi astratta:
          Let
         / |   \
     "add" Bifun  Biapp
            /  \     /   \
          "x" "y" Den("add") EInt(7) EInt(3)
              \
               Sum
              /   \
           Den("x") Den("y")
*)
eval example4 emptyenv;; (* Output atteso: Int(10) *)

