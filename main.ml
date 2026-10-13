

(* Sintassi astratta : il codice sorgente: vogliamo scrivere il nostro linguaggio di programmazione MINIOCaml utilizando il linguaggio
programmazione OCAML*)


(*--------------------------------------------------------------------------------------*)
(*1: Sintassi Astratta : IL Codice sorgente: dice cosa possiamo scrivere sintatticamente*)
(*---------------------------MEMORIA PROGRAMMA: DISCO-----------------------------------*)
type ide = string;; (*il primo tipo che si chiama ide che e' string*)

type exp = 
| EInt of int 
| CstTrue 
| CstFalse 
| EString of string
| Den of ide 
| Sum of exp*exp (*Operatori binari da interi a interi *)
| Sub of exp*exp 
| Mul of exp*exp
| Div of exp*exp
| Is_zero of exp
| Is_equal of exp*exp 
| Let of ide*exp*exp
| Fun of ide*exp 
| Apply of ide *exp 

type evT = 
| Int of int 
| Bool of bool 
| String of string 
| Closure of ide * exp * env
(* Closure sia il tipo restituito dalla funzione : nome della funzione 
                                                 : il corpo della funzione 
                                                 : ambiente in cui definito la funzione*)




(*
(*definiamo il funzione ausiliaria per typechecking *)
let int_plus (v1:evT) (v2:evT):evT = 
        match (v1,v2) with 
        |(Int n1, Int n2) -> Int(n1+n2)
        |_-> failwith "Runtime Errore: Tipi incompatibile";;
let int_minus (v1:evT) (v2:evT):evT = 
        match (v1,v2) with 
        | (Int n1, Int n2)-> Int(n1-n2)
        |_-> failwith"Runtime Errore : Tipi incompatibile";;
let bool_iszero (v:evT):evT = 
        match v with
        | Int 0 -> Bool true
        |_-> Bool false
let bool_isequal (v1:evT) (v2:evT):evT = 
        match (v1,v2) with 
        | (Int n1, Int n2)-> Bool (n1=n2)
        |_-> failwith"Runtime Errore : Tipi incompatibile";;
*)




(*--------------------------------------------------------------------------------------*)
(* TYPE CHECK DINAMICO *)
(*--------------------------------------------------------------------------------------*)


type tname = 
| TInt 
| TBool
| TString 
| TClosure 

(*definiamo la funzione che prende il type*)
let gettype (x:evT):tname = 
match x with 
        | Int(n)-> TInt
        | Bool(b) -> TBool
        | String(s)-> TString
        | Closure(f)-> TClosure

let typecheck (t,typedescriptor:tname*evT):bool= 
match t with 
| TInt -> (match typedescriptor with 
           | Int(u)-> true
           | _-> false)
| TBool -> (match typedescriptor with 
            | Bool(u)-> true
            | _-> false)
| TString -> (match typedescriptor with 
            | String(u)-> true
            | _-> false)            
                  



(*--------------------------------------------------------------------------------------*)
(* OPERAZIONE PRIMITIVE*)
(*--------------------------------------------------------------------------------------*)


(*DEFINIAMO DI NUOVO FUNZIONE ausiliaria QUESTO VOLTA UTILIZZANDO TYPECHECK*)

let int_plus v1 v2= 
match(typecheck(TInt,v1), typecheck(TInt,v2),v1,v2) with
| (true,true, Int (a),Int(b)) -> Int(a+b)
| (_,_,_,_) -> failwith  "Wrong type"

let bool_isequal x y = 
match (typecheck(TInt,x), typecheck(TInt,y),x,y) with
|(true,true, Int(v),Int(w))-> Bool(v=w)
|(_,_,_,_)-> failwith  "Wrong type"

let int_minus(v1:evT)(v2:evT):evT = 
match (typecheck(TInt,v1),typecheck(TInt,v2),v1,v2) with
|(true,true,Int(a),Int(b))-> Int(a-b)
|(_,_,_,_)-> failwith  "Wrong type"


let bool_iszero (v) = 
match (typecheck(TInt,v),v) with 
|(true,Int(0))-> Bool true  
|(true, Int(_)) -> Bool false
| (_,_)-> failwith "Wrong type"



(*ora definiamo il nuovo modo di controllare i tipi *)


(*--------------------------------------------------------------------------------------*)
(*IL NOSTRO RAM:AMBIENTE IN CUI VEDIAMO TUTTI CAMBIAMENTO DEI VALORI DURANTE ESECUZIONE *)
(*--------------------------------------------------------------------------------------*)


type env = (string*evT)list;;
let emptyenv = [("x", Int 10)];;

let bind (s:env) (x:string) (v:evT):env = (x,v)::s;;

let rec lookup (s:env) (x:string) = 
        match s with 
        | []-> failwith"Not Found"
        | (y,v)::rest -> if (y=x) then v 
                        else lookup rest x






(*--------------------------------------------------------------------------------------*)
(*QUESTO SIA IL NOSTRO ZONA DELLA MEMORIA IN CUI SCRIVIAMO IL NOSTRO COIDCE *)
(*--------------------------------------------------------------------------------------*)

(*ora generiamo il nostro primo espressione*)
let primo_p = EInt 5;;
let second_p = Sum (EInt 5 , EInt 8);;
let sub_pro = Sub (EInt 10, EInt 3);;
let zero = Is_zero(EInt 0);;
let uaguale = Is_equal(EInt 5 , EInt 4);;
let p_den = Den "x";;


let prog = 
  Let ("x", 
       Sum (EInt 5, EInt 5),          (* e1: valuta a Int 10 (un evT) *)
       Sum (Den "x", EInt 2)          (* e2: usa Den "x" che recupera Int 10 *)
  )

let P_funzione = Fun ("x", Sum(Den("x"),EInt 1));


(*Dove stiamo scrivendo questo programma in quale zona del nostro macchina astratta : Risposta su quaderno*)

(*--------------------------------------------------------------------------------------*)
(*ORA VOGLIAMO IL NOSTRO CPU: EVAL : INTERPRETE *)
(*--------------------------------------------------------------------------------------*)
(*abbiamo incontrato il primo problema il tipo di varaibile esprimibile: soluzione evT*)

let rec eval (e:exp) (s:env) : evT =
match e with 
| EInt n -> Int n 
| CstTrue -> Bool true
| CstFalse -> Bool false
| Sum (e1,e2) -> 
              let  v1 = eval e1 s in 
              let  v2 = eval e2 s in 
              int_plus v1 v2
| Sub (e1,e2) -> 
                let v1 = eval e1 s in 
                let v2 = eval e2 s in 
                int_minus v1 v2 
| Is_zero(e) -> 
                let v = eval e s in 
                bool_iszero v
| Is_equal(e1,e2)-> 
                   let v1 = eval e1 s in 
                   let v2 = eval e2  s in 
                   bool_isequal v1 v2 
| Den id -> lookup s id   
| Let (x,e1,e2) ->      
                let v1 = eval e1 emptyenv in (*guardare la semantica*) (* 1. eval restituisce un valore evT *)
                let env_esteso = bind s x v1 in (* 2. Leghiamo la stringa x al valore v1 *)
                eval e2 env_esteso  (* 3. Valutiamo e2 con il nuovo ambiente *)

(*funzione *)
| Fun (arg , ebody) -> Closure(arg, ebody, s )
| Apply (  f ,argattuale) ->
                (* recuperiamo il f_closure ovvero il tipo di ritorno della exp Fun*)
                let f_closure = eval f s in 
                match f_closure with 
                (*proviamo il match con il tipo di ritorno : Closure *)
                | Closure(argformale , ebody , fdecenv) -> 
                (*valutiamo argomento attuale e scriviamo in aev *)
                let aev = eval argattuale s in
                (*ora legiamo con il funzione bind parametro formale e aev: valutato argattuale*)
                let env_extend = bind  fdecenv argformale aev  in
                (*ora valutiamo il corpo della funzione in ambiente env_esteso : scoping statico *)
                eval ebody env_extend 
                |_-> failwith "NO match"
| Apply(_,_) -> failwith "No higher order funzione"

(*
        |Apply (Den(f),eArg) ->
        (* Valutiamo f per ottenere la chiusura *)
        let fclosure = eval f s in
        (* Valutiamo il parametro attuale subito (Call-by-value) *)
        let arg = eval eArg s in
        match fclosure with
        | Closure (x, ebody , dec_env)
        (* Estendiamo l'ambiente della decisione (dec_env) legando x al valore del parametro attuale *)
        let body_env = bind s x arg_val in
        (*scoping DINAMICO*)
        (* Valutiamo il corpo della funzione nell'ambiente esteso *)
        eval body body_env
        | _ -> failwith "Runtime Error: Tentativo di applicare un valore che non è una funzione"
*)
                









(*--------------------------------------------------------------------------------------*)
(*ORA TESTIAMO IL NOSTRO MACCHINA ASTRATTA *)
let result = eval primo_p emptyenv;;
let res = eval second_p emptyenv;;
let risultato = eval prog emptyenv    (* Restituisce Int 12 *)
(*--------------------------------------------------------------------------------------*)






  