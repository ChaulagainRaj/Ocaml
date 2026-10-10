

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

type evT = 
| Int of int 
| Bool of bool 
| String of string 



(*DEFINIAMO DI NUOVO FUNZIONE ausiliaria QUESTO VOLTA UTILIZZANDO TYPECHECK*)

let int_plus (x,y) = 
match(typecheck(TInt(x),x), typecheck(TInt(y),y),x,y) with
| (true,true, Int(v),Int(w)) -> Int(v+w)
| (_,_,_,_) -> raise ( RuntimeError "Wrong type")

let bool_isequal (x,y) = 
match (typecheck(TInt(x),x), typecheck(TInt(y),y),x,y) with
|(true,true, Int(v),Int(w))-> Bool(v=w)
|(_,_,_,_)-> raise (RuntimeError "Wrong type")

(*ora definiamo il nuovo modo di controllare i tipi *)


(*--------------------------------------------------------------------------------------*)
(* TYPE CHECK DINAMICO *)
(*--------------------------------------------------------------------------------------*)


type tname = 
| TInt 
| TBool
| TString 

(*definiamo la funzione che prende il type*)
let gettype (x:evT):tname = 
match x with 
        | Int(n)-> TInt
        | Bool(b) -> TBool
        | String(s)-> TString

let typecheck ((x,y):(tname*evT))= 
match x with 
| TInt -> (match y with 
           | Int(u)-> true
           | _-> false)
| TBool -> (match y with 
            | Bool(u)-> true
            | _-> false)
| TString -> (match y with 
            | String(u)-> true
            | _-> false)            
                  







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




(*--------------------------------------------------------------------------------------*)
(*ORA TESTIAMO IL NOSTRO MACCHINA ASTRATTA *)
(*--------------------------------------------------------------------------------------*)

let result = eval primo_p emptyenv;;
let res = eval second_p emptyenv;;



let risultato = eval prog emptyenv    (* Restituisce Int 12 *)





  