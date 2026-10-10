

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
| Sum of exp*exp (*Operatori binari da interi a interi *)
| Sub of exp*exp 
| Mul of exp*exp
| Div of exp*exp
| Is_zero of exp
| Is_equal of exp*exp 

type evT = 
| Int of int 
| Bool of bool 
| String of string 


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









(*--------------------------------------------------------------------------------------*)
(*QUESTO SIA IL NOSTRO ZONA DELLA MEMORIA IN CUI SCRIVIAMO IL NOSTRO COIDCE *)
(*--------------------------------------------------------------------------------------*)

(*ora generiamo il nostro primo espressione*)
let primo_p = EInt 5;;
let second_p = Sum (EInt 5 , EInt 8);;
let sub_pro = Sub (EInt 10, EInt 3);;
let zero = Is_zero(EInt 0);;
let uaguale = Is_equal(EInt 5 , EInt 4);;
(*Dove stiamo scrivendo questo programma in quale zona del nostro macchina astratta : Risposta su quaderno*)

(*--------------------------------------------------------------------------------------*)
(*ORA VOGLIAMO IL NOSTRO CPU: EVAL : INTERPRETE *)
(*--------------------------------------------------------------------------------------*)
(*abbiamo incontrato il primo problema il tipo di varaibile esprimibile: soluzione evT*)

let rec eval (e:exp) : evT =
match e with 
| EInt n -> Int n 
| CstTrue -> Bool true
| CstFalse -> Bool false
| Sum (e1,e2) -> 
              let  v1 = eval e1 in 
              let  v2 = eval e2 in 
              int_plus v1 v2
| Sub (e1,e2) -> 
                let v1 = eval e1 in 
                let v2 = eval e2 in 
                int_minus v1 v2 
| Is_zero(e) -> 
                let v = eval e in 
                bool_iszero v
| Is_equal(e1,e2)-> 
                   let v1 = eval e1 in 
                   let v2 = eval e2 in 
                   bool_isequal v1 v2 




(*--------------------------------------------------------------------------------------*)
(*ORA TESTIAMO IL NOSTRO MACCHINA ASTRATTA *)
(*--------------------------------------------------------------------------------------*)

let result = eval primo_p;;
let res = eval second_p;;

let minus = eval sub_pro;;

let xero = eval zero;;

let equal = eval uaguale;;







  