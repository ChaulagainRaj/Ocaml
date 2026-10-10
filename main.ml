

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
| Sum of exp*exp 

type evT = 
| Int of int 
| Bool of bool 
| String of string 




(*--------------------------------------------------------------------------------------*)
(*QUESTO SIA IL NOSTRO ZONA DELLA MEMORIA IN CUI SCRIVIAMO IL NOSTRO COIDCE *)
(*--------------------------------------------------------------------------------------*)

(*ora generiamo il nostro primo espressione*)
let primo_p = EInt 5;;
let second_p = Sum (EInt 5 , EInt 8);;
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
              let Int v1 = eval e1 in 
              let Int v2 = eval e2 in 
              Int (v1+v2) 





(*--------------------------------------------------------------------------------------*)
(*ORA TESTIAMO IL NOSTRO MACCHINA ASTRATTA *)
(*--------------------------------------------------------------------------------------*)

let result = eval primo_p;;
let res = eval second_p;;





  