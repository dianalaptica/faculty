(* Exercitiul 1*)
(* Scrieti o functie care primeste ca parametru un numar intreg (pozitiv sau negativ!) si returneaza suma cifrelor acestuia. *)
(* clasic *)

let rec sumcif n = match ( abs n ) with
  | 0 -> 0
  | n -> (n mod 10) + sumcif ( n / 10)

let a = sumcif 10
let b = sumcif 134
let c = sumcif 2876
let d = sumcif 99999 
let e = sumcif 123456789 
let f = sumcif (-7) (* nu stiu exact daca aici ar trebui sa returneze 7 sau -7 *)
let g = sumcif (-165) ;; (* de fapt nu stiu exact ce ar trebui sa returneze pentru orice numar negativ asa ca am presupus ca facem abstractie de minus *)

(* rezultate

val sumcif : int -> int = <fun>
val a : int = 1
val b : int = 8
val c : int = 23
val d : int = 45
val e : int = 45
val f : int = 7
val g : int = 12

*)

(* tail-recursive *)

let sumcif_tail n = 
  let rec functie aux n = match ( abs n ) with
    | 0 -> aux
    | n -> functie ( aux + ( n mod 10) ) ( n / 10)
  in functie 0 n

let a = sumcif_tail 10
let b = sumcif_tail 134
let c = sumcif_tail 2876
let d = sumcif_tail 99999  
let e = sumcif_tail 123456789 
let f = sumcif_tail (-7)
let g = sumcif_tail (-165) ;;

(* rezultate

val sumcif_tail : int -> int = <fun>
val a : int = 1
val b : int = 8
val c : int = 23
val d : int = 45
val e : int = 45
val f : int = 7
val g : int = 12

*)


(* Exercitiul 2 *)
(* Scrieti o functie care primeste ca parametru un numar intreg (pozitiv sau negativ!) si returneaza numarul de cifre pare ale acestuia. *)
(* clasic *)

let rec nrcifpar = function
  | cifrap when ( cifrap < 10 && ( cifrap mod 2) = 0 ) -> 1
  | 0 -> 0 
  | n -> if ( (n mod 10) mod 2) = 0 then ( 1 + nrcifpar ( n /10 ) )
         else nrcifpar ( n / 10 )

let a = nrcifpar 0
let b = nrcifpar 134
let c = nrcifpar 2876
let d = nrcifpar 99999  
let e = nrcifpar 123456789 
let f = nrcifpar (-2)
let g = nrcifpar (-165) 
let h = nrcifpar 2488846
let i = nrcifpar (-20) ;;

(* rezultate

val nrcifpar : int -> int = <fun>
val a : int = 1
val b : int = 1
val c : int = 3
val d : int = 0
val e : int = 4
val f : int = 1
val g : int = 1
val h : int = 7
val i : int = 2

*)

(* tail-recursive *)

let nrcifpar_tail n = 
  let rec f aux n = match n with
  | 0 -> aux
  | n -> if (n mod 10) mod 2 = 0 then f (aux + 1) (n / 10)
         else f aux (n / 10)
  in f 0 n

let a = nrcifpar_tail 10
let b = nrcifpar_tail 134
let c = nrcifpar_tail 2876
let d = nrcifpar_tail 99999  
let e = nrcifpar_tail 123456789 
let f = nrcifpar_tail (-2)
let g = nrcifpar_tail (-165) 
let h = nrcifpar_tail 2488846
let i = nrcifpar_tail (-20) ;;

(* rezultate

val nrcifpar_tail : int -> int = <fun>
val a : int = 1
val b : int = 1
val c : int = 3
val d : int = 0
val e : int = 4
val f : int = 1
val g : int = 1
val h : int = 7
val i : int = 2

*)


(* Exercitiul 3 *)
(* a. Scrieți o funcție care numara cate cifre de 1 are descompunerea în baza 2 a unui număr intreg pozitiv dat ca parametru. *)

let rec cif1 = function
  | 0 -> 0
  | n -> if ( n mod 2 ) = 1 then 1 + cif1 ( n / 2 )
         else cif1 ( n / 2 )

let a = cif1 123 
let b = cif1 7
let c = cif1 2598
let d = cif1 44444 ;;

(* rezultate

val cif1 : int -> int = <fun>
val a : int = 6
val b : int = 3
val c : int = 5
val d : int = 9

*)

(* b. Generalizați funcția pentru descompunerea în orice bază dată ca parametru. *)

let rec cif1b n b = match n with
  | 0 -> 0
  | n -> if ( n mod b ) = 1 then 1 + cif1b ( n / b ) b
         else cif1b ( n / b ) b

let a = cif1b 123 2
let b = cif1b 7 3 (* 7 in baza 3 e 21 *)
let c = cif1b 2598 4 (* 2598 in baza 4 e 220212 *)
let d = cif1b 44444 7 (* 44444 in baza 7 e 243401 *) ;;

(* rezultate

val cif1b : int -> int -> int = <fun>
val a : int = 6
val b : int = 1
val c : int = 1
val d : int = 1

*)

(* c. Generalizati functia astfel incat sa poata numara orice cifra data ca parametru. *)

let rec cif1bc n b c = match n with
  | 0 -> 0
  | n -> if ( n mod b ) = c then 1 + cif1bc ( n / b ) b c
         else cif1bc ( n / b ) b c

let a = cif1bc 123 2 1
let b = cif1bc 7 3 2 (* 7 in baza 3 e 21 *)
let c = cif1bc 2598 4 2 (* 2598 in baza 4 e 220212 *)
let d = cif1bc 44444 7  4 (* 44444 in baza 7 e 243401 *) 
let e = cif1bc 2765 8 5 (* 2765 in baza 8 e 5315*) ;;

(* rezultate

val cif1bc : int -> int -> int -> int = <fun>
val a : int = 6
val b : int = 1
val c : int = 4
val d : int = 2
val e : int = 2

*)