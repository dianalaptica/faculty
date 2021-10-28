(* Exercitiul 1 *)

let minmax x y z = ( min x (min y z), max x (max y z); );;

minmax 6 2 9;;
minmax 4. 4.1 7.9;;

 (* rezultate

  minmax 6 2 9;;
- : int * int = (2, 9)
# minmax 4. 4.1 7.9;;
- : float * float = (4., 7.9)
#

 *)


 (* Exercitiul 2 *)

 open Printf
 let ecgr2 a b c = 
  if b*b-4*a*c >= 0 then (
    printf "x1 =  %f\n" ( ( float_of_int (-b) -. sqrt (float_of_int( b*b-4*a*c )) ) /. 2.*.(float_of_int a) );
    printf "x2 =  %f\n" ( ( float_of_int (-b) +. sqrt (float_of_int( b*b-4*a*c )) ) /. 2.*.(float_of_int a) )
  ) else printf "nu exista solutii reale\n";;
 
ecgr2 1 2 3;;
ecgr2 1 3 2;;

(* rezultate

  ecgr2 1 2 3;;
nu exista solutii reale
- : unit = ()
# ecgr2 1 3 2;;
x1 =  -2.000000
x2 =  -1.000000
- : unit = ()
#

*)

(* ALTA VARIANTA *)
let functie a b c =
  let delta = b *. b -. 4. *. a *. c in 
  let a2 = 2. *. a in
  if delta >= 0. then
    let rad = sqrt delta in
    printf "x1 = %f\n x2 = %f\n" ( (-. b +. rad) /. a2) ( (-. b -. rad) /. a2)
  else printf "nu exista solutii reale\n";;

functie 1. 3. 2.;;

(* Exercitiul 3 *)

let an a =
  if ( (a mod 4 = 0 ) && ( a mod 100 <> 0 ) ) || ( a mod 400 = 0 )then true
  else false;;

let an1 a = a mod 400 = 0 || (a mod 4 = 0  &&  a mod 100 <> 0 );;

an 2002;;
an 2020;;
an 2000;;

(* rezultate

  an 2002;;
- : bool = false
# an 2020;;
- : bool = true
# an 2000;;
- : bool = true
#

*)
   

(* Exercitiul 4 *)
open Printf 
let id x y = if x = y then true else false;;
let dist x y z = (
  if id x y then (
    if id y z then print_string "toate argumentele sunt egale\n"
    else print_string "argumentele 1 si 2 sunt egale\n")
  else (
    if id y z then print_string "argumentele 2 si 3 sunt egale\n"
    else if id x z then print_string "argumntele 1 si 3 sunt egale\n"
         else print_string "toate argumentele sunt distincte\n"
  )
);;
  
dist 3 3 3;;
dist 3 3 2;;
dist 3 2 2;;
dist 3 2 3;;
dist 1 2 3;;

(* rezultate

  dist 3 3 3;;
toate argumentele sunt egale
- : unit = ()
# dist 3 3 2;;
argumentele 1 si 2 sunt egale
- : unit = ()
# dist 3 2 2;;
argumentele 2 si 3 sunt egale
- : unit = ()
# dist 3 2 3;;
argumntele 1 si 3 sunt egale
- : unit = ()
# dist 1 2 3;;
toate argumentele sunt distincte
- : unit = ()
#

*)


(* Exercitiul 5 *)

let mi x y z =  min x (min y z);;
let ma x y z = max x (max y z);;
let med x y z = (
  if (mi x y z ) + (ma x y z) = x + y then z 
  else 
    if  (mi x y z ) + (ma x y z) = x + z then y
    else x;   
);;  

med 1 3 7;;
med 8 5 9;;
med 10 4 2;;
med 3 13 5;;

(* rezultate

  med 1 3 7;;
- : int = 3
# med 8 5 9;;
- : int = 8
# med 10 4 2;;
- : int = 4
# med 3 13 5;;
- : int = 5
#

*)


(* Exercitiul 6 *)
(* a) *)

let f x = x + 1;;
let g x = x*2;;
let h x = f x + g x;;

h 0;;
h 3;;
h 7;;

(* rezultate

  h 0;;
- : int = 1
# h 3;;
- : int = 10
# h 7;;
- : int = 22
#

*)

(* b) *)
let k f g x op = op (f x) (g x);;

k f g 2 ( + );;
k f g 2 ( * );;