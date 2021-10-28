(* Problema 1 *)

let taylor x n = 
  let rec factorial n = match n with
    | 0 -> 1
    | _ -> n * factorial (n-1)
  in
  let putere = x ** (float_of_int n) in
  let fact = float_of_int ( factorial (n) ) in
  let rec calcul putere fact n = 
    if n = 0 then 1. 
    else ( putere /. fact ) +. calcul ( putere /. x) (fact /. float_of_int n) (n-1) 
  in calcul putere fact n

let a = taylor 1. 20
let b = taylor 2. 20
let c = taylor 3. 20 ;;

(* rezultate 

val taylor : float -> int -> float = <fun>
val a : float = 2.7182818284590455
val b : float = 7.3890560989306042
val c : float = 20.085536922950844

*)



(* Problema 2 *)

let lenght lst = 
  let rec aux nr lst = match lst with
    | [] -> nr
    | _::t -> aux ( nr+1 ) t
  in aux 0 lst

let a = lenght [1; 2; 3; 4]
let b = lenght [3.14; 4.; 7.9; 18.77; 91.]
let c = lenght ["aa"; "bb"] ;;

(* rezultate 

val lenght : 'a list -> int = <fun>
val a : int = 4
val b : int = 5
val c : int = 2

*)



(* Problema 3 *)

let rec nth_element lst n = match n with
  | t when t < 0 -> failwith " lista nu poate avea un numar negativ de elemente "
  | t when t > List.length lst -> failwith " numarul e prea mare "
  | 1 -> List.hd lst
  | _ -> nth_element (List.tl lst) (n-1) ;;

let a = nth_element [1; 5; 7; 4; 2] 3 ;;
let b = nth_element [2.; 5.6; 3.14;] 1 ;;
let c = nth_element ["aa"; "bb"; "cc"; "d"; "e"] (-4) ;;
let d = nth_element [5; 8; 13; 35] 5 ;;

(* rezultate 

let a = nth_element [1; 5; 7; 4; 2] 3 ;;
val a : int = 7
# let b = nth_element [2.; 5.6; 3.14;] 1 ;;
val b : float = 2.
# let c = nth_element ["aa"; "bb"; "cc"; "d"; "e"] (-4) ;;
Exception: Failure " lista nu poate avea un numar negativ de elemente ".
# let d = nth_element [5; 8; 13; 35] 5 ;;
Exception: Failure " numarul e prea mare ".

*)



(* Problema 4 *)

let fibo_list n =
  if n < 0 then failwith " sirul nu poate avea un numar negativ de termeni "
  else 
  let rec fibo_rec lst n a b c = match n with
    | 0 -> lst
    | _ -> fibo_rec ( a :: lst ) (n-1) b c (c+b)
  in List.rev (fibo_rec [] n 1 1 2) ;;

let a = fibo_list 2 ;;
let b = fibo_list 8 ;;
let c = fibo_list (-2) ;;

(* rezultate 

  let a = fibo_list 6 ;;
val a : int list = [1; 1; 2; 3; 5; 8]
# let b = fibo_list 8 ;;
val b : int list = [1; 1; 2; 3; 5; 8; 13; 21]
# let c = fibo_list (-2) ;;
Exception: Failure " sirul nu poate avea un numar negativ de termeni ".

*)



(* Problema 5 *)

let numar_nou lst =  
  let rec aux lst n p = match lst with
  | [] -> n
  | _  -> aux (List.tl lst) ( n + (List.hd lst)*p) ( p*10 )
  in aux (List.rev lst) 0 1 ;;

let a = numar_nou [3; 6 ; 9] ;;
let b = numar_nou [7; 3; 0; 1; 8] ;;

(* rezultate

let a = numar_nou [3; 6 ; 9] ;;
val a : int = 369
# let b = numar_nou [7; 3; 0; 1; 8] ;;
val b : int = 73018
#

*)
