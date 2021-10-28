let functie lst = List.filter (fun a -> let (x, y) = a in x<>0 && y<>0) lst;;
let s = functie [(0,2); (3, 0); (7,7)];;

let lst = [(9., 0.); (0., 2.); (1., 2.); ((-5.), 3.); (5., 6.); (0.,(-7.))];;

let cond a = 
  let (x, y) = a in
  sqrt (x*.x +. y*.y) ;;

let dist lst = List.map cond lst
let dist2 lst = List.map (fun (x,y) -> sqrt (x*.x +. y*.y) ) lst ;;

let a = dist lst;;
let b = dist2 lst ;;

(* 1) returneaza lista tuturor persoanelor nascute dupa 2000 si varsta lor 
   [  ("Ana", 15); ("Oana", 16); ("Bogdan", 20); ];;

   2) returneaza numele si anul nasterii al celei mai tinere pe
*)

let persoane = [
  ("Ana", 2006);
  ("Andrei", 1997);
  ("Bianca", 1996);
  ("Oana", 2005);
  ("Bogdan", 2001);
  ("George", 1985)
];;

let varsta lst = lst |> List.filter (fun (nume, an) -> 2021-an <= 21) |> List.map (fun (nume, an) -> (nume, 2021-an) );;

let a = varsta persoane;;
let tanar lst = List.fold_left ( fun anmax (nume , an) -> max anmax an ) 1900 lst;;

 let rez = tanar persoane;;
