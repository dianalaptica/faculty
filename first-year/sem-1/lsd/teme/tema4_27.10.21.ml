 let jud_pop = [
  ("Alba", 342376); 
  ("Arad", 430629); 
  ("Arges", 612431); 
  ("Bacau", 616168); 
  ("Bihor", 575398); 
  ("Bistrita-Nasaud", 286225); 
  ("Botosani", 412626); 
  ("Brasov", 549217); 
  ("Braila", 321212); 
  ("Buzau", 451069); 
  ("Caras-Severin", 295579); 
  ("Calarasi", 306691); 
  ("Cluj", 691106); 
  ("Constanta", 684082); 
  ("Covasna", 210177); 
  ("Dambovita", 518745); 
  ("Dolj", 660544); 
  ("Galati", 536167);
  ("Giurgiu", 281422); 
  ("Gorj", 341594); 
  ("Harghita", 310867); 
  ("Hunedoara", 418565); 
  ("Ialomita", 274148); 
  ("Iasi", 772348);
  ("Ilfov", 388738); 
  ("Maramures", 478659); 
  ("Mehedinti", 265390); 
  ("Mures", 550846); 
  ("Neamt", 470766); 
  ("Olt", 436400);
  ("Prahova", 762886); 
  ("Satu Mare", 344360); 
  ("Salaj", 224384); 
  ("Sibiu", 397322);
  ("Suceava", 634810); 
  ("Teleorman", 380123); 
  ("Timis", 683540); 
  ("Tulcea", 213083); 
  ("Vaslui", 395499); 
  ("Valcea", 371714); 
  ("Vrancea", 340310)
];;


(* Functia 1 *)

let numar_judete lst = List.fold_left ( fun suma el -> suma + 1) 0 lst ;;

let a = numar_judete jud_pop ;;

(* rezultat

  let a = numar_judete jud_pop ;;
val a : int = 41

*)

(* Functia 2 *)

let modifica_populatie nume x lst = List.map ( fun (a, b) -> if a = nume then (a, b+x) else (a, b) ) lst;;

let b = modifica_populatie "Alba" 1 jud_pop ;;
let c = modifica_populatie "Cluj" (-691106) jud_pop ;;

(* rezultate

  let b = modifica_populatie "Alba" 1 jud_pop ;;
val b : (string * int) list =
  [("Alba", 342377); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168);
   ("Bihor", 575398); ("Bistrita-Nasaud", 286225); ("Botosani", 412626);
   ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069);
   ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106);
   ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745);
   ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422);
   ("Gorj", 341594); ("Harghita", 310867); ("Hunedoara", 418565);
   ("Ialomita", 274148); ("Iasi", 772348); ("Ilfov", 388738);
   ("Maramures", 478659); ("Mehedinti", 265390); ("Mures", 550846);
   ("Neamt", 470766); ("Olt", 436400); ("Prahova", 762886);
   ("Satu Mare", 344360); ("Salaj", 224384); ("Sibiu", 397322);
   ("Suceava", 634810); ("Teleorman", 380123); ("Timis", 683540);
   ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714);
   ("Vrancea", 340310)]
# let c = modifica_populatie "Cluj" (-691106) jud_pop ;;
val c : (string * int) list =
  [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168);
   ("Bihor", 575398); ("Bistrita-Nasaud", 286225); ("Botosani", 412626);
   ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069);
   ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 0);
   ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745);
   ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422);
   ("Gorj", 341594); ("Harghita", 310867); ("Hunedoara", 418565);
   ("Ialomita", 274148); ("Iasi", 772348); ("Ilfov", 388738);
   ("Maramures", 478659); ("Mehedinti", 265390); ("Mures", 550846);
   ("Neamt", 470766); ("Olt", 436400); ("Prahova", 762886);
   ("Satu Mare", 344360); ("Salaj", 224384); ("Sibiu", 397322);
   ("Suceava", 634810); ("Teleorman", 380123); ("Timis", 683540);
   ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714);
   ("Vrancea", 340310)]

*)


(* Functia 3 *)

let media_populatie lst =
  let total_jud = List.fold_left ( fun suma el -> suma + 1) 0 lst in
  let total_pop = List.fold_left ( fun suma (x,y) -> suma + y) 0 lst in
  total_pop / total_jud ;;

let c = media_populatie jud_pop ;;

(* rezultat

let c = media_populatie jud_pop ;;
val c : int = 444834

*)


(* Functia 4 *)

let statistica nume lst = 
  let pop = List.fold_left ( fun sum (x, y) -> if x = nume then sum + y else sum + 0) 0 lst in 
  let partial = List.filter ( fun (x, y) -> y > pop ) lst in
  List.fold_left (fun vid (x, y) -> x :: vid) [] partial ;;

let a = statistica "Cluj" jud_pop ;;
let b = statistica "Bacau" jud_pop;;

(* rezultate 

  let a = statistica "Cluj" jud_pop ;;
val a : string list = ["Prahova"; "Iasi"]
# let b = statistica "Bacau" jud_pop;;
val b : string list =
  ["Timis"; "Suceava"; "Prahova"; "Iasi"; "Dolj"; "Constanta"; "Cluj"]

*)