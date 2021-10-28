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

(*

statistica: funcția primește ca parametri numele unui județ și o listă de județe
(o listă de perechi, ca în fișier) și returnează o lista cu numele tuturor județelor
ce au populația mai mare decât cea a județului al cărui nume e dat ca
parametru numarul x

*)

let statistica nume lst = 
  let pop = List.fold_left ( fun sum (x, y) -> if x = nume then sum + y else sum + 0) 0 lst in 
  let partial = List.filter ( fun (x, y) -> y > pop ) lst in
  List.fold_left (fun vid (x, y) -> x :: vid) [] partial ;;

let a = statistica "Cluj" jud_pop ;;
let b = statistica "Bacau" jud_pop;;