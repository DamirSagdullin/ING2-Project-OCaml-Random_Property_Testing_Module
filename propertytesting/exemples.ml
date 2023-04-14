#use "Test.ml" ;;

(* ##################### TEST DES FONCTIONNALITES DE BASE ##################### *)

(* TEST : Tests de la division euclidienne                                                                   *)
(* Les tests sont effectués sur des couples d'entiers compris entre -100 et 100 dont le second est *non nul* *)
(* (d'où l'utilisation du filtre pour éviter des divisions par zéro).                                        *)
let gen_intcouple =
  let gen_dividend =                            Generator.int (-100) 100
  and gen_divisor  = Generator.filter ((<>) 0) (Generator.int (-100) 100)
    in Generator.combine gen_dividend gen_divisor ;;
let red_intcouple =
  let red_dividend =                           Reduction.int
  and red_divisor  = Reduction.filter ((<>) 0) Reduction.int
    in Reduction.combine red_dividend red_divisor ;;
let test_intcouple = Test.make_test gen_intcouple red_intcouple ;;

(* Construction des tests *)
let test_quorem       = test_intcouple "/ et mod (correct)" (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;
let test_quorem_wrong = test_intcouple "/ et mod (faux)"    (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;

(* Exécution des tests *)
Test.check    100 test_quorem       ;;
Test.check    100 test_quorem_wrong ;;
Test.fails_at 100 test_quorem       ;;
Test.fails_at 100 test_quorem_wrong ;;
Test.execute  100 [test_quorem ; test_quorem_wrong] ;;

(* TEST : Tests sur la concaténation de listes                                  *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlistcouple =
  let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) in
    Generator.combine gen_intlist gen_intlist ;;
let red_intlistcouple =
  let red_intlist = Reduction.list     Reduction.int_nonneg     in
    Reduction.combine red_intlist red_intlist ;;
let test_intlistcouple = Test.make_test gen_intlistcouple red_intlistcouple ;;

(* Constructon des tests *)
let test_append       = test_intlistcouple "List.@ (correct)" (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;
let test_append_wrong = test_intlistcouple "List.@ (faux)"    (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;

(* Exécution des tests *)
Test.check    100 test_append       ;;
Test.check    100 test_append_wrong ;;
Test.fails_at 100 test_append       ;;
Test.fails_at 100 test_append_wrong ;;
Test.execute  100 [test_append ; test_append_wrong] ;;

(* TEST : Tests sur l'inversion de listes                                       *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) ;;
let red_intlist = Reduction.list Reduction.int_nonneg ;;

let test_intlist = Test.make_test gen_intlist red_intlist ;;

(* Construction des tests *)
(* test_rev_correct vérifie que l'inversion deux fois de la liste donne la liste originale.*)
let test_rev_correct = test_intlist "List.rev (correct)" (fun l -> List.rev (List.rev l) = l) ;;
(* test_rev_wrong vérifie que l'inversion de la liste donne la même liste, ce qui devrait échouer.*)
let test_rev_wrong   = test_intlist "List.rev (faux)" (fun l -> List.rev l = l) ;;

(* Exécution des tests *)
Test.check    100 test_rev_correct ;;
Test.check    100 test_rev_wrong ;;
Test.fails_at 100 test_rev_correct ;;
Test.fails_at 100 test_rev_wrong ;;
Test.execute  100 [test_rev_correct; test_rev_wrong] ;;

(* TEST : Tests sur l'application d'une fonction à chaque élément d'une liste   *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) ;;
let red_intlist = Reduction.list Reduction.int_nonneg ;;
let test_intlist = Test.make_test gen_intlist red_intlist ;;

(* Construction des tests *)
let double x = 2 * x ;;
(* test_map_correct vérifie que l'application de la fonction double deux fois à la liste donne la même liste que l'application d'une fonction qui multiplie chaque élément par 4.*)
let test_map_correct = test_intlist "List.map (correct)" (fun l -> List.map double (List.map double l) = List.map (fun x -> 4 * x) l) ;;
(* test_map_wrong vérifie que l'application de la fonction double à la liste donne la même liste, ce qui devrait échouer.*)
let test_map_wrong   = test_intlist "List.map (faux)" (fun l -> List.map double l = l) ;;

(* Exécution des tests *)
Test.check    100 test_map_correct ;;
Test.check    100 test_map_wrong ;;
Test.fails_at 100 test_map_correct ;;
Test.fails_at 100 test_map_wrong ;;
Test.execute  100 [test_map_correct; test_map_wrong] ;;

(* TEST : Tests sur le filtrage des éléments d'une liste                        *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) ;;
let red_intlist = Reduction.list Reduction.int_nonneg ;;
let test_intlist = Test.make_test gen_intlist red_intlist ;;

(* Construction des tests *)
let is_even x = x mod 2 = 0 ;;
(** test_filter_correct vérifie que le filtrage des éléments pairs d'une liste obtenue 
  * en multipliant chaque élément d'une liste par 2 donne la même liste que celle obtenue en multipliant chaque élément par 2.
  *)
let test_filter_correct = test_intlist "List.filter (correct)" (fun l -> List.filter is_even (List.map (fun x -> 2 * x) l) = List.map (fun x -> 2 * x) l) ;;
(* test_filter_wrong vérifie que le filtrage des éléments pairs d'une liste donne la même liste, ce qui devrait échouer.*)
let test_filter_wrong   = test_intlist "List.filter (faux)" (fun l -> List.filter is_even l = l) ;;

(* Exécution des tests *)
Test.check    100 test_filter_correct ;;
Test.check    100 test_filter_wrong ;;
Test.fails_at 100 test_filter_correct ;;
Test.fails_at 100 test_filter_wrong ;;
Test.execute  100 [test_filter_correct; test_filter_wrong] ;;

(* TEST : Tests sur le tri d'une liste                                           *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 100. *)
let gen_intlist = Generator.list 10 (Generator.int 0 100) ;;
let red_intlist = Reduction.list Reduction.int ;;
let test_intlist = Test.make_test gen_intlist red_intlist ;;

(* Construction des tests *)
(* Fonction qui trie une liste d'entiers en utilisant l'algorithme de tri par sélection  c'est la fonction quand va tester, elle contient volontairement une erreur*)
let rec selection_sort l= match l with
  | [] -> []
  | lst ->
      let min = List.fold_left (fun x y -> if x < y then x else y) (List.hd lst) lst in
      let rest = List.filter (fun x -> x != min) lst in
      min :: selection_sort rest
;;
(* test_sort vérifie que le tri d'une liste avec la fonction "List.sort" donne une liste triée.*)
let test_sort = test_intlist "List.sort (correct?)" (fun l -> List.sort compare l = selection_sort l) ;;

(* Exécution des tests *)
Test.check    100 test_sort;;
Test.checkPercentage    100 test_sort;;
Test.fails_at 100 test_sort ;;

(* Exemple : Fonction de produit scalaire *)
(* Nous avons défini une fonction dot_product pour calculer le produit scalaire de deux vecteurs représentés par des listes d'entiers. *)
let dot_product v1 v2 =
  List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2
;;

(* TEST : Tests sur le produit scalaire *)
let gen_intlist = Generator.list 10 (Generator.int (-10) 10) ;;
let red_intlist = Reduction.list Reduction.int ;;
let test_intlist = Test.make_test gen_intlist red_intlist ;;

(* Construction des tests *)
(* dot_product_testRight qui prend deux listes en argument et exécute les tests de produit scalaire correct (avec un +)
   Ensuite, nous avons défini les tests test_dot_product_correct en utilisant cette fonction. *)
   let dot_product_testRight l1 l2 =
    if List.length l1 <> List.length l2 then false
    else
      let result = dot_product l1 l2 in
      let combined_list = List.combine l1 l2 in
      let manual_result = List.fold_left (fun acc (x, y) -> acc + (x * y)) 0 combined_list in
      result = manual_result
  ;;  

(* dot_product_testWrong qui prend deux listes en argument et exécute les tests de produit scalaire correct (avec un -)
   Ensuite, nous avons défini les tests de test_dot_product_wrong en utilisant cette fonction. *)
  let dot_product_testWrong l1 l2 =
    if List.length l1 <> List.length l2 then false
    else
      let result = dot_product l1 l2 in
      let combined_list = List.combine l1 l2 in
      let manual_result = List.fold_left (fun acc (x, y) -> acc - (x * y)) 0 combined_list in
      result = manual_result
  ;;  

(* test_dot_product_correct compare que le produit scalaire calculé par la fonction dot_product 
   est égal au résultat obtenu en utilisant la fonction List.fold_left pour calculer manuellement le produit scalaire.*)
let test_dot_product_correct = Test.make_test (Generator.combine gen_intlist gen_intlist) (Reduction.combine red_intlist red_intlist) "dot_product (correct)" (fun (l1, l2) ->
    dot_product_testRight l1 l2) ;;
(* test_dot_product_wrong vérifie que le produit scalaire calculé par la fonction dot_product est égal au résultat obtenu en utilisant 
   la fonction List.fold_left pour calculer une version incorrecte du produit scalaire (en soustrayant plutôt qu'en additionnant les produits), ce qui devrait échouer.*)
let test_dot_product_wrong = Test.make_test (Generator.combine gen_intlist gen_intlist) (Reduction.combine red_intlist red_intlist) "dot_product (faux)" (fun (l1, l2) ->
 (dot_product_testWrong l1 l2)) ;;

(* Exécution des tests *)
Test.check    100 test_dot_product_correct ;;
Test.check    100 test_dot_product_wrong ;;
Test.fails_at 100 test_dot_product_correct ;;
Test.fails_at 100 test_dot_product_wrong ;;
Test.execute  100 [test_dot_product_correct; test_dot_product_wrong] ;;

(* ##################### FIN TEST DES FONCTIONNALITES DE BASE ##################### *)

(* ##################### TEST FONCTIONNALITES SUPPLEMENTAIRES ##################### *)

(** TEST DE 
  * int_seed de Generator,
  * and_ or_ not_ implies_ de Property,
  * fails_at_init de Test,
  *)

(* Define some properties *)
let is_positive x = x > 0 ;;
let is_even x = x mod 2 = 0 ;;
let is_divisible_by_3 x = x mod 3 = 0 ;;

(* Construct compound properties using and_, or_, not_, and implies_ *)
let p1 = Property.and_ is_positive is_even ;;
let p2 = Property.or_ is_positive is_divisible_by_3 ;;
let p3 = Property.not_ is_positive ;;                       
let p4 = Property.implies_ is_positive is_divisible_by_3 ;;

(* TEST : Tests de la propriété p1 *)
let test_p1 = 
  let gen_p1_input = Generator.filter is_positive (Generator.int_seed (-10) 10 123)
  in let red_p1_input = Reduction.int
  in let prop_p1 = p1
  in Test.make_test gen_p1_input red_p1_input "is_positive && is_even" prop_p1
;;

(* TEST : Tests de la propriété p2 *)
let test_p2 = 
  let gen_p2_input = Generator.int_seed (-10) 10 123
  in let red_p2_input = Reduction.int
  in let prop_p2 = p2
  in Test.make_test gen_p2_input red_p2_input "is_positive || is_divisible_by_3" prop_p2
;;

(* TEST : Tests de la propriété p3 *)
let test_p3 = 
  let gen_p3_input = Generator.filter (Property.not_ is_positive) (Generator.int_seed (-10) 10 123)
  in let red_p3_input = Reduction.int
  in let prop_p3 = p3
  in Test.make_test gen_p3_input red_p3_input "not is_positive" prop_p3
;;

(* TEST : Tests de la propriété p4 *)
let test_p4 = 
  let gen_p4_input = Generator.filter is_positive (Generator.int_seed (-10) 10 123)
  in let red_p4_input = Reduction.int
  in let prop_p4 = p4
  in Test.make_test gen_p4_input red_p4_input "is_positive => is_divisible_by_3" prop_p4
;;

Test.fails_at_init 100 test_p1 ;;
Test.fails_at_init 100 test_p2 ;;
Test.fails_at_init 100 test_p3 ;;
Test.fails_at_init 100 test_p4 ;;

(* ##################### FIN TEST FONCTIONNALITES SUPPLEMENTAIRES ##################### *)