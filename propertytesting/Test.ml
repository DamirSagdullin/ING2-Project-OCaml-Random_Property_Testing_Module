#use  "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t
    
    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param name nom du test
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> string -> 'a Property.t -> 'a t
    
    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     un couple (pourcentage true,pourcentage false)
      *)
    val checkPercentage : int -> 'a t -> (float*float)
    
    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
    *)
    val fails_at : int -> 'a t -> 'a option

     (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 couple de la valeur ne vérifiant pas `prop` et de sa version réduite
    *)
    val fails_at_init : int -> 'a t -> ('a * 'a) option
    
    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list
  end =
  struct
    (* Définition de test sous forme d'enregistrement*)
    type 'a t = 
      {
        gen : 'a Generator.t;
        red : 'a Reduction.t;
        name : string;
        prop : 'a Property.t;
      }
    ;;

    (* Initialisation de l'enregistrement avec les données fournies *)
    let make_test gen red name prop = 
      { 
        gen;
        red;
        name;
        prop;
      }
    ;;

    let check n test =
      (* Définition d'une fonction auxiliaire "aux" qui prend un entier "i" et valeur "x" comme arguments *)
      let rec aux i x =
        (* Si la propriété testé est vérifiée pour n valeurs alors on retourne "true" *)
        if i >= n then 
          let () = Printf.printf "  >>> TEST ( %s ) --- PASSED\n" test.name in
          true
        (* Si le test de "x" passe on rappelle la fonction aux avec (i+1) et une nouvelle valeur générée*)
        else if test.prop x then aux (i+1) (Generator.next test.gen)
        (* On renvoie "false" sinon *)
        else 
          let () = Printf.printf "  >>> TEST ( %s ) --- FAILED\n" test.name in
          false
      (* On commence par appeller la fonction "aux" avec l'arguments 0 et une première valeur générée, et on retourne "true" si "n" est supérieur à 0 et que la fonction "aux" a renvoyé "true", sinon on retourne "false" *)
      in n > 0 && aux 0 (Generator.next test.gen)
    ;;

    let checkPercentage n test=
      let rec aux i j k x =
        if i>=n then
          (float_of_int(j)/.float_of_int(n),float_of_int(k)/.float_of_int(n))
        else if test.prop x then aux (i+1) (j+1) k (Generator.next test.gen)
        else aux (i+1) j (k+1) (Generator.next test.gen)
      in aux 0 0 0 (Generator.next test.gen)
    ;;

    let fails_at n test =
      (* Définition d'une fonction auxiliaire "aux" qui prend un entier "i" et valeur "x" comme arguments *)
      let rec aux i x =
        (* Si la propriété testé est vérifiée pour n valeurs alors on retourne "None" *)
        if i >= n then 
          let () = Printf.printf "  >>> TEST ( %s ) --- PASSED\n" test.name in
          None
        (* Si le test de "x" passe on rappelle la fonction aux avec (i+1) et une nouvelle valeur générée*)
        else if test.prop x then aux (i+1) (Generator.next test.gen)
        (* Sinon, on cherche un élément "k" de la liste de réduction qui ne satisfait pas la propriété "prop" de "test" *)
        else match List.find_opt (fun k -> not (test.prop k)) (test.red x) with
          (* Si on n'en trouve pas alors on renvoie x *)
          | None -> 
            let () = Printf.printf "  >>> TEST ( %s ) --- FAILED\n  >>> FAILED AT : ( Reduced input )\n  >>> " test.name in
            Some x
          (* Si on trouve une valeur "k" qui est égale à "x" alors on renvoie x *)
          | Some k when k = x -> 
            let () = Printf.printf "  >>> TEST ( %s ) --- FAILED\n  >>> FAILED AT : ( Reduced input )\n  >>> " test.name in
            Some x
          (* Sinon on réduit x encore une fois *)
          | Some k -> aux i k
      in aux 0 (Generator.next test.gen)
    ;;

    (* idem avec init mais retourne en plus la valeur qui causé l'erreur en cas d'échec de test *)
    let fails_at_init n test =
      let rec aux i x init =
        if i >= n then 
          let () = Printf.printf "  >>> TEST ( %s ) --- PASSED\n" test.name in
          None
        else if test.prop x then aux (i+1) (Generator.next test.gen) init
        else match List.find_opt (fun k -> not (test.prop k)) (test.red x) with
          | None -> 
            let () = Printf.printf "  >>> TEST ( %s ) --- FAILED\n  >>> FAILED AT : ( Failed input, reduced input )\n  >>> " test.name in
            Some (init, x)
          | Some k when k = x -> 
            let () = Printf.printf "  >>> TEST ( %s ) --- FAILED\n  >>> FAILED AT : ( Failed input, reduced input )\n  >>> " test.name in
            Some (init, x)
          | Some k -> aux i k init
      in aux 0 (Generator.next test.gen) (Generator.next test.gen)
    ;;

    let execute n tests = 
      (* Pour chache test on renvoie la paire de "test" et de résultat de "fails_at n test" *)
      List.map (fun test -> (test, fails_at n test)) tests
    ;;
  end ;;