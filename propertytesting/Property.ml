module Property :
  sig
    (** Type d'une propriété portant sur des éléments de type 'a
      * Une propriété est une fonction booléenne. *)
    type 'a t = 'a -> bool

    (* CONSTANTES *)

    (** Propriété toujours vérifiée *)
    val always_true  : 'a t

    (** Propriété jamais   vérifiée *)
    val always_false : 'a t

    (* FONCTIONS *)

    (* ET logique *)
    val and_ : 'a t -> 'a t -> 'a t 

    (* OU logique *)
    val or_ : 'a t -> 'a t -> 'a t

    (* NON logique  *)
    val not_  : 'a t -> 'a t 

    val implies_ : 'a t -> 'a t -> 'a t
  end =
  struct
    type 'a t = 
      'a -> bool
    ;;

    (* CONSTANTES *)

    let always_true = 
      (* Fonction anonyme prenant un argument ignoré "_" et renvoyant "true" *)
      fun _ -> true
    ;;

    let always_false = 
      (* Fonction anonyme prenant un argument ignoré "_" et renvoyant "false" *)
      fun _ -> false
    ;;

    (* FONCTIONS *)

    let and_ p q = 
      fun x -> p x && q x
    ;;

    let or_ p q = 
      fun x -> p x || q x
    ;;

    let not_ p = 
      fun x -> not (p x)
    ;;

    let implies_ p q = 
      fun x -> not (p x) || q x
    ;;
  end ;;