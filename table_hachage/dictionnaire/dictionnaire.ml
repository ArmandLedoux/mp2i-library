(* Lorsque l'on implémente une table de hachage on peut l'implémenter avec un tableau contenant des listes. Le problème étant que la complexité de recherche d'un élément est linéaire, car il faut parcourir chaque élément d'une de ces listes. J'ai essayé de créer une implémentation impérative d'une table de hachage dont la complexité ne dépend pas du nombre de clés mais de la clé elle-même, et avec une fonction de hachage injective. Elle a été conçue avec l'idée d'un dictionnaire (comme en python) comme le témoigne son nom, mais peut fonctionner avec d'autres types et fonctions de hachage.
Cette implémentation consiste en une table de hachage à tableau non dynamique. Chage case du tableau contient un couple d'options : un élément et une autre table de hachage, ce qui résulte en des tables de hachage imbriquées.
Ici, j'ai choisi de créer deux types : un dict (niveau le plus haut) et des sub_dict, qui sont contenus dans le dict ou dans d'autres sub_dict. *)

type ('a, 'b) dict = {
    values: ('b option * ('a, 'b) sub_dict option)  array;
    hash: 'a -> int -> int; (* Dépend d'un paramètre supplémentaire. Pour un dictionnaire, ce sera la position du caractère dans la clé. *)
    klength: 'a -> int} (* Pour une clé, retourne sa "taille", c'est-à-dire le nombre de sub_dict imbriqués nécessaires pour la stocker (pour un dictionnaire, ce sera le nombre de cractères de la clé). *)
and ('a, 'b) sub_dict = {
    data: ('b option * ('a, 'b) sub_dict option)  array;
    mutable nbkeys: int} (* Suit le nombre d'élements différents de None (éléments ou su_dicts) présents dans data.
                           Lorsqu'il atteint 0, on remplace sub_dict par None (pour libérer de l'espace) *)
;;

(** Crée un dict avec la fonction de hachage h (injective), la fonction klen qui renvoie la "taille" de la clé, et la taille des tableaux du dict et des sub_dict. (O(m)) *)
let create_dict m h klen = { 
    values = Array.make m (None,None) ; 
    hash = h ;
    klength = klen
};;

(** Ajoute l'élément e de clé k au dictionnaire d. Avec n la "taille" de k et m la taille des tableaux des sub_dict, la complexité est de O(m * n) dans le pire des cas (si on est obligé de créer un sub_dict à chaque étape) et de O(n) dans le meilleur des cas (si les sub_dicts existent déjà). *)
let add_dict d k e = 
    let n = d.klength k - 1 in
    let rec aux sd i = 
        let array = sd.data in
        let ki = d.hash k i in
        if i = n then
            (array.(ki) <- (Some e, snd (array.(ki))) ;
            sd.nbkeys <- sd.nbkeys + 1)
        else ((if snd array.(ki) = None then 
            array.(ki) <- (fst array.(ki), 
            Some {data = Array.make (Array.length array) (None, None); nbkeys=0}) ;
            sd.nbkeys <- sd.nbkeys + 1) ;
            aux (Option.get (snd array.(ki))) (i+1))
    in
    aux {data=d.values; nbkeys=0} 0
;;

(** Retourne la valeur de l'unique élément de d avec la clé k. Si il n'éxiste pas, failwith "key not found" (O("taille" de k)) *)
let get_dict d k  = 
    let n = d.klength k - 1 in
    let rec aux sd i = 
        let array = sd.data in
        let ki = d.hash k i in
        if i = n then 
            try Option.get (fst array.(ki)) 
            with Invalid_argument _ -> failwith "key not found"
        else if snd array.(ki) = None then failwith "key not found"
        else aux (Option.get (snd array.(ki))) (i+1)
    in
    aux {data=d.values; nbkeys=0} 0
;;

(** Comme get_dict mais retire cet élément du tableau *)
let pop_dict d k  = 
    let n = d.klength k - 1 in
    let rec aux sd i = 
        let array = sd.data in
        let ki = d.hash k i in
        if i = n then (
            let e = fst array.(ki) in
            if e = None then failwith "key not found"
            else
            (array.(ki) <- (None, snd array.(ki)) ; 
             sd.nbkeys <- sd.nbkeys - 1 ;
            Option.get e))
        else if snd array.(ki) = None then failwith "key not found"
        else (let e = aux (Option.get (snd array.(ki))) (i+1) in
            if (Option.get (snd array.(ki))).nbkeys = 0 then (* Si un sub_dict est vide, on le supprime aussi *)
            (array.(ki) <- (fst array.(ki), None) ;
            sd.nbkeys <- sd.nbkeys - 1);
            e)
    in
    aux {data=d.values; nbkeys=0} 0
;;