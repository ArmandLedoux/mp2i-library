(** Voici une implémentation impérative d'une liste doublement chaînée.
    Pour un élement, appelé simplement l2c (car il permet d'accéder à tous les autres éléments de la liste), elem désigne sa valeur, next l'élément suivant et prev le précédent. *)
type 'a l2c = {elem: 'a; mutable prev: 'a l2c ; mutable next: 'a l2c}
;;

(** Crée une nouvelle liste doublement chaînée ne contenant qu'un seul élement : e *)
let create_l2c e =
    let rec l = {elem = e; prev = l; next = l} in l
;;

(** Ajoute e à la liste doublement chaînée, en tant qu'élément précédent de l2c, et retourne unit *)
let add l2c e = 
    let l2c2 = {elem = e; prev=l2c.prev; next=l2c} in
    l2c.prev.next <- l2c2 ;
    l2c.prev <- l2c2
;;

(** Retire de la liste doublement chaînée l'élement précédent de l2c, et retourne unit *)
let del l2c = 
    if l2c.next == l2c then failwith "Ne peut pas supprimer le seul élément de la l2c"
    else l2c.prev.next <- l2c.next ;
    l2c.next.prev <- l2c.prev ;
;;

(** Compte le nombre d'élements de la liste doublement chaînée *)
let length l2c = 
    let rec aux l2c l2c2 n = 
        if l2c == l2c2 then n
        else aux l2c (l2c2.next) (n+1) 
    in 
    aux l2c (l2c.next) 1
;;

(** Détermine si e appartient à la liste doublement chaînée *)
let mem l2c e = 
    let rec aux l2c l2c2 e =
        l2c2.elem = e || (l2c != l2c2 && aux l2c (l2c2.next) e)
    in
    aux l2c (l2c.next) e
;;

(** fusionne deux listes doublement chaînés à la suite *)
let fusion l2c l2c2 = 
    l2c.prev.next <- l2c2 ;
    l2c2.prev.next <- l2c ;
    l2c.prev <- l2c2.prev ;
    l2c2.prev <- l2c.prev
;;
