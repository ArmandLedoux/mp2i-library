(** Implémentation persistante d'une liste doublement chaînée : le zipper.
    Il est implémenté avec deux listes. Comparé à une liste classique, la première liste (appelée left) contient les élements du début de la liste classique jusqu'à un certain élément, appelé curseur, la seconde liste (appelée right) de cet élément jusqu'à la fin. *)
type 'a zipper = {left : 'a list ; right : 'a list}
;;

(** Retourne un nouveau zipper identique avec le curseur déplacé d'un cran vers la droite. *)
let rec move_right z = 
    match (z.right) with
        | [] -> if z.left = [] then failwith "Zipper vide"
            else move_right {left=([]) ; right=List.rev (z.left)}
        | e::q -> {left=e::(z.left) ; right=q}
;;

(** Retourne un nouveau zipper identique avec le curseur déplacé d'un cran vers la gauche. *)
let rec move_left z = 
    match (z.left) with
        | [] -> if z.right = [] then failwith "Zipper vide"
            else move_left {left=List.rev (z.right) ; right=([])}
        | e::q -> {left=q ; right=e::(z.right)}
;;

(** Retourne un nouveau zipper avec l'élément e au niveau du curseur en tant que premier élément *)
let ajout z e = 
    {left = z.left ; right = e::(z.right)}
;;

(** Retourne le premier élément du zipper ainsi qu'un nouveau zipper sans ce premier élémént *)
let rec retire z = match z.right with
    | [] -> retire {left=[]; right=List.rev z.left}
    | e::q -> e, {left = z.left; right = q}
;;

(** Convertit le zipper en liste et retourne cette dernière *)
let list_of_zipper z =
    List.rev (z.left) @ z.right