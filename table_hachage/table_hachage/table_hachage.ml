(** Voici une implémentation impérative d'une table de hachage avec un tableau.
    Une table_hachage contient une fonction de hachage qui, avec une clé de type 'a, retourne un entier compris entre 0 et [largeur]. 
    Il contient également un tableau de taille [largeur]. Chaque élément de ce tableau contient la liste des couples (clé, valeur) tels que [hache clé] soit égal à l'indice de cet élément. *)
type ('a, 'b) table_hachage = {hache: 'a -> int; donnees: ('a * 'b) list array; largeur: int}
;;

(** Crée une table_hashage vide de largeur w et de fonction de hachage h en (O(w))*)
let creer h w = 
    {hache = h; donnees = (Array.make w []); largeur = w}
;;

(** Retourne true si un élément avec la clé k est dans la table_hachage th et false sinon (O(n/w)) *)
let recherche th k = 
    let l = th.donnees.((th.hache) k) in
    List.mem k (List.map fst l)
;;

(** Si un élément a pour clé k dans la table_hachage th, renvoie sa valeur. Sinon failwith "k not found" (O(n/w)) *)
let element th k = 
    let rec aux l k = match l with
        | [] -> failwith "k not found"
        | (k',e)::q when k' = k -> e
        | e::q -> aux q k
    in
    aux (th.donnees.(th.hache k)) k
;;

(** Ajoute l'élément e avec la clé k à la table_hachage th (O(1)) *)
let ajout th k e = 
    let rec aux l k e = match l with
        | (k',_)::q when k = k'-> l
        | e'::q -> e'::aux q k e
        | [] -> [(k,e)]
    in
    let hash = th.hache k in
    th.donnees.(hash) <- aux th.donnees.(hash) k e
;;

(** Retire l'élément avec la clé k de la table_hachage th (O(n/w)) si il existe *)
let suppr th k = 
    let rec aux l k = match l with
        | (k',e)::q when k' = k -> q
        | e::q-> e::aux q k
        | [] -> []
    in
    let hash = th.hache k in
     th.donnees.(hash) <- aux (th.donnees.((th.hache) k)) k
;;