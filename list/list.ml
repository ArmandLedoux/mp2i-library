(** mem l e retourne true si e est dans l et false sinon *)
let rec mem l e = match l with 
    | [] -> false
    | x::q -> e=x || mem q e
 ;;
 
(** taille l retourne le nombre d'éléments de l *)
 let rec taille l = match l with
    | [] -> 0
    | e::q -> 1 + taille q
;;

(** somme l retourne la somme des éléments de l (liste d'entiers) *)
let rec somme l = match l with
    | [] -> 0
    | e::q -> e + somme q;;
;;

(** moyenne l retourne la moyenne des éléments de l *)
let moyenne l = 
    let rec sum_len l = match l with
        | [] -> (0., 0.)
        | e::q -> let lenq, sumq = sum_len l in
            (lenq +. 1., sumq +. e)
    in
    let len, sum = sum_len l in
    if l = [] then 0. else 
    sum /. len
;;

(** croissante l retourne true si l est croissante et false sinon*)
 let rec croissante l = match l with 
    | [] -> true 
    | [e] -> true
    | e1::e2::q -> e1 <= e2 && croissante (e2::q)
 ;;
 
 (** rev l retourne l dans le sens inverse *)
 let rev l = 
    let rec aux l acc = match l with 
    | [] -> acc
    | e::q -> aux q (e::acc)
    in aux l []
;;

(** range l retourne une liste contenant les entier de 1 à n*)
let range n = 
    let rec f n k = 
        if k = 0 then [n] else
        (n-k) :: f n (k-1) in
    f n (abs n)
;;

(** iter f l aplique f sur chaque élement de l et retourne () *)
let rec iter f l = match l with
    | [] -> ()
    | e::q -> f e ; iter f q
;;

(** comme iter f l mais renvoie la liste des valeurs de retour de chaque élément par f *)
let rec map f l = match l with 
    | [] -> []
    | e::q -> (f e)::(map f q)
;;

(** retourne true si il existe un élement x de l tel que f x = true et retourne false sinon *)
let rec exists f l = match l with 
    | [] -> false
    | e::q -> f e || exists f q
;;

(** retourne true si chaque élément x de vérifie f x = true et retourne false sinon *)
let rec for_all f l = match l with
    | [] -> true 
    | e::q -> f e && for_all f q
;;

(** retourne la liste contenant tous les éléments x qui vérifient f x = true *)
let rec filter f l = match l with
    | [] -> []
    | e::q when f e -> e::(filter f q)
    | e::q -> filter f q
;;

(** retourne l triée par ordre croissant en complexité O(n*log(n)) *)
let rec tri_fusion l =
    (* sépare une liste en deux listes égales (à un élément près) en complexité O(n) *)
    let rec split l = match l with 
        | [] -> [],[]
        | [e] -> [e],[]
        | e1::e2::q -> let q1, q2 = split q in
                      e1::q1, e2::q2 
    in
    (* fusionne deux liste triées par ordre croissant en une liste triée par ordre croissant en complexité O(n1+n2) *)
    let rec fusion l1 l2 = match l1, l2 with 
        | [], q -> q
        | q, [] -> q
        | e1::q1, e2::q2 when e1 > e2 -> e2::(fusion l1 q2)
        | e1::q1, e2::q2 -> e1::(fusion q1 l2)
    in
    match l with 
    | [] -> []
    | [e] -> [e]
    | q -> let q1, q2 = split q in
        fusion (tri_fusion q1) (tri_fusion q2)
;;

(** retourne true si l a deux fois le même élément et false sinon *)
let rec doublons l = match l with 
    | [] -> false
    | e::q -> mem q e || doublons q
