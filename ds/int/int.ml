(** Renvroide le plus grand diviseur commun de a et de b *)
let rec euclide a b = 
    let r = a mod b in
    if r = 0 then b else euclide b r
;;

(** Renvoie a**n avec a et n entiers (O(log(n))) *)
let rec exp_rapide a n = 
    if n < 0 then failwith "positive integer expected"
    else if n = 0 then 1
    else let x = exp_rapide a (n/2) in 
        if n mod 2 = 0 then x*x
        else x*x*a
;;

(** Renvoie si n est premier en complexité O(n) *)
let premier n = 
    if n < 0 then failwith "positive integer expected"
    else
        let rec diviseurs n i =
            if i = 1 then false 
            else n mod i = 0 || diviseurs n (i-1)
        in
        if n < 2 then false else not (diviseurs n (n/2))
;;