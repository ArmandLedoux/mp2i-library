(* Implémentation impérative d'une file avec un tableau *)
type 'a file = {tab : 'a array ;  mutable deb : int ; mutable fin : int ; mutable vide : bool} ;;

(** ajoute x à la file f (O(1)) et renvoie () *)
let ajoute f x = 
    if f.deb = f.fin && not f.vide then failwith "File pleine"
    else (f.tab.(f.fin) <- x ;
         f.fin <- (f.fin + 1) mod (Array.length f.tab) ;
         f.vide <- false)
;;

(** retire et retourne le plus ancien élément de la file f (O(1)) *)
let retire f = 
    if f.vide then failwith "File vide"
    else (let x = f.tab.(f.deb) in
        f.deb <- (f.deb + 1) mod (Array.length (f.tab)) ;
        f.vide <- f.deb = f.fin;
        x)
;;