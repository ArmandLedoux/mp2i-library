type 'a pile_imperative = {
    vide : unit -> bool;
    ajout : 'a -> unit;
    retire : unit -> 'a
};;

(** Retourne une pile implémentée avec un array *)
let stack_of_array t =
  let n = ref 0 in (* nombre d'éléments de la pile *)
  {
    vide = (fun () -> !n = 0);
    ajout = (fun e -> if !n >= Array.length t 
            then failwith "Full stack"
            else (t.(!n) <- e; incr n));
    retire = (fun () -> if !n = 0
          then failwith "Empty stack"
          else (decr n; t.(!n)))
  };;

(** [stack_to_list s] converts a stack to a list *)
let stack_to_list s =
  let rec make_list () =
    if s.vide () then []
    else let top = s.retire () in
      top::make_list () in
  let l = List.rev (make_list ()) in
  List.iter s.ajout l;
  l
;;
