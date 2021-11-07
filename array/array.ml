(** Retourne la somme des éléments de t (entiers) (O(n)) *)
let somme t = 
    let s = ref 0 in
    for i=0 to (Array.length t)-1 do
    s := !s + t.(i) done ;
    !s
;;

(** Retourne le maximum de t (O(n)) **)
let maximum t = 
    let m = ref min_int in
    for i=0 to (Array.length t)-1 do
    m := max !m  t.(i) done ;
    !m
;;

(** Retourne true si t est croissant et false sinon (O(n)) *)
let croissant t = 
    let croiss = ref true in
    for i=0 to Array.length t - 2 do
        if t.(i) > t.(i+1) then
        croiss := false
    done ;
    !croiss
;;

(** Applique f à chaque élément de t de façon impérative (t est modifié) (O(n) * O(f))
    f doit être de type 'a -> 'a *)
let map f t =
    for i = 0 to Array.length t - 1 do 
        t.(i) <- f t.(i)
    done ;
;;

(** Comme map mais retourne un nouveau tableau sans modifier l'ancien.
    f est du type 'a -> 'b *)
let map2 f t = 
    let n = Array.length t in
    if n <> 0 then
        let t2 = Array.make n (f t.(0)) in
        for i=1 to n-1 do 
            t2.(i) <- f t.(i)
        done;
        t2
    else [||]
;;

(** Retourne true si e est dans t et false sinon (O(n)) *)
let mem t e = 
    let n = Array.length t in
    let found = ref false in
    for i = 0 to n-1 do
        found := !found || t.(i) = e
    done ;
    !found
;;

(** Comme mem t e mais par dichotomie pour un tableau déjà trié par ordre croissant (O(log(n)))  *)
let mem2 t e = 
    let i, j = ref 0, ref (Array.length t - 1) in
    let m = ref ((!i + !j)/2) in
    while !i <= !j && t.(!m) <> e do
        if t.(!m) > e then i := !m + 1 
        else j := !m - 1;
        m := (!i + !j)/2
    done ;
    t.(!m) = e
;;

(** Retourne un max local de t (O(n)) *)
let max_local t = 
    let n = Array.length t in
    if n = 0 then min_int 
    else if n = 1 || t.(0) >= t.(1) then t.(0)
    else if t.(n-1) >= t.(n-2) then t.(n-1)
    else let i = ref 0 in
        while !i < n && t.(!i) < t.(!i+1) do
            incr i 
        done ;
        !i
;;

(** Comme max_local mais par dichotomie (O(log(n))) *)
let max_local2 t =
    let n = Array.length t in
    if n = 0 then min_int 
    else if n = 1 || t.(0) >= t.(1) then t.(0)
    else if t.(n-1) >= t.(n-2) then t.(n-1)
    else
        let rec dicho i j =
            let m = (i+j)/2 in
            if t.(m) <= t.(m-1) then dicho i (m-1)
            else if t.(m) <= t.(m-1) then dicho (m+1) j
            else m
        in let m = dicho 0 n in
        t.(m)
;;

(** Trie t par comptage (O(n+m) où m est le maximum de t) *)
let tri_comptage t = 
    let m = maximum t in
    let compte = Array.make (m+1) 0 in
    for i=0 to Array.length t -1 do
        compte.(t.(i)) <- compte.(t.(i))+1
    done ;
    let a = ref 0 in
    for i=0 to m do
        for j=0 to compte.(i)-1 do
            t.(!a) <- i ;
            incr a
        done ;
    done ;
;;

(** swap t i j échange les valeurs t.(i) et t.(j) (O(1)) *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp
;;

(** tri_bulle t trie t en complexité O(n**2) *)
let tri_bulles t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done
;;
