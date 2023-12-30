(*INSERTION SORT*)

(*Recursiva no terminal*)
let rec insert x = function
  [] -> [x]
  | h::t -> if x <= h then x :: h :: t
            else h :: insert x t;;
let rec isort = function
  [] -> []
  | h::t -> insert h (isort t);;
  
let bigl = List.init 1000000 succ;;

(*Recursiva terminal*)
(*Intento 1, tarda demasiado*)
(*let rec insert_t x l =
   let rec insertaux aux x lst = match lst with
              [] -> aux @ [x]
            | h::t -> if x<=h then aux@[x]@lst 
                              else insertaux (aux@[h]) x t
   in insertaux [] x l;; *)

let rec insert_t x l =
   let rec insertaux aux x lst = match lst with
              [] -> List.rev(x::aux)
            | h::t -> if x<=h then List.rev_append aux (x::lst)
                              else insertaux (h::aux) x t
   in insertaux [] x l;;


let rec isort_t l= 
     let rec isortaux aux lst = match lst with
            [] -> aux
          | h::t  -> isortaux (insert_t h aux) t      
     in isortaux [] l;;

(*CONCLUSIONES*)
(*
isort lc1 : 0.00144
isort lc2 : 0.00308
sort ld1 : 2.8869
isort ld2 : 13.756
Esta implementación funciona muy bien en el caso en el que la lista ya esté ordenada,
pero se obtienen unos resultados muy malos en el peor caso (lista descendiente) 

isort_t lc1 : 4.23
isort_t lc2 : 20.24
isort_t ld1 : 0.00094
isort_t ld2 : 0.0029
isort_t lr1 : 0.956
isort_t lr2 : 4.32

Esta implementación, en cambio, funciona mucho mejor en el "peor caso" (orden descendiente),
ya que la función insert_t nunca entra en el caso recursivo (solo tiene que recorrer la lista
una única vez)
*)


(*DEFINICIONES*)
let rec rlist = function
   0 ->[]
  | n -> Random.int 100 :: rlist (n-1);;

let lc1 = List.init 10000 succ;;
let lc2 = List.init 20000 succ;;

let ld1 = List.rev lc1;;
let ld2 = List.rev lc2;;

let lr1 = rlist 10000;;
let lr2 = rlist 20000;;


let crono f x =
   let t = Sys.time () in
   let _ = f x in
   Sys.time () -. t;;   


(*ISORT G*) 
(*Intento 1, tarda demasiado*)
(*let insert_g f x l =
   let rec insertaux f aux x lst = match lst with
              [] -> aux @ [x]
            | h::t -> if f x h =true then aux@[x]@lst 
                              else insertaux f (aux@[h]) x t
   in insertaux f [] x l;; *)

let insert_g f x l =
   let rec insertaux f aux x lst = match lst with
              [] -> List.rev (x::aux)
            | h::t -> if f x h =true then List.rev_append aux (x::lst) 
                              else insertaux f (h::aux) x t
   in insertaux f [] x l;; 


let isort_g f l =
   let rec isortaux f aux lst = match lst with
            [] -> aux
          | h::t  -> isortaux f (insert_g f h aux)  t      
     in isortaux f [] l;;


(*MERGE SORT*)
let rec split l = match l with
   h1::h2::t -> let t1, t2 = split t
                 in h1::t1, h2::t2
  | _ -> l, [];;
  
let rec merge (l1,l2) = match l1, l2 with
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, l2)
                      else h2 :: merge (l1, t2)
                      
let rec msort l = match l with
     [] | [_] -> l
    | _ -> let l1, l2 = split l
           in merge (msort l1, msort l2);;
           

let bigl2 = List.init 50_000_000 succ;;

let split_t l =
  let rec splitaux l aux1 aux2 = match l with
    h1::h2::t -> splitaux t (h1::aux1) (h2::aux2)
  | _ -> List.rev aux1, List.rev aux2
in splitaux l [] [];;

let merge_t (l1, l2) = 
  let rec mergeaux aux l1 l2 = match l1, l2 with
    [], l | l, [] -> List.rev_append aux l
    | h1::t1, h2::t2 -> if h1 <= h2 then mergeaux (h1::aux) t1 l2
                        else mergeaux (h2::aux) l1 t2
in mergeaux [] l1 l2;;

let rec msort' l = match l with
  [] | [_] -> l
| _ -> let l1, l2 = split_t l
       in merge_t (msort' l1, msort' l2);;

let bigl3 = [];;



         
(*CONCLUSIONES*)
(* No encuentro razones por las que no debería provocar stack overflow, pues no es recursiva terminal. Quizá por optimizaciones del compilador.

msort lc1 : 0.0416
msort lc2 : 0.0840
msort ld1 : 0.0416
msort ld2 : 0.0788
msort lr1 : 0.0201
msort lr2 : 0.0365

msort' lc1 : 0.0364
msort' lc2 : 0.0865
msort' ld1 : 0.0232
msort' ld2 : 0.0537
msort' lr1 : 0.0182
msort' lr2 : 0.0312

Podemos ver que ambas implementaciones funcionan bastante bien, habiendo poca diferencia en cuanto a los tiempos. En cambio, comparándolo con el isort sí que podemos ver una gran mejoría en cualquier caso.

*)  

let merge_t_g f (l1, l2) = 
  let rec mergeaux acc l1 l2 = match l1, l2 with
    [], l | l, [] -> List.rev_append acc l
    | h1::t1, h2::t2 -> if f h1 h2 then mergeaux (h1::acc) t1 l2
                        else mergeaux (h2::acc) l1 t2
  in mergeaux [] l1 l2;;

let rec msort_g f l = match l with
  [] | [_] -> l
| _ -> let l1, l2 = split_t l
        in merge_t_g f (msort_g f l1, msort_g f l2);;
        
        
