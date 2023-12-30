(*no está entero, solo las primeras funciones*)  

let rec is_bst ord tree = match tree with 
    Empty ->true
  | Node (r, i, d) -> match i, d with
          Empty, Empty -> true
        | Node (ii,_,_), Node (dd, _,_) ->     
            is_bst ord i &&
            is_bst ord d &&
            ord ii r &&
            ord r dd;;
         | _ -> false ;;
(*
let rec bfs tree = function
     Empty -> []
     Node (r, i, d) -> match i, d with  r :: bfs i @ b
          Node (ii,_,_), Node(dd,_,_) -> r :: ii :: dd @ bfs i @ bfs d;;

*)

(*supuesto numero de nodos 1+2^n-1*)

let rec nnodos= function
   Empty -> 0
 | Node (_, i, d) -> 1 + nnodos i + nnodos d;;

let rec altura = function
   Empty -> 0
 | Node (_, i, d) -> 1 + max (altura i) (altura d);;


let perfecto tree = if nnodos tree = 1+2^(altura -1);;

