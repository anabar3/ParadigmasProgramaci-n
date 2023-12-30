type 'a bintree = 
    Empty
  | Node of 'a * 'a bintree * 'a bintree;;


let rec in_order = function
   Empty -> []
 | Node (r,i,d) ->  in_order i @ [r] @ in_order d;;


let rec insert ord tree x = match tree with
    Empty -> Node (x, Empty, Empty)
  | Node (r, i, d) -> if ord x r then Node (r, insert ord i x, d)
                      else Node (r, i, insert ord d x);;
    
let rec bst ord l = match l with
    [] -> Empty
  | h::t -> insert ord (bst ord t) h;;
    
let qsort ord l = 
    in_order (bst ord l);; 
    
  
