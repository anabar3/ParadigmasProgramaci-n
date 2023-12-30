let next (x, y)=
  if  x=1 && y mod 2=1 then (x,y+1) 
      else if y=1 && x mod 2 = 0 then (x+1,y)
  else if (x+y) mod 2 =0 then (x-1,y+1)
      else (x+1, y-1);;
 
let rec steps_from (x,y) n=
   if n=0 then (x,y)
   else steps_from (next (x,y)) (n-1);;

let pair n= steps_from (1,1) (n-1);;

(*let pair_i p =
    let rec find i =
       if pair i = p then i
       else find (i+1)
 in find 1;;*)
 
 (* La función "pair_i" busca el índice de un par dado en la secuencia, pero su ejecución es lenta debido al cálculo de todos los pares hasta encontrar el que se busca.*)


let pair_i' p=
 let rec find (x,y) p1=
  if (x,y) = p then p1
   else find (next(x,y)) (p1+1)
 in find (1,1) 1;;
 (*La función "pair_i'" encuentra el índice de un par dado en la secuencia de manera más eficiente al emplear la función "next", reduciendo las llamadas recursivas en comparación con la otra función*)
