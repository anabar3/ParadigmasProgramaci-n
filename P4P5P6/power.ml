let rec power x y =
    if y=0 then 1
    else x * power x (y-1);;
    
let rec power' x y =
    if y= 0 then 1
    else if y mod 2 =0 then power' (x*x) (y/2)
    else x* power' (x*x) (y/2);;
 
(*Es mejor en cuanto a eficiencia porque en los casos en los que y es par
la recursión se hace de forma terminal, y además la "y" se reduce a la mitad en
cada paso recursivo y no en 1 como en el primer caso*)
(*No hace realmente mucha diferencia porque la mejora es solo perceptible en números
muy grandes, para los cuales el resultado no será el correcto por el valor max_int*)   
    
let rec powerf x y =
    if y= 0 then 1.
    else if y mod 2 =0 then powerf (x*.x) (y/2)
    else x*. powerf (x*.x) (y/2);;
