let g1 n = 
    if n>= 0 then 
        if n mod 2= 0 then true 
             else false
    else if n mod 2 = -1 then true 
             else false;;
    
let g2 n = match n>=0, n mod 2 with
    true, 0 -> true
   | false, -1 ->true
   | _, _ -> false;;

