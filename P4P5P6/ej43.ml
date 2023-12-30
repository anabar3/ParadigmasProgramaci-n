let rec cifras = function n ->
    if (n==0) then 0
    else 1+cifras(n/10)

let rec exp= function 0 ->1 | n ->10 *exp (n-1)

let rec reverse = function
    0 -> 0 |
    n-> (n mod 10) * exp (cifras (n) -1) + reverse (n/10);;
    
  
let substring = function str -> 
    String.sub str 1 (String.length str -2)
  
let rec palindromo = function
    "" -> true |
    str -> if String.length str = 1 then true
            else str.[0] = str.[String.length str -1] && palindromo (substring str);;
    
let rec mcd (x, y) =
    if x>y then
        if x mod y =0 then y
        else mcd (x mod y, y)
    else if x<y then
        if y mod x = 0 then x
        else mcd (y mod x, x)
    else x;;
