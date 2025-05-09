let is_prime n =
  let rec check_from i =
    i >= n ||
   (n mod i <> 0 && check_from (i+1))
  in check_from 2;;

let rec next_prime n =
    if is_prime (n+1) = true then n+1
    else next_prime (n+1);;
    
let rec last_prime_to n =
    if is_prime (n) = true then n
    else last_prime_to (n-1);;
    
    
let rec is_prime2 n =
  let rec check_from i =
    i >= int_of_float (sqrt(float_of_int(n))) || (*No hay que revisar todos, solo hasta la raíz del número*)
   (n mod i <> 0 && check_from (i+1))
  in check_from 2;;
    
