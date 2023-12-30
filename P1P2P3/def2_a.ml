let p x = 2.*. 2.*. asin 1.*.x;;
let area x = 4.*.atan 1. *. x*. x;;
let absf x = if (x<0.) then x *. -1. else x;;
let even x = x mod 2 = 0;;
let next3 x = if x mod 3 = 0 then x else x - x mod 3 +3;;
let is_a_letter x = x >= 'A' && x<='Z' || x>='a' && x<='z' ;;
let string_of_bool x = if true then "verdadero" else "falso";;
