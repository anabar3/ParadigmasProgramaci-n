let p = function x->2.*. 2.*. asin 1.*.x;;
let area = function x->4.*.atan 1. *. x*. x;;
let absf = function x->if (x<0.) then x *. -1. else x;;
let even = function x-> x mod 2 = 0;;
let next3 = function x-> if x mod 3 = 0 then x else x - x mod 3 +3;;
let is_a_letter = function x-> x >= 'A' && x<='Z' || x>='a' && x<='z' ;;
let string_of_bool = function x->if true then "verdadero" else "falso";;
