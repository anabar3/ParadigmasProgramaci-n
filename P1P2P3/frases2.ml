(*x-y;;*)
(*Error de compilación: x no está definido*)

let x = 1;;
(*val x : int=1*)

(*x-y;;*)
(*Error de compilación: y no está definido*)

let y =2;;
(*val y : int =2*)

(*x-y;;*)
(function x -> function y->x-y) 1 2;;
(*int =-1*)

(*let x=y in x-y;;*)
(function x-> function y-> y-y) 1 2;;
(*int =0*)

(*x-y;;*)
(function x-> function y-> x-y) 1 2;;
(*int = -1*)

(*z;;*)
(*Error de compilación: z no está definido*)

let z =x+y;;
(*val z: int =3*)

z;;
(*int = 3*)

let x=5;;
(*val x : int= 5*)

(*x+y;;*)
(function x-> function y-> x+y) 5 2;;
(*int = 7*)

z;;
(*int =3*)

(*let y=5 in x+y;;*)
(function x -> function y-> x+y) 5 5;;
(*int =10*)

(*x+y;;*)
(function x-> function y-> x+y) 5 2;;
(*int = 7*)

(*let x=x+y in let y = x * y in x+y+z;;*)
(function x-> function y -> function z -> x+ y +(x+y)*y +z) 5 2 3;;
(*int = 24*)

(*x+y+z;;*)
(function x-> function y -> function z ->  x+y+z) 5 2 3;;
(*int =10*)

function x ->2*x;;
(*int -> int = <fun>*)

(function x ->2*x) (2+1);;
(*int = 6*) 

(function x -> 2* x) 2+1;;
(*int = 5*)

let f= function x->2*x;;
(*val f: int -> int = <fun>*)

f;;
(*int -> int = <fun>*)

f(2+1);;
(*int = 6*)

f 2+1;;
(*int =5*)

f x;;
(*int=10*)

let x=100;;
(*val x: int=100*)

f x;;
(*int = 200*)

let m = 1000;;
(*val m: int =1000*)

let g = function x->x+m;;
(*val g: int-> int = <fun>*)

(*g;;*)
(function x->x+m);;
(*int -> int = <fun>*)

(*g 3;;*)
(function x-> function m->x+m) 3 1000;;
(*int =1003*)

(*g 3.0;;*)
(*Error de tipo: se esperaba un int y recibió un float*)

let m =7;;
(*val m: int = 7*)

(*g 3;;*)
(function x-> function m->x+m) 3 1000;;
(*int = 1003*)

let istrue = function true ->true;;
(*WARNING partial match 
val istrue : bool ->bool = <fun>*)

istrue;;
(*bool -> bool = <fun>*)

istrue (1<2);;
(*bool = true*)

istrue (2<1);;
(*Exception : Match_failure*)

(*istrue (0);;*)
(*Error de tipo: se esperaba una expresion de tipo bool y se recibió una de tipo int*)

let iscero_v1 = function 0 ->true;;
(*WARNING partial match
val iscero_v1: int ->bool = <fun>*)

iscero_v1 0;;
(*bool = true*)

(*iscero_v1 1;;*)
(*Exception: Match_failure*)

(*iscero_v1 0.;;*)
(*Error de tipo: se esperaba una expresión de tipo int y se recibió una de tipo float*)

let iscero_v2 =function 0->true | _ ->false;;
(*val iscero_v2 : int ->bool = <fun>*)

iscero_v2 0;;
(*bool = true*)

iscero_v2 1;;
(*bool = false*)

(*iscero_v2 0.;;*)
(*Error de tipo: se esperaba una expresión de tipo int y se recibió una de tipo float*)

let all_to_true = function true ->true |false ->true;;
(*val all_to_true : bool -> bool = <fun>*)

all_to_true (1<2);;
(*bool = true*)

all_to_true (2<1);;
(*bool = true*)

(*all_to_true 0;;*)
(*Error de tipo: se esperaba una expresion de tipo bool y recibio una de tipo int*)

let first_all_to_true = all_to_true;;
(*val first_all_to_true : bool ->bool = <fun>*)

let all_to_true = function x ->true;;
(*val all_to_true : 'a ->bool =<fun>*)

all_to_true (1<2);;
(*bool = true*)

all_to_true (2<1);;
(*bool =true*)

all_to_true 0;;
(*bool=true*)

(*first_all_to_true 0;;*)
(*Error de tipo: se esperaba una expresión de tipo bool y se recibió una de tipo int*)

