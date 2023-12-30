let rec fib n =
   if n <= 1 then n
   else fib (n-1) + fib (n-2);;
  
let rec printfib n counter =
  let curr = fib counter in
  if curr <= n then
    (print_endline (string_of_int curr);
     printfib n (counter+1));;


if (Array.length Sys.argv == 2) then 
   printfib (int_of_string Sys.argv.(1)) 0  
else print_endline("fibto: Invalid number of arguments");;
  
(*Si hubiéramos querido imprimir los primeros n elementos de la sucesión*)
(*let rec printfib n=
   if n>=0 then (printfib (n-1);
           print_endline(string_of_int(fib(n))));;
	   
	   
if Array.length (Sys.argv) =2 then 
   printfib (int_of_string (Sys.argv.(1)))
else print_endline("fibto: Invalid number of arguments");;*)
