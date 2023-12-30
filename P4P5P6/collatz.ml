let f n = if n mod 2 = 0 then n/2 else 3*n+1;;

(*orbit*)
let rec orbit n =
    if n<=1 then "1"
    else string_of_int(n) ^ ", "^orbit(f n);;
    
(*length*)
let rec length n =
    if n<=1 then 0
    else 1+length(f n);;
    
(*top*)
let rec top n =
    if n<=1 then 1
    else max n (top(f n));; 

(*length'n'top*)
let rec length'n'top n =
    if n<=1 then (0,1)
    else let (len, top) = length'n'top (f n) in
             if n > top then (len+1,n)
             else (len+1,top);;

             
(*longest_in*)
let rec longestaux n curr maxl maxn =
  if curr = n+1 then (maxn,maxl) else
    let currlen = length curr in
    if currlen > maxl then 
      longestaux n (curr+1) currlen curr else
      longestaux n (curr+1) maxl maxn;;

let longest_in m n = longestaux n m 0 1;; 

(*highest_in*)
let rec highestaux n curr maxm maxn =
  if curr = n+1 then (maxn,maxm) else
    let currmax = top curr in
    if currmax > maxm then 
      highestaux n (curr+1) currmax curr else
      highestaux n (curr+1) maxm maxn;;

let highest_in m n = highestaux n m 1 1;;


