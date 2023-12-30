let come (i1,j1)(i2,j2) = 
    i1=i2|| j1=j2 || abs(i1-i2) = abs (j1-j2);;

let compatible p l = 
    not (List.exists (come p) l);;

let queens n =
     let rec completar path i j =
         if i>n then [path]
         else if j >n then []
         else if compatible (i,j) path
                 then (completar ((i,j) :: path) (i+1) 1)@(completar path i (j+1))
                 else completar path i (j+1)
     in completar [] 1 1;;

let is_queens_sol n =
  let rec is_queens_solaux n acc = function
    [] -> acc=0
  |  (x,y)::t -> y<=n && x<=n && y>0 && x>0 && not(List.exists (come (x,y)) t) &&    is_queens_solaux n (acc-1) t
in is_queens_solaux n n;;




