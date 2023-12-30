let length l =
    let i = ref 0 in
    let vlist = ref l in
    while !vlist <> [] do
      i := !i + 1;
      vlist:= List.tl !vlist
    done;
  !i;;

let last l =
  if length l = 0 then failwith "last"
  else
    let i = ref 0 in
    let vlist = ref l in
    while List.tl !vlist <> [] do
      i := !i + 1;
      vlist:= List.tl !vlist
    done;
  List.hd !vlist;;
    
let nth l n =
  if n<0 then raise (Invalid_argument "last")
  else
    let i = ref 0 in
    let vlist = ref l in
    while !i < n && !vlist <> [] do
      i := !i + 1;
      vlist:= List.tl !vlist
    done;
    if !vlist = [] then raise (Failure "last")
    else List.hd !vlist;;
    
let rev l =
    let revl = ref [] in
    let vlist = ref l in
    while !vlist <> [] do
      revl := List.hd (!vlist) :: !revl;
      vlist := List.tl !vlist;
    done;
  !revl;;
  
let append l1 l2 =
    let result = ref [] in
    let vlist1 = ref l1 in
    let vlist2 = ref l2 in

    while !vlist1 <> [] do
      result := List.hd !vlist1 :: !result;
      vlist1 := List.tl !vlist1
    done;

    while !vlist2 <> [] do
      result := List.hd !vlist2 :: !result;
      vlist2 := List.tl !vlist2
    done;

    rev !result;;
    
let concat l = 
    let result = ref [] in
    let vlist = ref l in
    
    while !vlist <> [] do
       let vlist2 = ref (List.hd !vlist) in
       while !vlist2 <> [] do
         result := List.hd (!vlist2) :: !result;
         vlist2 := List.tl !vlist2;
       done;
       vlist := List.tl !vlist
    done;
    
    rev !result;;
    
    
let for_all f l =
    let result = ref true in
    let vlist = ref l in
    while !result = true && !vlist <> [] do
       result := f (List.hd !vlist);
       vlist := List.tl !vlist;
    done;
    
  !result;;
    
    
let exists f l =
    let result = ref false in
    let vlist = ref l in
    while !result = false && !vlist <> [] do
       result := f (List.hd !vlist);
       vlist := List.tl !vlist;
    done;
    
  !result;;

let find_opt f l =
    let result = ref None in
    let resultaux = ref false in
    let vlist = ref l in
    while !resultaux = false && !vlist <> [] do
       resultaux := f (List.hd !vlist);
       if !resultaux =true then result := Some (List.hd !vlist)
       else vlist := List.tl !vlist
    done;
    !result;;
    
let iter f l = 
    let vlist = ref l in
    while !vlist <> [] do
       f (List.hd !vlist);
       vlist := List.tl !vlist
    done;;

let fold_left f acc l =
   let result = ref acc in
   let vlist = ref l in
   while !vlist <> [] do
     result := f !result (List.hd !vlist);
     vlist := List.tl !vlist
   done;
   !result;;
