(*hd*)
let hd = function
    [] -> raise(Failure "hd")
  | h::_ -> h;;
	
(*tl*)
let tl = function
     [] -> raise(Failure "tl")
   | _::t -> t;;

(*length*)
(*recursiva no terminal*)
let rec length l =
    if l = [ ] then 0
    else 1+length (tl l) ;;

(*recursiva terminal*)
let length ll =
   let rec aux (l, i) =
       if l = [ ] then i
       else aux (tl l, i+1)
   in aux (ll, 0) ;;

(*compare_lengths*)
(*recursiva no terminal*)
let rec compare_lengths = function
   [] -> (function [] -> 0
                  | _ -> -1)
   | h ::t -> (function [] -> 1
                       | h2::t2 -> compare_lengths t t2);;

(*recursiva terminal*)
let rec compare_lengths l1 l2 =
   match l1, l2 with
      [], [] ->0
    | [], _ -> -1
    | _, [] -> 1
    | h1::t1, h2::t2 -> compare_lengths t1 t2;;
    
(*compare_length_with*)
let rec compare_length_with l n = match l with
   [] -> if n = 0 then 0 else -1
 | _ :: t -> compare_length_with t (n - 1);;
 
 (*init*)
 (*recursiva no terminal y al revés*)
 let rec init n f =
     if n<0 then raise (Invalid_argument "init")
     else if n != 0 then f (n-1) :: init (n-1) f
          else [];;
  
 (*recursiva terminal*)
 let init i f =
     if i<0 then raise (Invalid_argument "init")
     else let rec init i accum = match i with
               0 -> accum
             | n -> init (i-1) (f (i-1) :: accum)
     in init i [];;                         

(*nth*)
let rec nth l i = match l,i with
     [],_ -> raise(Failure "nth")
     | h::t,i -> if i<0 then raise(Invalid_argument "nth")
                 else if i=0 then h else nth t (i-1) ;;
    
(*append*)
let rec append l1 l2 =  match l1 with
    [] -> l2
  | h::t -> h::append t l2;;

(*rev_append*)
let rec rev_append l1 l2 = match l1 with
    [] -> l2
  | h::t -> rev_append t (h::l2);;
 
 (*rev*)
let rev l = rev_append l [];;

(*concat*)
let concat l =
  let rec concataux acc l = match l with
      [] -> rev acc
    | []::t -> concataux acc t
    | (h::t1)::t2 -> concataux (h::acc) (t1::t2)
in concataux [] l;;


(*flatten*)	
let flatten l = concat l;;
		
(*split*)		
let rec split l = match l with
    [] -> ([],[])
   | (h1, h2)::t -> let p1,p2 = split t in 
                    h1::p1, h2::p2;;
                    
(*combine*)                
let rec combine l1 l2 = match l1,l2 with
    [], [] -> []
   | _,[] | [],_ -> raise(Invalid_argument "combine")
   | h1::t1, h2::t2 -> (h1,h2) :: (combine t1 t2);;
    
(*map*)
let rec map f l = match l with
   [] -> []
  | h::t -> (f h) :: (map f t);;

(*map2*) 
let rec map2 f l1 l2 = match l1,l2 with
   [], [] -> []
 | _,[] | [],_ -> raise(Invalid_argument "map2")
 | h1::t1, h2::t2 -> (f h1 h2) :: (map2 f t1 t2);;
 
(*rev_map*)
let rev_map f l =
  let rec rev_map_aux acc l = match l with
    [] -> acc
  | h::t -> rev_map_aux (f h :: acc) t
  in rev_map_aux [] l;;
    
(*for_all*)
(*recursiva no terminal*)
let rec for_all f l = match l with
  [] -> true
| h::t -> f h && for_all f t;;

(*recursiva terminal*)
let for_all f l =
    let rec forallaux f l acc = match l with
      [] -> acc
    | h::t -> if acc then forallaux f t (acc && f h) else false
in forallaux f l true;;

(*exists*)
(*recursiva no terminal*)
let rec exists f l = match l with
  [] -> false
| h::t -> f h || exists f t;;

(*recursiva terminal*)
let exists f l =
  let rec existsaux f l acc = match l with
    [] -> acc
  | h::t -> if acc then true else existsaux f t (acc || f h)
in existsaux f l false;;

(*mem*)
let rec mem a l = match l with
   [] -> false
 | h::t -> if h=a then true else mem a t;;

(*find*)
let rec find f l = match l with
   [] -> raise(Failure "Not_found")
 | h::t -> if f h then h else find f t;;

(*filter*)
(*recursiva no terminal*)
let rec filter f l = match l with
  [] -> []
| h::t -> if f h then h::(filter f t) else filter f t;;

(*recursiva terminal*)
let filter f l =
  let rec filteraux acc l = match l with
      [] -> rev acc
    | h::t -> if f h then filteraux (h::acc) t else filteraux acc t
in filteraux [] l;;

(*find_all*)
let find_all f l = filter f l;;

(*partition*)
(*recursiva no terminal*)
let rec partition f l = match l with
   [] -> ([],[])
 | h::t -> let (par1,par2) = partition f t in
           if f h then (h::par1, par2) 
           else (par1,h::par2);;
    
(*recursiva terminal*)
let rec partition f l = 
  let rec partitionaux acc1 acc2 l = match l with
    | [] -> (rev acc1, rev acc2)
    | h::t -> if f h then partitionaux (h::acc1) acc2 t else partitionaux acc1 (h::acc2) t
in partitionaux [] [] l;;

(*fold_left*)
let rec fold_left f acc l = match l with
| [] -> acc
| h::t -> fold_left f (f acc h) t;;

(*fold_right*)
let rec fold_right f l init = match l with
| [] -> init
| h::t -> f h (fold_right f t init);;

(*assoc*)
let rec assoc key l = match l with
| [] -> raise(Failure "Not_found")
| h::t -> if fst h=key then snd h else assoc key t;;

(*mem_assoc*)
let rec mem_assoc key l = match l with
| [] -> false
| h::t -> if fst h=key then true else mem_assoc key t;;

(*remove_assoc*)
let remove_assoc key l = 
  let rec assocaux key acc l =
      match l with
    | [] -> rev acc
    | h::t -> if fst h=key then append (rev acc) t else assocaux key (h::acc) t
in assocaux key [] l;;

