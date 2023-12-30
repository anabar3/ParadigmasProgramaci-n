
let append a1 a2 =
  let len1 = ref (Array.length a1) in
  let len2 = ref (Array.length a2) in

  if !len1 + !len2 > Sys.max_array_length then
    raise (Invalid_argument "append");

  let result = Array.init (!len1 + !len2) (fun i ->
    if i < !len1 then a1.(i)
    else a2.(i - !len1)
  ) in

  result;;
  
  
let sub a pos len =
   let length = ref (Array.length a) in
   if pos < 0 || pos + len > !length then
    raise (Invalid_argument "sub");
   
   let result = Array.init len (fun i -> a.(pos+i)) in
   result;;
   
let copy a =
  let length = ref (Array.length a) in
  let result = Array.init !length (fun i ->
    a.(i)
  ) in
  result;;
 
let fill a pos len x =
   let length = ref (Array.length a) in
   if pos < 0 || pos + len > !length then
     raise (Invalid_argument "fill");
   for i = pos to (!length - 1) do
    a.(i) <- x
  done;;
  
  
let blit src src_pos dest dest_pos len =
  let src_len = ref (Array.length src) in
  let dest_len = ref (Array.length dest) in

  if src_pos < 0 || src_pos + len > !src_len || dest_pos < 0 || dest_pos + len > !dest_len then
    raise (Invalid_argument "invalid positions or length for blit");

  for i = 0 to len - 1 do
    dest.(dest_pos + i) <- src.(src_pos + i)
  done;;
  
let to_list a =
  let length = ref (Array.length a) in
  let result = ref [] in
  for i = 0 to !length - 1 do
    result := a.(!length - 1 - i) :: !result
  done;
  !result;;


let iter f a =
  let len = Array.length a in
  for i = 0 to len - 1 do
    f a.(i)
  done;;

let fold_left f init a =
  let length = Array.length a in
  let acc = ref init in
  for i = 0 to length - 1 do
    acc := f !acc a.(i)
  done;
  !acc;;

let for_all p a =
  let length = Array.length a in
  let result = ref true in
  for i = 0 to length - 1 do
    if not (p a.(i)) then result := false
  done;
  !result;;

let exists p a =
  let length = Array.length a in
  let result = ref false in
  for i = 0 to length - 1 do
    if p a.(i) then result := true
  done;
 !result;;

let find_opt p a =
  let length = Array.length a in
  let result = ref None in
  let i = ref 0 in
  while !i < length && !result = None do
    if p a.(!i) then result := Some a.(!i);
    i := !i + 1
  done;
 !result;;


 
