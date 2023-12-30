open MinPrioQueue  


let with_negatives matrix =
  let n = Array.length matrix in
  let rec check_elements i j =
    if i = n then false else 
      if j = n then check_elements (i+1) 0 else 
        if matrix.(i).(j) <> None && matrix.(i).(j) < Some 0 then true
        else check_elements i (j+1)
  in check_elements 0 0
  

let check_square matrix =
  let rows = Array.length matrix in
    let rec check_lines i =
      if i = rows then true
      else if Array.length matrix.(i) <> rows then false
      else check_lines (i+1)
  in check_lines 0



let dijkstra graph =
  if with_negatives graph = true || check_square graph = false then raise (Invalid_argument "dijkstra")   
  else
    let n = Array.length graph in
    let dist = Array.map (fun row -> Array.copy row) (Array.copy graph) in (*hacer copia*)
    for i = 0 to n-1 do
      dist.(i).(i) <- Some 0 
    done;
  
    for start = 0 to n-1 do (*bucle externo: recorre todos los orígenes*)
      let unvisited = Array.init (n) (fun _ -> true) in 
      unvisited.(start) <- false;
      let q = new_queue () in 
      insert q (Some 0) start;
      let minNode = ref None in
      while (minNode := extract_opt q; !minNode <> None) do 
        let minNode = snd (Option.get !minNode) in
        unvisited.(minNode) <- false;

        for final = 0 to n-1 do (*bucle interno: recorre todos los destinos*)
          if unvisited.(final) && (dist.(start).(minNode) <> None) && (dist.(minNode).(final) <> None) then
            let new_dist_start_final = Option.get dist.(start).(minNode) + Option.get dist.(minNode).(final) in 
            match dist.(start).(final) with
            | None -> dist.(start).(final) <- Some new_dist_start_final; 
                      insert q (Some new_dist_start_final) final
            | Some value ->
                if value >= new_dist_start_final then
                  (dist.(start).(final) <- Some new_dist_start_final; 
                  insert q (Some new_dist_start_final) final)
                else () 
          else ()
        done;
      done;
    done;
  dist;;
