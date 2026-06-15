open Bdd

let num_of_cores = 4

let bddvars_of n =
  let rec bddvars_of_iter n i =
    let n' = n lsr 1 in
    if n' > 0 then bddvars_of_iter n' (i+1)
    else i
  in
    bddvars_of_iter n 1

let init : int -> int -> int -> unit
=fun n1 n2 n3 -> 
  prerr_endline "initializing BDDs";
  let v1 = bddvars_of n1 in
  let v2 = bddvars_of n2 in
  let v3 = bddvars_of n3 in
    init 1000000 10000 num_of_cores;
    init_env v1 v2 v3

let c = ref 0
let gbcall () =
  c := !c + 1;
  if !c > 500 then
  begin
    c := 0;
	Gc.major ();
  end

let add (n1,n2,n3) =
(*  gbcall (); *)
  let _ = bdd_add n1 n2 n3 in
    ()

let add_set (n1,n2,ns) =
  if Array.length ns = 0 then ()
  else bdd_add_set n1 n2 ns

let remove (n1,n2,n3) =
  let _ = bdd_remove n1 n2 n3 in
    ()

let printstat () =
  print_stat ()

(* subset : same src and dst *)
let subset_sd n1 n2 =
  bdd_find_sub n1 n2
	
let mem (n1,n2,n3) = 
  bdd_mem n1 n2 n3

let mem_sub n3 =
  bdd_mem_sub n3

let next = bdd_next
let finish () = finish ()

let save filename = bdd_save filename
let load filename = bdd_load filename
