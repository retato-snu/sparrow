val init      : int -> int -> int -> unit
val init_env  : int -> int -> int -> unit
val finish      : unit -> unit

(** Sets the cache ratio for the operator caches
 @param new cache ratio (nodesize / cacheratio) = cachesize
 @return old cache ratio
*)
val set_cacheratio : int -> int

val bdd_add : int -> int -> int -> unit
val bdd_add_set : int -> int -> int array -> unit
val bdd_remove : int -> int -> int -> unit

val bdd_mem : int -> int -> int -> bool

val bdd_find_sub : int -> int -> bool
val bdd_mem_sub : int -> bool
val bdd_next : unit -> int

val bdd_save : string -> unit
val bdd_load : string -> unit

val print_stat      : unit -> unit
