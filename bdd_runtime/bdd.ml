    external init : int -> int -> int -> unit         = "mlbdd_bdd_init"
    external init_env : int -> int -> int -> unit = "mlbdd_muddy_init"
    external finish : unit -> unit         = "mlbdd_muddy_finished"

    external set_cacheratio : int -> int   = "mlbdd_bdd_setcacheratio"
    
    external bdd_add : int -> int -> int -> unit = "mlbdd_muddy_add"
    external bdd_add_set : int -> int -> int array -> unit = "mlbdd_muddy_add_set"
    external bdd_remove : int -> int -> int -> unit = "mlbdd_muddy_remove"
    
    external bdd_mem : int -> int -> int -> bool = "mlbdd_muddy_mem"

    external bdd_find_sub : int -> int -> bool = "mlbdd_muddy_find_sub_bdd"
    external bdd_mem_sub : int -> bool = "mlbdd_muddy_mem_sub"
    external bdd_next : unit -> int = "mlbdd_muddy_next"

    external bdd_save : string -> unit = "mlbdd_bdd_fnsave"
    external bdd_load : string -> unit = "mlbdd_bdd_fnload"

    external print_stat : unit -> unit              = "mlbdd_muddy_printstat"
