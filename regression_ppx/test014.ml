let () = print_endline "test014"

module _ : sig
  (* type 'a0 aaa_fuly = A2 of 'a0 [@@deriving gt ~options:{ gmap }]
  type 'a0 bbb_fuly = B2 of 'a0 [@@deriving gt ~options:{ gmap }] *)

  type 'a aaa = 'a * 'a bbb GT.list
  and 'a bbb = 'a aaa GT.list

  (* [@@deriving gt ~options:{ gmap }] *)
  class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] aaa_t :
    object
      method virtual c_AAA : 'inh -> 'extra -> 'syn
    end

  val gcata_aaa : (_, 'a, 'sa, 'inh, 'a aaa, 'syn) #aaa_t -> 'inh -> 'a aaa -> 'syn

  class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] bbb_t :
    object
      inherit ['ia aaa, 'a aaa, 'sa aaa, 'inh, 'extra, 'syn] GT.list_t
    end

  val gcata_bbb : (_, 'a, 'sa, 'inh, 'a bbb, 'syn) #bbb_t -> 'inh -> 'a bbb -> 'syn

  class ['a, 'a_2, 'extra_aaa, 'syn_aaa] gmap_aaa_t_stub :
    ((unit -> 'a -> 'a_2) -> unit -> 'a aaa -> 'a_2 aaa)
    * ((unit -> 'a -> 'a_2) -> unit -> 'a bbb -> 'a_2 bbb)
    -> (unit -> 'a -> 'a_2)
    -> object
         constraint 'extra_aaa = 'a aaa
         constraint 'syn_aaa = 'a_2 aaa
         method c_AAA : unit -> 'extra_aaa -> 'a_2 aaa
       end

  class ['a, 'a_2, 'extra_aaa, 'syn_aaa] gmap_aaa_t :
    (unit -> 'a -> 'a_2)
    -> (unit -> 'a aaa -> 'a_2 aaa)
    -> object
         constraint 'extra_aaa = 'a aaa
         constraint 'syn_aaa = 'a_2 aaa
         method c_AAA : unit -> 'extra_aaa -> 'a_2 aaa
       end

  class ['a, 'a_2, 'extra_bbb, 'syn_bbb] gmap_bbb_t_stub :
    ((unit -> 'a -> 'a_2) -> unit -> 'a aaa -> 'a_2 aaa)
    * ((unit -> 'a -> 'a_2) -> unit -> 'a bbb -> 'a_2 bbb)
    -> (unit -> 'a -> 'a_2)
    -> object
         constraint 'extra_bbb = 'a bbb
         constraint 'syn_bbb = 'a_2 bbb
         inherit ['a aaa, 'a_2 aaa, 'extra_bbb, 'syn_bbb] GT.gmap_list_t
       end

  class ['a, 'a_2, 'extra_bbb, 'syn_bbb] gmap_bbb_t :
    (unit -> 'a -> 'a_2)
    -> (unit -> 'a bbb -> 'a_2 bbb)
    -> object
         constraint 'extra_bbb = 'a bbb
         constraint 'syn_bbb = 'a_2 bbb
         inherit ['a aaa, 'a_2 aaa, 'extra_bbb, 'syn_bbb] GT.gmap_list_t
       end

  val gmap_aaa : ('a -> 'a_2) -> 'a aaa -> 'a_2 aaa
  val gmap_bbb : ('a -> 'a_2) -> 'a bbb -> 'a_2 bbb

  val aaa
    : ( (_, 'a, 'sa, 'inh, 'a aaa, 'syn) #aaa_t -> 'inh -> 'a aaa -> 'syn
      , < gmap : ('a -> 'a_2) -> 'a aaa -> 'a_2 aaa >
      , (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
         -> ('a1_i, 'a1, 'a1_s, 'inh2, 'a1 aaa, 'syn3) #aaa_t)
        -> (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
            -> ('a4_i, 'a4, 'a4_s, 'inh5, 'a4 bbb, 'syn6) #bbb_t)
        -> ('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6) )
      GT.t

  val bbb
    : ( (_, 'a, 'sa, 'inh, 'a bbb, 'syn) #bbb_t -> 'inh -> 'a bbb -> 'syn
      , < gmap : ('a -> 'a_2) -> 'a bbb -> 'a_2 bbb >
      , (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
         -> ('a1_i, 'a1, 'a1_s, 'inh2, 'a1 aaa, 'syn3) #aaa_t)
        -> (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
            -> ('a4_i, 'a4, 'a4_s, 'inh5, 'a4 bbb, 'syn6) #bbb_t)
        -> ('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6) )
      GT.t
  (*
  val fix_aaa
    :  (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
        -> ('a1_i, 'a1, 'a1_s, 'inh2, 'a1 aaa, 'syn3) #aaa_t)
    -> (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
        -> ('a4_i, 'a4, 'a4_s, 'inh5, 'a4 bbb, 'syn6) #bbb_t)
    -> ('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6) *)

  val fix_aaa
    :  (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
        -> ('a1_i, 'a1, 'a1_s, 'inh2, 'a1 aaa, 'syn3) #aaa_t)
    -> (('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
        -> ('a4_i, 'a4, 'a4_s, 'inh5, 'a4 bbb, 'syn6) #bbb_t)
    -> ('inh2 -> 'a1 aaa -> 'syn3) * ('inh5 -> 'a4 bbb -> 'syn6)
end = struct
  (* type 'a0 aaa_fuly = A2 of 'a0 [@@deriving gt ~options:{ gmap }]
  type 'a0 bbb_fuly = B2 of 'a0 [@@deriving gt ~options:{ gmap }] *)

  type 'a aaa = 'a * 'a bbb GT.list
  and 'a bbb = 'a aaa GT.list [@@deriving gt ~options:{ gmap }]

  let __ (type a b) : (a -> b) -> a aaa -> b aaa = GT.gmap aaa
  let __ (type a b) : (a -> b) -> a bbb -> b bbb = GT.gmap bbb

  let (_ :
        (('a -> 'b -> 'c aaa -> 'd) * ('e -> 'f -> 'g list -> 'h)
         -> 'a
         -> ('i, 'c, 'j, 'b, 'c aaa, 'd) #aaa_t)
        -> (('a -> 'b -> 'c aaa -> 'd) * ('e -> 'f -> 'g list -> 'h)
            -> 'e
            -> < c_Cons : 'f -> 'g list -> 'g -> 'g list -> 'h ; c_Nil : 'f -> 'g list -> 'h ; .. >)
        -> ('a -> 'b -> 'c aaa -> 'd) * ('e -> 'f -> 'g list -> 'h))
    =
    fix_aaa
  ;;
end
