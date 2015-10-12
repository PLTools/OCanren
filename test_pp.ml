open GT
open MiniKanren 

type t =
    A of int
  | B of string
  | C of t * t
class type virtual ['inh, 'syn] t_tt =
  object
    method c_A : 'inh -> ('inh, t, 'syn, < >) GT.a -> int -> 'syn
    method c_B : 'inh -> ('inh, t, 'syn, < >) GT.a -> string -> 'syn
    method c_C :
      'inh -> ('inh, t, 'syn, < >) GT.a -> ('inh, t, 'syn, < >) GT.a ->
        ('inh, t, 'syn, < >) GT.a -> 'syn
    method t_t : 'inh -> t -> 'syn
  end
let (t : (('inh, 'syn) #t_tt -> 'inh -> t -> 'syn, unit) GT.t) =
  let rec t_gcata trans inh subj =
    let rec self = t_gcata trans
    and tpo = object  end in
    match subj with
      A p0 -> trans#c_A inh (GT.make self subj tpo) p0
    | B p0 -> trans#c_B inh (GT.make self subj tpo) p0
    | C (p0, p1) ->
        trans#c_C inh (GT.make self subj tpo) (GT.make self p0 tpo)
          (GT.make self p1 tpo)
  in
  {GT.gcata = t_gcata; GT.plugins = ()}
class virtual ['inh, 'syn] t_t =
  object (this)
    method virtual c_A : 'inh -> ('inh, t, 'syn, < >) GT.a -> int -> 'syn
    method virtual c_B : 'inh -> ('inh, t, 'syn, < >) GT.a -> string -> 'syn
    method virtual c_C :
      'inh -> ('inh, t, 'syn, < >) GT.a -> ('inh, t, 'syn, < >) GT.a ->
        ('inh, t, 'syn, < >) GT.a -> 'syn
    method t_t = GT.transform t this
  end
class type minikanren_t_env_tt = object  end
class minikanren_proto_t env =
  object (this)
    inherit [MiniKanren.Env.t, string] t_t
    method c_C inh subj p0 p1 =
      (("C (" ^ p0.GT.fx inh) ^ ", " ^ p1.GT.fx inh) ^ ")"
    method c_B inh subj p0 =
      ("B (" ^ GT.transform string (new minikanren_string_t) inh p0) ^ ")"
    method c_A inh subj p0 =
      ("A (" ^ GT.transform int (new minikanren_int_t) inh p0) ^ ")"
  end
class minikanren_t_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [MiniKanren.Env.t, string] t_t
    inherit minikanren_proto_t self
    initializer (:=) self (this :> minikanren_t_t)
  end
let (t :
 (('inh, 'syn) #t_tt -> 'inh -> t -> 'syn, < minikanren : MiniKanren.Env.t -> t -> string >)
   GT.t) =
  {GT.gcata = t.GT.gcata;
   GT.plugins =
     object method minikanren = GT.transform t (new minikanren_t_t) end}
