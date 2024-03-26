let ( <.> ) f g x = f (g x)
let rec zed f x = f (zed f) x
let fac = zed (fun self x -> if x <= 1 then 1 else self (x - 1) * x)

open OCanren

let rework_logic = Reifier.rework

module TestNat = struct
  open Std.Nat

  type ground = Std.Nat.ground
  type logic = Std.Nat.logic
  type injected = Std.Nat.groundi

  let fmap eta = GT.gmap t eta

  let reify_with_compose : (groundi, Std.Nat.logic) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.compose
        Reifier.reify
        (let* fr = self in
         let rec foo = function
           | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
           | Value x -> Value (GT.gmap t fr x)
         in
         Env.Monad.return foo))
  ;;

  let reify : (groundi, Std.Nat.logic) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* rself = self in
      let* r = OCanren.reify in
      let rec foo x =
        match r x with
        | Value x -> Value (GT.gmap t rself x)
        | Var (v, xs) -> Var (v, Stdlib.List.map (GT.gmap logic' (GT.gmap t foo)) xs)
      in
      Env.Monad.return foo)
  ;;

  let fmapt : 'a 'b. ('a, 'b) Reifier.t -> 'a t Env.m -> 'b t Env.m =
    fun fa subj -> Env.Monad.(return (GT.gmap t) <*> fa <*> subj)
  ;;

  let (_prj_exn : (injected, ground) Reifier.t) =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self -> OCanren.prj_exn <..> Env.Monad.chain (fmapt self))
  ;;

  (* non-essential, testing-only stuff *)
  module _ (R : sig
      val self : (injected, logic) Reifier.t
      val chain : ('a Env.m -> 'b Env.m) -> ('a -> 'b) Env.m
    end) =
  struct
    open R

    let ( <..> ) = Env.Monad.( <..> )
    let (_ : (injected t, logic t) Reifier.t) = Env.Monad.chain (fmapt self)
    (* let (_ : int) = Reifier.fcomap OCanren.reify *)

    let (_ : ('a OCanren.logic, 'b) Reifier.t -> ('a ilogic, 'b) Reifier.t) =
      Reifier.compose OCanren.reify
    ;;

    let (_ : ('a, 'b ilogic) Reifier.t -> ('a, 'b OCanren.logic) Reifier.t) =
      fun f -> Reifier.compose f OCanren.reify
    ;;

    let (_ : ('a, 'b ilogic) Reifier.t -> ('a, 'b OCanren.logic) Reifier.t) =
      fun f -> Reifier.compose f OCanren.reify
    ;;

    let (_ : (logic t, 'a) Reifier.t -> (injected t, 'a) Reifier.t) =
      fun x -> Reifier.compose (Env.Monad.chain (fmapt self)) x
    ;;

    let (_ :
          (int -> 'a logic' list -> _) -> ('a OCanren.logic -> _) -> 'a ilogic -> 'b Env.m)
      =
      fun onvar onvalue x ->
      Reifier.fix (fun self ->
        let open Env.Monad.Syntax in
        let* rsh = Reifier.reify in
        Env.Monad.return
          (match rsh x with
           | Var (n, xs) -> onvar n xs
           | Value _ as x -> onvalue x))
    ;;

    let (_ : (injected t ilogic -> logic t) Env.m) =
      OCanren.prj_exn <..> chain (fmapt self)
    ;;

    let (_ : ('a, 'b) Reifier.t -> ('a t ilogic, 'b t) Reifier.t) =
      fun self ->
      (OCanren.prj_exn : ('d ilogic, 'd) Reifier.t)
      <..> (chain (fmapt self) : ('a t, 'b t) Reifier.t)
    ;;

    let remove_monad : 'a Env.m -> 'a = fun _ -> assert false

    type 'a m = 'a Env.m

    let list_mapm : f:('a m -> 'b m) -> 'a list -> 'b list m = fun ~f x -> assert false
    let return = Env.Monad.return

    type ('a, 'b) arr = 'a -> 'b

    let (_ : ('a, 'b) arr -> 'a t -> 'b t) = fmap
    let (_ : ('a, 'b) arr m -> 'a t m -> 'b t m) = fmapt

    let rework_logic
      : 'a 'b. fv:('a -> 'b) -> fdeq:('a logic' -> 'b logic') -> 'a logic' -> 'b logic'
      =
      fun ~fv ~fdeq -> function
      | Var (v, xs) -> Var (v, Stdlib.List.map fdeq xs)
      | Value t -> Value (fv t)
    ;;

    let __ : 'a 'b. ('a -> 'b) m -> ('a t logic' -> 'b t logic') m =
      fun fa ->
      let open Env.Monad.Syntax in
      let* rf = fa in
      let rec foo x = rework_logic ~fdeq:foo ~fv:(fmap rf) x in
      return foo
    ;;

    let rework_logic2
      : 'a 'b.
      fv:('a m -> 'b m) -> fdeq:('a logic' m -> 'b logic' m) -> 'a logic' m -> 'b logic' m
      =
      fun ~fv ~fdeq x ->
      let open Env.Monad.Syntax in
      let* x = x in
      match x with
      | Var (v, xs) -> return @@ Var (v, remove_monad @@ list_mapm ~f:fdeq xs)
      | Value t -> return @@ Value (remove_monad (fv (Env.Monad.return t)))
    ;;

    let __ : 'a 'b. ('a -> 'b) m -> 'a t logic' m -> 'b t logic' m =
      fun fa ->
      let open Env.Monad.Syntax in
      let rec foo x = rework_logic2 ~fdeq:foo ~fv:(fmapt fa) x in
      foo
    ;;

    let __ : 'a 'b. ('a -> 'b) m -> ('a t logic' -> 'b t logic') m =
      fun fa ->
      let open Env.Monad.Syntax in
      let rec foo x = rework_logic2 ~fdeq:foo ~fv:(fmapt fa) x in
      chain foo
    ;;

    let reify : ('a, 'b) Reifier.t -> ('a t ilogic, 'b t OCanren.logic) Reifier.t =
      fun fa ->
      let open Env.Monad.Syntax in
      Reifier.fix (fun self ->
        Reifier.reify
        <..>
        let rec foo x = rework_logic2 ~fdeq:foo ~fv:(fmapt fa) x in
        chain foo)
    ;;

    let rework_logic3
      : 'a 'b.
      fv:('a m -> 'b m) -> fdeq:('a logic' m -> 'b logic' m) -> 'a logic' m -> 'b logic' m
      =
      fun ~fv ~fdeq x ->
      let open Env.Monad.Syntax in
      let* x = x in
      match x with
      | Var (v, xs) ->
        let* diseq = list_mapm ~f:fdeq xs in
        return @@ Var (v, diseq)
      | Value t ->
        let* inner = fv (Env.Monad.return t) in
        return @@ Value inner
    ;;

    let reify : ('a, 'b) Reifier.t -> ('a t ilogic, 'b t OCanren.logic) Reifier.t =
      fun fa ->
      let open Env.Monad.Syntax in
      Reifier.fix (fun self ->
        Reifier.reify
        <..>
        let rec foo x = rework_logic2 ~fdeq:foo ~fv:(fmapt fa) x in
        chain foo)
    ;;
  end

  let (reify : (injected, logic) Reifier.t) =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.reify
      <..> chain
             (let rec foo x = rework_logic ~fv:(fmapt self) foo x in
              foo))
  ;;

  let (reify : (injected, logic) Reifier.t) =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt self))))
  ;;

  let test_reify reify =
    let goal q = fresh x (q === inji @@ S x) in
    let xs : logic Stream.t = OCanren.(run q) goal (fun rr -> rr#reify reify) in
    match Stream.take xs with
    | [ Value (S (Var (_, _))) ] -> true
    | _ -> false
  ;;

  (* ************************************* *)
  let prj_exn : (groundi, Std.Nat.ground) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* rself = self in
      let* r = OCanren.prj_exn in
      let foo x = GT.gmap t rself (r x) in
      Env.Monad.return foo)
  ;;

  let test_prj_exn () =
    let goal q = fresh x (q === inji @@ S (inji O)) in
    let xs : ground Stream.t = OCanren.(run q) goal (fun rr -> rr#reify prj_exn) in
    match Stream.take xs with
    | [ S O ] -> true
    | _ -> false
  ;;

  let%test _ = test_prj_exn ()
  let%test _ = test_reify reify
  let test () = test_reify reify
end

(* example of reifiers for custom types *)
module TestOption = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
  [@@deriving gt ~options:{ gmap }]

  type 'a ground = 'a t
  type 'a logic = 'a t OCanren.logic [@@deriving gt ~options:{ gmap }]
  type 'a injected = 'a t OCanren.ilogic

  let fmapt : 'a 'b. ('a, 'b) Reifier.t -> 'a t Env.m -> 'b t Env.m =
    fun fa subj -> Env.Monad.(return (GT.gmap t) <*> fa <*> subj)
  ;;

  let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b ground) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ -> OCanren.prj_exn <..> Env.Monad.chain (fmapt fa))
  ;;

  (* test projection *)
  let%test _ =
    let goal q = q === inji @@ Some (inji 42) in
    let xs : int ground Stream.t =
      OCanren.(run q) goal (fun rr -> rr#reify (Std.Option.prj_exn Reifier.prj_exn))
    in
    match Stream.take xs with
    | [ Some 42 ] -> true
    | _ -> false
  ;;

  let test1 : _ =
    fun fa fv ->
    let open Env.Monad in
    let _xxx : _ = fmapt fa in
    Reifier.reify <..> chain (zed (rework_logic ~fv))
  ;;

  let reify : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b logic) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ -> Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt fa))))
  ;;

  (* test reification *)
  let%test _ =
    let goal q = q === inji @@ Some (inji 42) in
    let xs : int OCanren.logic logic Stream.t =
      OCanren.(run q) goal (fun rr -> rr#reify (reify Reifier.reify))
    in
    match Stream.take xs with
    | [ Value (Some (Value 42)) ] -> true
    | _ -> false
  ;;

  let%test _ =
    let goal q = success in
    let xs : int OCanren.logic logic Stream.t =
      OCanren.(run q) goal (fun rr -> rr#reify (reify Reifier.reify))
    in
    match Stream.take xs with
    | [ Var (_, _) ] -> true
    | _ -> false
  ;;

  let%test _ =
    let goal q = fresh x (q === inji @@ Some x) in
    let xs : int OCanren.logic logic Stream.t =
      OCanren.(run q) goal (fun rr -> rr#reify (reify Reifier.reify))
    in
    match Stream.take xs with
    | [ Value (Some (Var (_, _))) ] -> true
    | _ -> false
  ;;
end

module TestNestedOption = struct
  type 'a t = 'a option
  type ground = ground t
  type logic = logic t OCanren.logic
  type ilogic = ilogic t OCanren.ilogic

  let reify_with_compose : (ilogic, logic) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* rself = self in
      Reifier.compose
        Reifier.reify
        (let rec foo = function
           | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
           | Value t -> Value (Option.map rself t)
         in
         Env.Monad.return foo))
  ;;

  let _reify_old : (ilogic, logic) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* rself = self in
      let* r = Reifier.reify in
      let rec foo x =
        match r x with
        | Var (v, xs) ->
          Var (v, Stdlib.List.map (GT.gmap OCanren.logic (GT.gmap Std.Option.t rself)) xs)
        | Value t -> Value (Option.map rself t)
      in
      Env.Monad.return foo)
  ;;

  let fmapt : 'a 'b. ('a, 'b) Reifier.t -> 'a t Env.m -> 'b t Env.m =
    fun fa subj -> Env.Monad.(return Option.map <*> fa <*> subj)
  ;;

  let reify : (ilogic, logic) Reifier.t =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt self))))
  ;;

  let test reifier =
    let goal q = fresh x (q === inji @@ Some x) in
    let xs : logic Stream.t = OCanren.(run q) goal (fun rr -> rr#reify reifier) in
    match Stream.take xs with
    | [ Value (Some (Var (_, _))) ] -> true
    | _ -> false
  ;;

  let%test _ = test reify_with_compose
  let%test _ = test reify
end

(* Natural numbers not being tied-in-the-knot *)
module TestNat2 = struct
  type 'a t =
    | O
    | S of 'a
  [@@deriving gt ~options:{ gmap }]

  type 'a ground = 'a t
  type 'a logic = 'a t OCanren.logic
  type 'a injected = 'a t ilogic

  (* old *)
  let reify_open : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b logic) Reifier.t =
    fun fa ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun _self ->
      let* (ra : 'a -> 'b) = fa in
      let* r = OCanren.reify in
      let rec foo x =
        match r x with
        | Value x -> Value (GT.gmap t ra x)
        | Var (v, xs) -> Var (v, Stdlib.List.map (GT.gmap OCanren.logic (GT.gmap t ra)) xs)
      in
      Env.Monad.return foo)
  ;;

  let test reifier =
    let goal q = fresh x (q === inji @@ S x) in
    let xs : _ Stream.t = OCanren.(run q) goal (fun rr -> rr#reify reifier) in
    match Stream.take xs with
    | [ Value (S (Var (_, _))) ] -> true
    | _ -> false
  ;;

  let%test _ = test (reify_open OCanren.reify)

  (* old closed reifier *)
  let reify_old_style : (('a injected as 'a), ('b logic as 'b)) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* r = OCanren.reify in
      let* rself = self in
      let rec foo x =
        match r x with
        | Value x -> Value (GT.gmap t rself x)
        | Var (v, xs) ->
          Var (v, Stdlib.List.map (GT.gmap OCanren.logic (GT.gmap t rself)) xs)
      in
      Env.Monad.return foo)
  ;;

  let%test _ = test reify_old_style

  let fmapt : 'a 'b. ('a, 'b) Reifier.t -> 'a t Env.m -> 'b t Env.m =
    fun fa subj -> Env.Monad.(return (GT.gmap t) <*> fa <*> subj)
  ;;

  let prj_exn_open : ('a, 'b) Reifier.t -> ('a injected, 'b ground) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _self -> OCanren.prj_exn <..> Env.Monad.chain (fmapt fa))
  ;;

  let prj_exn_knotted : (('a injected as 'a), ('b ground as 'b)) Reifier.t =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self -> OCanren.prj_exn <..> Env.Monad.chain (fmapt self))
  ;;

  let reify_open_new : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b logic) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ -> Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt fa))))
  ;;

  let reifynew : (('a injected as 'a), ('b logic as 'b)) Reifier.t =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt self))))
  ;;

  let%test _ = test reifynew
end

module TestList = struct
  type nonrec ('a, 'b) t =
    | [] [@name "nil"]
    | ( :: ) of 'a * 'b [@name "cons"]
  [@@deriving gt ~options:{ gmap }]

  type 'a ground = ('a, 'a ground) t
  type 'a logic = ('a, 'a logic) t OCanren.logic
  type 'a injected = ('a, 'a injected) t ilogic

  let reify_old : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b logic) Reifier.t =
    fun fa ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* (ra : 'a -> 'b) = fa in
      let* rself = self in
      let* r = OCanren.reify in
      let rec foo x =
        match r x with
        | Value x -> Value (GT.gmap t ra rself x)
        | Var (v, xs) ->
          let (_ : ('a, 'a injected) t OCanren.logic list) = xs in
          Var (v, Stdlib.List.map (GT.gmap OCanren.logic (GT.gmap t ra rself)) xs)
      in
      Env.Monad.return foo)
  ;;

  let prj_exn_old : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b ground) Reifier.t =
    fun fa ->
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      let* (ra : 'a -> 'b) = fa in
      let* rself = self in
      let* r = OCanren.prj_exn in
      let rec foo x =
        match r x with
        | x -> GT.gmap t ra rself x
      in
      Env.Monad.return foo)
  ;;

  (* new *)

  let fmapt
    : 'a 'b 'c 'd.
    ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> ('a, 'c) t Env.m -> ('b, 'd) t Env.m
    =
    fun fa fb subj ->
    let open Env.Monad in
    return (GT.gmap t) <*> fa <*> fb <*> subj
  ;;

  let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b ground) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self -> OCanren.prj_exn <..> Env.Monad.chain (fmapt fa self))
  ;;

  let reify : 'a 'b. ('a, 'b) Reifier.t -> ('a injected, 'b logic) Reifier.t =
    fun fa ->
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun self ->
      Reifier.reify <..> chain (zed (rework_logic ~fv:(fmapt fa self))))
  ;;

  (* tests *)
  let test1_reify reifier =
    let goal q = fresh x (q === inji @@ (x :: inji [])) in
    let xs = OCanren.(run q) goal (fun rr -> rr#reify reifier) in
    match Stream.take xs with
    | [ Value (Var (_, _) :: Value []) ] -> true
    | _ -> false
  ;;

  let test2_prj reifier =
    let goal q = fresh x (q === inji @@ (inji 42 :: inji [])) in
    let xs : _ Stream.t = OCanren.(run q) goal (fun rr -> rr#reify reifier) in
    match Stream.take xs with
    | [ [ 42 ] ] -> true
    | _ -> false
  ;;

  let%test "reify_old" = test1_reify (reify_old OCanren.reify)
  let%test "prj_old" = test2_prj (prj_exn_old OCanren.prj_exn)
  let%test "reify_new" = test1_reify (reify OCanren.reify)
  let%test "prj_new" = test2_prj (prj_exn OCanren.prj_exn)
end

module _ = struct
  type ('a, 'b) t = ('a, 'b) Std.Pair.t [@@deriving gt ~options:{ gmap }]
  type nonrec ground = (int Std.Option.t, string Std.Option.t) Std.Pair.t

  type nonrec logic =
    (int logic Std.Option.logic, string logic Std.Option.logic) Std.Pair.logic

  type injected =
    (int ilogic Std.Option.groundi, string ilogic Std.Option.groundi) t ilogic

  (* Old style reifiers *)
  let reify_old : (injected, logic) Reifier.t =
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ ->
      let* r = OCanren.reify in
      let* roptionstring = Std.Option.reify OCanren.reify in
      let* roptionint = Std.Option.reify OCanren.reify in
      let rec foo x =
        match r x with
        | Value x -> Value (GT.gmap t roptionint roptionstring x)
        | Var (v, xs) ->
          Var
            ( v
            , Stdlib.List.map
                (GT.gmap OCanren.logic (GT.gmap t roptionint roptionstring))
                xs )
      in
      Env.Monad.return foo)
  ;;

  let fmapt
    : 'a 'b 'c 'd.
    ('a, 'b) Reifier.t -> ('c, 'd) Reifier.t -> ('a, 'c) t Env.m -> ('b, 'd) t Env.m
    =
    fun fa fb subj -> Env.Monad.(return (GT.gmap t) <*> fa <*> fb <*> subj)
  ;;

  let prj_exn_new : (injected, ground) Reifier.t =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ ->
      OCanren.prj_exn
      <..> chain
             (fmapt
                (Std.Option.prj_exn OCanren.prj_exn)
                (Std.Option.prj_exn OCanren.prj_exn)))
  ;;

  (* test projection *)
  let test_prj r =
    let goal q = q === inji (Std.Option.some (inji 42), Std.Option.some (inji "42")) in
    let xs : ground Stream.t = OCanren.(run q) goal (fun rr -> rr#reify r) in
    match Stream.take xs with
    | [ (Some 42, Some "42") ] -> true
    | _ -> false
  ;;

  (* let%test "prj_exn_old" = test_prj prj_exn_old *)
  let%test "prj_exn_new" = test_prj prj_exn_new

  let reify : (injected, logic) Reifier.t =
    let open Env.Monad in
    let open Env.Monad.Syntax in
    Reifier.fix (fun _ ->
      Reifier.reify
      <..> chain
             (zed
                (rework_logic
                   ~fv:
                     (fmapt
                        (Std.Option.reify OCanren.reify)
                        (Std.Option.reify OCanren.reify)))))
  ;;

  let test_reify rrr =
    let goal q = q === inji (Std.Option.some (inji 42), Std.Option.some (inji "42")) in
    let xs : logic Stream.t = OCanren.(run q) goal (fun rr -> rr#reify rrr) in
    match Stream.take xs with
    | [ Value (Value (Some (Value 42)), Value (Some _)) ] -> true
    | _ -> false
  ;;

  let%test "reify_old" = test_reify reify_old
  let%test "reify_new" = test_reify reify
end
