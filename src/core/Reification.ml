let rec reify env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

let rec prjc of_int env x =
  match Env.var env x with
  | Some v -> let i, cs = Var.reify (prjc of_int env) v in of_int i cs
  | None   -> Obj.magic x

module type T1 =
  sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
  end

module type T2 =
  sig
   type ('a, 'b) t
   val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end

module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t -> ('q, 'r, 's) t
  end

module type T4 =
  sig
    type ('a, 'b, 'c, 'd) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('a, 'b, 'c, 'd) t -> ('q, 'r, 's, 't) t
  end

module type T5 =
  sig
    type ('a, 'b, 'c, 'd, 'e) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('a, 'b, 'c, 'd, 'e) t -> ('q, 'r, 's, 't, 'u) t
  end

module type T6 =
  sig
    type ('a, 'b, 'c, 'd, 'e, 'f) t
    val fmap : ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('d -> 't) -> ('e -> 'u) -> ('f -> 'v) -> ('a, 'b, 'c, 'd, 'e, 'f) t -> ('q, 'r, 's, 't, 'u, 'v) t
  end

module Fmap (T : T1) =
  struct
    external distrib : ('a,'b) injected T.t -> ('a T.t, 'b T.t) injected = "%identity"

    let rec reify r env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r env) v in Var (i, cs)
      | None   -> Value (T.fmap (r env) x)

    let rec prjc r of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r of_int env) v in of_int i cs
      | None   -> T.fmap (r env) x
  end

module Fmap2 (T : T2) =
  struct
    external distrib : (('a,'b) injected, ('c, 'd) injected) T.t -> (('a, 'b) T.t, ('c, 'd) T.t) injected = "%identity"

    let rec reify r1 r2 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) x)

    let rec prjc r1 r2 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) x
  end

module Fmap3 (T : T3) =
  struct
    external distrib : (('a, 'b) injected, ('c, 'd) injected, ('e, 'f) injected) T.t -> (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t) injected = "%identity"

    let rec reify r1 r2 r3 env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (reify r1 r2 r3 env) v in Var (i, cs)
      | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) x)

    let rec prjc r1 r2 r3 of_int env x =
      match Env.var env x with
      | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 of_int env) v in of_int i cs
      | None   -> T.fmap (r1 env) (r2 env) (r3 env) x
end

module Fmap4 (T : T4) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected) T.t ->
                     (('a, 'c, 'e, 'g) T.t, ('b, 'd, 'f, 'h) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x)

  let rec prjc r1 r2 r3 r4 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) x
end

module Fmap5 (T : T5) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i) T.t, ('b, 'd, 'f, 'h, 'j) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x)

  let rec prjc r1 r2 r3 r4 r5 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) x
end

module Fmap6 (T : T6) = struct
  external distrib : (('a,'b) injected, ('c, 'd) injected, ('e, 'f) injected, ('g, 'h) injected, ('i, 'j) injected, ('k, 'l) injected) T.t ->
                     (('a, 'c, 'e, 'g, 'i, 'k) T.t, ('b, 'd, 'f, 'h, 'j, 'l) T.t) injected = "%identity"

  let rec reify r1 r2 r3 r4 r5 r6 env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (reify r1 r2 r3 r4 r5 r6 env) v in Var (i, cs)
    | None   -> Value (T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x)

  let rec prjc r1 r2 r3 r4 r5 r6 of_int env x =
    match Env.var env x with
    | Some v -> let i, cs = Var.reify (prjc r1 r2 r3 r4 r5 r6 of_int env) v in of_int i cs
    | None   -> T.fmap (r1 env) (r2 env) (r3 env) (r4 env) (r5 env) (r6 env) x
end