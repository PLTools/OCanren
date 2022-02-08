#### Applicative reifiers

In the end of last year, @YueLiPicasso was checking usefulness of new reifiers more extensively. He discovered some issues and made some changes (more details in the mail and [here](https://github.com/YueLiPicasso/intro_ocaml/tree/master/))

* Some problems with recursive reifiers are discovered.
  * Declare of recursive reifiers with `lazy` keyword is implemented.
* There was an attempt to eliminate `compose` combinator (as Yue Li said: "Getting rid of the unreliable `Reifier.compose` that was used by Moiseenko to define recursive reifiers.")

At the same time I thought about reifiers in background, and was hacking compile-time generation of reifiers. This generation has some quirks (although, not too complicated) related to the naming. Let's look at the following piece of code (will continue to use natural numbers as an example later).

````ocaml
  type 'a t =
    | O
    | S of 'a
  [@@deriving gt ~options:{ gmap }]

  type 'a ground = 'a t
  type 'a logic = 'a t OCanren.logic
  type 'a injected = 'a t ilogic

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
````

There I'm using a fancy syntax with `let*` which was introduced in OCaml 4.08 and basically is applicative+monadic do-notation (a concept highly popularized by Haskell).

This code has minor (or not) issues related to naming. We need to carefully invent names `r`, `rself`, `foo` which may hide some names declared above and carefully use them in reifier. Also, every reifier will have a special function `foo`, which does a shallow reification of provided values (called `x` here).

I discovered all these issues and kind of paused writing code for automatic generation of reifiers. I had a feeling that we could make everything simpler, better suited for automatic generation.

**Minor remark about fixpoint combinator**. (I'm not sure, TODO: recheck this) In original @eucpp's work on monadic combinators, he didn't use any fixpoint combinator and relied on `let rec`.
In my original rework of the approach in `eucpp` branch this fix point combinator (I think) was flawed. It is implemented as  standard CBV fixpoint  combinator there, but we require a fix point combinator for reifiers, which are values of type `('a -> 'b) Reader.t` which is essentially `env -> 'a -> 'b`. To make this fix point combinator work as expected at some moment of hacking I added one more level for eta-expansion to make it work (not to hang).

The idea about fixing the approach was partially inspired by my frustration about names during generic programming, and partially by Yue Li's idea about getting rid of `compose` function. We need to generate new names when we use `bind` operator on our monadic reifiers. Yue Li tried to get rid of `compose` function and make "pure" monadic reifiers. Maybe we should get rid of monadic reifiers and use only `compose` function? Which (occasionally or not) is an `ap` operation if we will see our `_ Reader.t` as an *applicative functor*.

**Remark about applicative functors**. Every monad is an applicative functor -- more general and less powerful concept. Both of them have two operations: ones is the same and other is different.

```ocaml
  type _ t
  val return : 'a -> 'a t
  val (>>=): 'a t  -> ('a -> 'b) t -> 'b t

  (* Applicative functors favor this one instead of bind *)
  val (<*>): ('a -> 'b) t  -> 'a t -> 'b t
```
Practical difference between these two is that monads allow to *dynamically* modify second computation according to result of 1st one, and combine two results later. Applicative functors allow only static actions on the values of two monadic/applicative values. So, applicative functors are less powerful (it could be complicated to express context-sensitive parsers as applicative functors), but compiler can optimize them better. As example, you could compare composition of plain functions, applicative functors and monadic values

````ocaml
let ( <.> ): ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = fun f g x -> f (g x)

let ( <..> ) g f =
  return (<.>) <*> f <*> g

let ( <..> ) g f =
  f >>= fun f ->
  g >>= fun g ->
  return (f <.> g)

````

Now, let's rewrite old-style monadic reifier to new applicative ones. The first observation is that we want avoid explicit names, which means we are not going to use monadic `bind`. It means that we can't pass plain reification functions to our functor operations (`GT.gmap t`), i.e. we need to adapt our functor action to work immediate reifier.

````ocaml
  let fmap : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
   fun fa subj -> (GT.gmap t) fa subj

  let fmapt : 'a 'b. ('a, 'b) Reifier.t -> 'a t Env.m -> 'b t Env.m =
   fun fa subj -> Env.Monad.(return (GT.gmap t) <*> fa <*> subj)
````

This is a simple wrapping of original `GT.gmap t` into applicative one. Now we can write a new projection

````ocaml
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
````

Where we need a few extra function from Env.Monad module

````ocaml
  module Monad : sig
    ...
    (* auxilary function *)
    val chain :  ('a t -> 'b t) -> ('a -> 'b) t

    (* our compose function *)
    val ( <..> ): ('a -> 'b) t -> ('b -> 'c) t -> ('a -> 'c) t
  end = ...
````

Reification will be more complicated. To represent it more compactly let's refactor function that pattern-matches logic values and applies reification to disequalities (we did it explicitly in old style reifiers). The name `rework` is probably not the best but let's stick with it for now.

````ocaml
  module Env.Monad : sig
    val list_mapm : f:('a t -> 'b t) -> 'a list -> 'b list t
  end = ...

  let rework :
      'a 'b.
      fv:('a Env.m -> 'b Env.m)
      -> ('a logic Env.m -> 'b logic Env.m)
      -> 'a logic Env.m
      -> 'b logic Env.m
  =
 fun ~fv fdeq x ->
  let open Env.Monad in
  let open Env.Monad.Syntax in
  let* x = x in
  match x with
  | Var (v, xs) ->
    let+ diseq = list_mapm ~f:fdeq xs in
    Var (v, diseq)
  | Value t ->
    let+ inner = fv (return t) in
    Value inner
;;
````

And now we can define open and closed reifiers for natural numbers:

````ocaml
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
````

Here we use auxiliary `zed` function, it is CBV fixpoint (Zed) combinator.

Supporting code for described approach is here:
  * branch: https://github.com/Kakadu/OCanren/tree/eucpp-work2
  * [Manually written demo](https://github.com/Kakadu/OCanren/blob/eucpp-work2/test_eucpp/lib.ml)
  * [Regression tests](https://github.com/Kakadu/OCanren/blob/eucpp-work2/regression_ppx/test001.ml) with some demos, preprocess using automatic generation of reifiers (via generic programming).
  * [The test above after syntax expansion](https://github.com/Kakadu/OCanren/blob/eucpp-work2/regression_ppx/test001.t) (you can see the code that is actually generated)

TODOs
* We could try to fuse `chain (zed (rework_logic ...))` into a single function call to optimize performance.
* Double check if we could make reifier an *abstract* type (right now all monadic code breaks with type error)

Shortcomings of approach:
* Complicated (?)
* Requires knowledge of abstract stuff (?)
* Abandoning monads may make Yue Li disappointed or sad...
