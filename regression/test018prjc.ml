open GT
open MiniKanren
open Tester

module X = struct
  module X = struct
    @type 'a t = A of 'a | Var1 of int with show,gmap
    let fmap f x = gmap(t) f x
  end
  include X
  include Fmap(X)
  let a x     = inj @@ distrib (A x)
end
module Y = struct
  module Y = struct
    @type 'a t = B of 'a | Var2 of int with show,gmap
    let fmap f x = gmap(t) f x
  end

  include Y
  include Fmap(Y)

  let b x     = inj @@ distrib (B x)
end

let prjc_xy h t =
  X.prjc (Y.prjc (MiniKanren.prjc (fun _ -> assert false)) (fun n _ -> Y.Var2 n))
    (fun n _ -> X.Var1 n) h t

let showxy_int = show X.t @@ (show Y.t (show int))

let runResult n = run_prjc prjc_xy showxy_int n

let () =
  runResult     (-1) q qh (REPR(fun q -> Fresh.one (fun r -> q === q )));
  runResult     (-1) q qh (REPR(fun q -> Fresh.two (fun r s -> (r=/=s) &&& (q===(X.a r))) ));
  ()
