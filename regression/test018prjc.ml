open GT
open OCanren
open Tester

let () = Printexc.record_backtrace false

module X = struct
  @type 'a t = A of 'a | Var1 of int with show,gmap
  type nonrec 'a logic = 'a t logic
  type nonrec 'a ilogic = 'a t ilogic

  let fmap f x = gmap(t) f x

  let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('a ilogic, 'b t) Reifier.t
        =
     fun ra ->
      let ( >>= ) = Env.Monad.bind in
      Reifier.prj (fun n -> Var1 n) >>= fun r ->
      ra >>= fun fa ->
      Env.Monad.return (fun x -> GT.gmap t fa (r x))

  let a x     = inj (A x)
end

module Y = struct
  @type 'a t = B of 'a | Var2 of int with show,gmap
  type nonrec 'a logic = 'a t logic
  type nonrec 'a ilogic = 'a t ilogic

  let b x = inj (B x)

  let prj_exn : 'a 'b. ('a, 'b) Reifier.t -> ('a ilogic, 'b t) Reifier.t =
     fun ra ->
      let open Env.Monad.Syntax in
      let* r = Reifier.prj_exn in
      let* fa = ra in
      Env.Monad.return (fun x -> try GT.gmap t fa (r x) with Not_a_value -> Var2 11)

(*
  (* ERROR *)
  let prj : 'a 'b. (int -> 'b t) -> ('a, 'b) Reifier.t -> ('a ilogic, 'b t) Reifier.t =
     fun onvar ra ->
      let ( >>= ) = Env.Monad.bind in
      Reifier.prj onvar >>= fun r ->
      ra >>= fun fa -> Env.Monad.return (fun x -> GT.gmap t fa (r x))
*)

end

let prjc_xy = X.prj_exn (Y.prj_exn OCanren.prj_exn)
let showxy_int = show X.t @@ (show Y.t (show int))
let runResult n = run_r prjc_xy showxy_int n

let () =
  runResult     (-1) q qh (REPR(fun q -> Fresh.one (fun r -> q === q )));
  runResult     (-1) q qh (REPR(fun q -> Fresh.two (fun r s -> (r=/=s) &&& (q===(X.a r))) ));
  ()

let run_list n =
  (* let reifier =
    Std.List.prj (fun _ -> assert false) (OCanren.prj (fun _ -> assert false))
  in *)
  let reifier =
    Std.List.prj_exn OCanren.prj_exn
  in
  run_r reifier (GT.show(Std.List.ground) @@ GT.show GT.int) n

let _ =
  run_list (-1) q qh (REPR(fun q -> Fresh.one (fun r -> q === q )))
