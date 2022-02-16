(*
 * OCanren PPX
 * Copyright (C) 2016-2021
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(* There we register a few syntax extension to be runned together (to speedup compilation)
 * (actual extensions are provided via linking options)
 *)

let () = Ppxlib.Driver.standalone ()
