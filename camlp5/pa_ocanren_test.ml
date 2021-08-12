open Pcaml

let () = Pcaml.inter_phrases := Some (";\n")

let pa1 = PAPR.Implem.pa1
let pr = PAPR.Implem.pr
let fmt_string s = Printf.sprintf "<<%s>>" s

type instance = {
    name : string
  ; code : string
  ; expect : string
}


let  mktest i =
i.name >:: (fun   _ ->
        assert_equal ~msg:"not equal" ~printer:fmt_string
          i.expect
          (pr (pa1 i.code))
                         )


let _ =
  let ast = () in
  ()
