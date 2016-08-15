open GT
open MiniKanren
open Tester

let rec build_num = 
  function
  | 0                   -> !Nil
  | n when n mod 2 == 0 -> !0 % build_num (n / 2)
  | n                   -> !1 % build_num (n / 2)

let nullo q =
  (q === !Nil)

let cdro p d =
  fresh (a)
    ((a % d) === p)

let rec appendo l s out =
  conde [
    (nullo l) &&& (s === out);
    fresh (a d res)
      ((a % d) === l)
      (appendo d s res)
      ((a % res) === out)
  ]

let poso q =
  fresh (h t)
    (q === h % t)

let gt1o q =
  fresh (h t tt)
    (q === h % (t % tt))

let full_addero b x y r c =
  conde [
    (!0 === b) &&& (!0 === x) &&& (!0 === y) &&& (!0 === r) &&& (!0 === c);
    (!1 === b) &&& (!0 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c);
    (!0 === b) &&& (!1 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c);
    (!1 === b) &&& (!1 === x) &&& (!0 === y) &&& (!0 === r) &&& (!1 === c);
    (!0 === b) &&& (!0 === x) &&& (!1 === y) &&& (!1 === r) &&& (!0 === c);
    (!1 === b) &&& (!0 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c);
    (!0 === b) &&& (!1 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c);
    (!1 === b) &&& (!1 === x) &&& (!1 === y) &&& (!1 === r) &&& (!1 === c)
  ]

let rec addero d n m r =
  conde [
    (!0 === d) &&& (!Nil === m) &&& (n === r);
    (!0 === d) &&& (!Nil === n) &&& (m === r) &&& (poso m);
    (!1 === d) &&& (!Nil === m) &&& (defer (addero !0 n (!< !1) r));
    (!1 === d) &&& (!Nil === n) &&& (poso m) &&& (defer (addero !0 m (!< !1) r));
    ?& [
      ((!< !1) === n);
      ((!< !1) === m);
      fresh (a c)
        ((a %< c) === r)
        (full_addero d !1 !1 a c)
    ];
    ((!< !1) === n) &&& (gen_addero d n m r);
    ((!< !1) === m) &&& (gt1o n) &&& (gt1o r) &&& (defer (addero d (!< !1) n r));
    (gt1o n) &&& (gen_addero d n m r)
  ]
and gen_addero d n m r =
  fresh (a b c e x y z)
    ((a % x) === n)
    ((b % y) === m)
    (poso y)
    ((c % z) === r)
    (poso z)
    (full_addero d a b c e)
    (addero e x y z)

let pluso n m k = addero !0 n m k

let minuso n m k = pluso m k n

let rec bound_multo q p n m =
  conde [
    (nullo q) &&& (poso p);
    fresh (x y z)
      (cdro q x)
      (cdro p y)
      (conde [
        (nullo n) &&& (cdro m z) &&& (bound_multo x y z !Nil);
        (cdro n z) &&& (bound_multo x y z m)
      ]) 
  ]

let rec multo n m p =
  conde [
    (!Nil === n) &&& (!Nil === p);
    (poso n) &&& (!Nil === m) &&& (!Nil === p);
    ((!< !1) === n) &&& (poso m) &&& (m === p);
    (gt1o n) &&& ((!< !1) === m) &&& (n === p);
    fresh (x z)
      ((!0 % x) === n)
      (poso x)
      ((!0 % z) === p)
      (poso z)
      (gt1o m)
      (multo x m z);
    fresh (x y)
      ((!1 % x) === n)
      (poso x)
      ((!0 % y) === m)
      (poso y)
      (multo m n p);
    fresh (x y)
      ((!1 % x) === n)
      (poso x)
      ((!1 % y) === m)
      (poso y)
      (odd_multo x n m p)
  ]
and odd_multo x n m p =
  fresh (q)
    (bound_multo q p n m)
    (multo x m q)
    (pluso (!0 % q) m p)

let rec eqlo n m =
  conde [
    (!Nil === n) &&& (!Nil === m);
    ((!< !1) === n) &&& ((!< !1) === m);
    fresh (a x b y)
      ((a % x) === n)
      (poso x)
      ((b % y) === m)
      (poso y)
      (eqlo x y)
  ]

let rec ltlo n m =
  conde [
    (!Nil === n) &&& (poso m);
    ((!< !1) === n) &&& (gt1o m);
    fresh (a x b y)
      ((a % x) === n)
      (poso x)
      ((b % y) === m)
      (poso y)
      (ltlo x y)
  ]

let lelo n m =
  conde [
    (eqlo n m);
    (ltlo n m)
  ]

let rec lto n m =
  conde [
    (ltlo n m);
    ?& [
      (eqlo n m);
      fresh (x)
        (poso x)
        (pluso n x m)
    ]
  ]

let leo n m =
  conde [
    (n === m);
    (lto n m)
  ]

let rec splito n r l h =
  conde [
    (!Nil === n) &&& (!Nil === h) &&& (!Nil === l);
    fresh (b n')
      ((!0 % (b % n')) === n)
      (!Nil === r)
      ((b % n') === h)
      (!Nil === l);
    fresh (n')
      ((!1 % n') === n)
      (!Nil === r)
      (n' === h)
      ((!< !1) === l);
    fresh (b n' a r')
      ((!0 % (b % n')) === n)
      ((a % r') === r)
      (!Nil === l)
      (splito (b % n') r' !Nil h);
    fresh (n' a r')
      ((!1 % n') === n)
      ((a % r') === r)
      ((!< !1) === l)
      (splito n' r' !Nil h);
    fresh (b n' a r' l')
      ((b % n') === n)
      ((a % r') === r)
      ((b % l') === l)
      (poso l')
      (splito n' r' l' h)
  ]

let rec divo n m q r =
  conde [
    (r === n) &&& (!Nil === q) &&& (lto n m);
    ((!< !1) === q) &&& (eqlo n m) &&& (pluso r m n) &&& (lto r m);
    ?& [
      (ltlo m n);
      (lto r m);
      (poso q);
      fresh (nh nl qh ql qlm qlmr rr rh)
        (splito n r nl nh)
        (splito q r ql qh)
        (conde [
          (!Nil === nh) &&& (!Nil === qh) &&& (minuso nl r qlm) &&& (multo ql m qlm);
          ?& [
            (poso nh);
            (multo ql m qlm);
            (pluso qlm r qlmr); 
            (minuso qlmr nl rr);
            (splito rr r !Nil rh);
            (divo nh m qh rh)
          ]
        ])
    ]
  ]

let rec repeated_mul n q nq =
  conde [
    (poso n) &&& (!Nil === q) &&& ((!< !1) === nq);
    ((!< !1) === q) &&& (n === nq);
    ?& [
      (gt1o q);
      fresh (q1 nq1)
        (pluso q1 (!< !1) q)
        (repeated_mul n q1 nq1)
        (multo nq1 n nq)
    ]
  ]

let rec exp2 n b q =
  conde [
    ((!< !1) === n) &&& (!Nil === q);
    ?& [
      (gt1o n);
      ((!< !1) === q);
      fresh (s)
        (splito n b s (!< !1))
    ];
    fresh (q1 b2)
      ((!0 % q1) === q)
      (poso q1)
      (ltlo b n)
      (appendo b (!1 % b) b2)
      (exp2 n b2 q1);
    fresh (q1 nh b2 s)
      ((!1 % q1) === q)
      (poso q1)
      (poso nh)
      (splito n b s nh)
      (appendo b (!1 % b) b2)
      (exp2 nh b2 q1)
  ]

let rec logo n b q r =
  conde [
    ((!< !1) === n) &&& (poso b) &&& (!Nil === q) &&& (!Nil === r);
    (!Nil === q) &&& (lto n b) &&& (pluso r (!< !1) n);
    ((!< !1) === q) &&& (gt1o b) &&& (eqlo n b) &&& (pluso r b n);
    ((!< !1) === b) &&& (poso q) &&& (pluso r (!< !1) n);
    (!Nil === b) &&& (poso q) &&& (r === n);
    ?& [
      ((!0 %< !1) === b);
      fresh (a ad dd)
        (poso dd)
        ((a % (ad % dd)) === n)
        (exp2 n !Nil q)
        (fresh (s)
          (splito n dd r s)
        )
    ];
    ?& [
      fresh (a ad add ddd)
        (conde [
          ((!1 %< !1) === b);
          ((a % (ad % (add % ddd))) === b)
        ]);
      (ltlo b n);
      fresh (bw1 bw nw nw1 ql1 ql s)
        (exp2 b !Nil bw1)
        (pluso bw1 (!< !1) bw)
        (ltlo q n)
        (fresh (q1 bwq1)
          (pluso q (!< !1) q1)
          (multo bw q1 bwq1)
          (lto nw1 bwq1)
          (exp2 n !Nil nw1)
          (pluso nw1 (!< !1) nw)
          (divo nw bw ql1 s)
          (pluso ql (!< !1) ql1)
          (lelo ql q)
          (fresh (bql qh s qdh qd)
            (repeated_mul b ql bql)
            (divo nw bw1 qh s)
            (pluso ql qdh qh)
            (pluso ql qd q)
            (leo qd qdh)
            (fresh (bqd bq1 bq)
              (repeated_mul b qd bqd)
              (multo bql bqd bq)
              (multo b bq bq1)
              (pluso bq r n)
              (lto n bq1)
            )
          )
        )
    ]
  ]

let expo b q n =
  (logo n b q !Nil)

let test17 n m =
  (lelo n m) &&& (multo n (build_num 2) m)

let test27 b q r =
  (logo (build_num 68) b q r) &&& (gt1o q)

let show_int      = show(logic) (show(int))
let show_int_list = show(List.logic) show_int

let qrsth = fun qs rs ss ts -> ["q", qs; "r", rs; "s", ss; "t", ts]

let _ =
  run show_int_list  22  qrs (REPR (fun q r s   -> pluso q r s                                      ))  qrsh;
  run show_int_list (-1)  qr (REPR (fun q r     -> multo q r (build_num 1)                          ))   qrh;
  run show_int_list (-1)   q (REPR (fun q       -> multo (build_num 7) (build_num 63) q             ))    qh;
  run show_int_list (-1)  qr (REPR (fun q r     -> divo (build_num 3) (build_num 2) q r             ))   qrh;
  run show_int_list  34  qrs (REPR (fun q r s   -> multo q r s                                      ))  qrsh;
  run show_int_list  10   qr (REPR (fun q r     -> test17 q r                                       ))   qrh;
  run show_int_list  15   qr (REPR (fun q r     -> lelo q r                                         ))   qrh;
  run show_int_list (-1)   q (REPR (fun q       -> lto q (build_num 5)                              ))    qh;
  run show_int_list (-1)   q (REPR (fun q       -> lto (build_num 5) q                              ))    qh;
  run show_int_list   6 qrst (REPR (fun q r s t -> divo q r s t                                     )) qrsth;
  run show_int_list (-1)   q (REPR (fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q))    qh;
  (* run show_int_list   5  qrs (REPR (fun q r s   -> test27 q r s                                     ))  qrsh; *)
  (* run show_int_list (-1)   q (REPR (fun q       -> expo (build_num 3) (build_num 5) q               ))    qh; *)
