open GT
open MiniKanren
open Tester

@type t = N | A of t logic with show

let show_t         = show(logic) (show t)
let show_int       = show(logic) (show int)
let show_list      = show(logic) (show list show_int)
let show_list_list = show(logic) (show list show_list)

let int_reifier dc x =
  match reify dc x with
  | [] -> ""
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_int x) (show_int c)) cs

let list_reifier dc x =
  match reify dc x with
  | [] -> ""
  | cs -> show(list) (fun c -> Printf.sprintf "%s =/= %s" (show_list x) (show_list c)) cs

let _ = 
  run show_t         empty_reifier (-1) q (fun q st -> REPR((fresh(x) (x =/= !(A x)))                                                                   st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                          st), ["q", q]);
  run show_list_list empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !1)(![x; y] === q)) st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !0))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !0)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                          st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !1)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y z)(z === !1)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2))                                             st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(y === !1))                                             st), ["q", q]);
  run show_int       empty_reifier (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1))                                   st), ["q", q]);
(* Fix reify:
  run show_list      list_reifier  (-1) q (fun q st -> REPR ((fresh (x y)(![x; !1] =/= ![!2; y])(![x; y] === q))                                        st), ["q", q]);
*)

 
(*

(test "=/=-22"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== `(,x ,y) q)))
  '(((_.0 _.1) (=/= ((_.0 2) (_.1 1))))))

(test "=/=-23"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== `(,x ,y) q)))
  '(((2 _.0) (=/= ((_.0 1))))))

(test "=/=-24"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== y 9)
      (== `(,x ,y) q)))
  '((2 9)))

(test "=/=-24b"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 5)
    (== d 6)))
  '())

(test "=/=-25"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== y 1)
      (== `(,x ,y) q)))
  '())

(test "=/=-26"
  (run* (q)
    (fresh (a x z)
      (=/= a `(,x 1))
      (== a `(,z 1))
      (== x z)))
  '())

(test "=/=-27"
  (run* (q)
    (fresh (a x z)
      (=/= a `(,x 1))
      (== a `(,z 1))
      (== x 5)
      (== `(,x ,z) q)))
  '(((5 _.0) (=/= ((_.0 5))))))

(test "=/=-28"
  (run* (q)
    (=/= 3 4))
  '(_.0))

(test "=/=-29"
  (run* (q)
    (=/= 3 3))
  '())

(test "=/=-30"
  (run* (q) (=/= 5 q)
	    (=/= 6 q)
	    (== q 5))
  '())

(test "=/=-31"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 5)))
  '(((5 . _.0) (=/= ((_.0 6))))))

(test "=/=-32"
  (run* (q)
    (fresh (a)
      (== 3 a)
      (=/= a 4)))
  '(_.0))

(test "=/=-33"
  (run* (q)
    (=/= 4 q)
    (=/= 3 q))
  '((_.0 (=/= ((_.0 3)) ((_.0 4))))))

(test "=/=-34"
  (run* (q) (=/= q 5) (=/= q 5))
  '((_.0 (=/= ((_.0 5))))))

(test "=/=-35"
  (let ((foo (lambda (x)
               (fresh (a)
                 (=/= x a)))))
    (run* (q) (fresh (a) (foo a))))
  '(_.0))

(test "=/=-36"
  (let ((foo (lambda (x)
               (fresh (a)
                 (=/= x a)))))
    (run* (q) (fresh (b) (foo b))))
  '(_.0))

(test "=/=-37"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)))
  '(((_.0 _.1) (=/= ((_.0 _.1))))))

(test "=/=-37b"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))))
  '(((_.0 . _.1) (=/= ((_.0 5) (_.1 6))))))

(test "=/=-37c"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 3)))
  '((3 . _.0)))

(test "=/=-38"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= y x)))
  '(((_.0 _.1) (=/= ((_.0 _.1))))))

(test "=/=-39"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)
      (=/= y x)))
  '(((_.0 _.1) (=/= ((_.0 _.1))))))

(test "=/=-40"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)
      (=/= x y)))
  '(((_.0 _.1) (=/= ((_.0 _.1))))))

(test "=/=-41"
  (run* (q) (=/= q 5) (=/= 5 q))
  '((_.0 (=/= ((_.0 5))))))

(test "=/=-42"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(,x ,y) `(5 6))
      (=/= x 5)))
  '(((_.0 _.1) (=/= ((_.0 5))))))

(test "=/=-43"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x 5)
      (=/= `(,x ,y) `(5 6))))
  '(((_.0 _.1) (=/= ((_.0 5))))))

(test "=/=-44"
  (run* (q)
    (fresh (x y)
      (=/= x 5)
      (=/= `(,x ,y) `(5 6))
      (== `(,x ,y) q)))
  '(((_.0 _.1) (=/= ((_.0 5))))))

(test "=/=-45"
  (run* (q)
    (fresh (x y)
      (=/= 5 x)
      (=/= `(,x ,y) `(5 6))
      (== `(,x ,y) q)))
  '(((_.0 _.1) (=/= ((_.0 5))))))

(test "=/=-46"
  (run* (q)
    (fresh (x y)
      (=/= 5 x)
      (=/= `( ,y ,x) `(6 5))
      (== `(,x ,y) q)))
  '(((_.0 _.1) (=/= ((_.0 5))))))

(test "=/=-47"
  (run* (x)
    (fresh (y z)
      (=/= x `(,y 2))
      (== x `(,z 2))))
  '((_.0 2)))

(test "=/=-48"
  (run* (x)
    (fresh (y z)
      (=/= x `(,y 2))
      (== x `((,z) 2))))
  '(((_.0) 2)))

(test "=/=-49"
  (run* (x)
    (fresh (y z)
      (=/= x `((,y) 2))
      (== x `(,z 2))))
  '((_.0 2)))

(define distincto
  (lambda (l)
    (conde
      ((== l '()))
      ((fresh (a) (== l `(,a))))
      ((fresh (a ad dd)
         (== l `(,a ,ad . ,dd))
         (=/= a ad)
         (distincto `(,a . ,dd))
         (distincto `(,ad . ,dd)))))))

(test "=/=-50"
   (run* (q)
     (distincto `(2 3 ,q)))
   '((_.0 (=/= ((_.0 2)) ((_.0 3))))))

(define rembero
  (lambda (x ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d res)
         (== `(,a . ,d) ls)
         (rembero x d res)
         (conde
           ((== a x) (== out res))
           ((== `(,a . ,res) out))))))))

(test "=/=-51"
  (run* (q) (rembero 'a '(a b a c) q))
  '((b c) (b a c) (a b c) (a b a c)))

(test "=/=-52"
  (run* (q) (rembero 'a '(a b c) '(a b c)))
  '(_.0))

(define rembero
  (lambda (x ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d res)
         (== `(,a . ,d) ls)
         (rembero x d res)
         (conde
           ((== a x) (== out res))
           ((=/= a x) (== `(,a . ,res) out))))))))

(test "=/=-53"
  (run* (q) (rembero 'a '(a b a c) q))
  '((b c)))

(test "=/=-54"
  (run* (q) (rembero 'a '(a b c) '(a b c)))
  '())

(test "=/=-55"
  (run 1 (q) (=/= q #f))
  '((_.0 (=/= ((_.0 #f))))))
*)
