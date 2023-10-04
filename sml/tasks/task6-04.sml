fun f6 [] = 0.0
  | f6 [x] = x
  | f6 (x1 :: x2 :: xs) = x1 - x2 + f6 xs

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test0 = f6 []
val test1 = f6 [1.0]
val test4 = f6 [1.0, ~3.0]
val test2 = f6 [1.0, 2.0, 3.0, 4.0]
val test3 = f6 [5.0, 5.0, 10.0, 10.0, ~1.0]