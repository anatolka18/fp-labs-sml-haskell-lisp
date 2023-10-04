fun f4 (lst : real list) : real =
  if null lst then 0.0
  else
    let
      fun sumList (lst1 : real list, n : int, sum : real, sign : real) : real =
        if null lst1 then sum
        else sumList (tl lst1, 1, sum + sign * hd lst1, sign * ~1.0)
    in
      sumList (lst, 0, 0.0, 1.0)
    end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test0 = f4 []
val test1 = f4 [1.0]
val test2 = f4 [1.0, 2.0, 3.0, 4.0]
val test3 = f4 [5.0, 5.0, 10.0, 10.0, ~1.0]