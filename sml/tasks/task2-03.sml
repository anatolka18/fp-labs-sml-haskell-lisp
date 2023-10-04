fun f2 (x : real, y : real, z : real) : real =
  let
    val xy = x * y
    val xyzM = xy * z
    val xyzS = x + y + z
  in
    if xyzS > xyzM then xy + xyzS else xy + xyzM
  end

(* ТЕСТОВЫЕ ЗАПУСКИ *)
val test1 = f2 (1.0, 2.0, 3.0)
val test2 = f2 (2.0, 3.0, 4.0)
val test3 = f2 (3.0, 3.7, 3.0)
val test4 = f2 (4.5, 4.3, 6.7)