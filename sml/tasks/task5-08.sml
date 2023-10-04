fun f5 (x : real, n : int) : real =
  let 
    val doubleX = x * x
    (* Цикл *)
    fun f5Iter ( i : int        (* счетчик *)
               , accum : real    (* сумма *)
               , twoDeg : real   (* степень 2 *)
               , twonFactC : real (* посчитанный факториал 2n *)
               , twonFactUnC : real (* непосчитанный факториал 2n *)
               , sign : real     (* знак *)
               , xDeg : real )   (* степень икса *)
               : real =
      if i > n then accum
      else 
        f5Iter ( i + 1
               , accum 
                 - sign * twoDeg / twonFactC * xDeg
               , twoDeg * 4.0
               , twonFactC * (twonFactUnC - 1.0) * twonFactUnC
               , twonFactUnC + 2.0
               , sign * ~1.0
               , xDeg * doubleX )
  in
    f5Iter (1, 0.0, 2.0, 2.0, 4.0, ~1.0, doubleX)
  end

fun f5Test (x : real) : real = 
  let 
    val sinX = Math.sin x
  in sinX * sinX
  end

val test11 = f5 (0.7, 100)
val test12 = f5Test 0.7

val test21 = f5 (0.5, 100)
val test22 = f5Test 0.5

val test31 = f5 (~0.3, 100)
val test32 = f5Test ~0.3