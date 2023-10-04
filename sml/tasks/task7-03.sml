fun f7 n =
  let
    val nP3 = n + 3
    fun p (i, proiz) =
      if i > n then proiz
      else 
        let 
          fun s (j, sum) =
            if j > nP3 then sum
            else 
              let val jR = real j 
              in s (j + 1, sum + real i / jR + Math.sqrt jR) 
              end
        in 
          p (i + 1, proiz * s (4, 0.0))
        end
  in
    p (2, 1.0)
  end

val test1 = f7 3
val test2 = f7 4
val test3 = f7 5