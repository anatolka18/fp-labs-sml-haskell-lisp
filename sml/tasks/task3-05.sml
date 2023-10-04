fun f3 (a1 : real * real * int list, a2 : (int * real) list)  
       : (int * int) list * (int * int) list list * (int * int) list list =
  let
    val x = hd (tl (tl (#3 a1)))
    val f = [(x + 3, 5), (5 + #1 (hd a2), x)]
  in
    (f, [f, f], [f, f, f])
  end

val test0 = f3 ( (3.5, 4.3, [6, 0, 2, 2, 4])
               , [(2, 1.0), (5, 1.0), (5, 1.0)]
               )
val test1 = f3 ( (3.5, 4.3, [6, 1, 1, 2, 4])
               , [(4, 1.0), (5, 1.0), (5, 1.0)] 
               )
val test2 = f3 ( (3.5, 4.3, [6, 0, 2, 2, 4])
               , [(1, 1.0), (5, 1.0), (5, 1.0), (5, 1.0)]
               )
val test3 = f3 ( (3.5, 4.3, [6, 1, 1, 2, 4])
               , [(0, 1.0), (5, 1.0), (5, 1.0), (5, 1.0)] 
               )
