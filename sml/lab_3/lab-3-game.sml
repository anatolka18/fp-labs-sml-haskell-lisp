(****************************************************************************** 
  Пример запуска игры UNO
 ******************************************************************************)

(****************************************************************************** 
  Загрузка всех предварительных определений 
 ******************************************************************************)
use "lab-3.sml";
(******************************************************************************)
structure MySTRAT :> STRATEGY = struct
  fun ownStrategy (_, _, cardToPlay, _, _, _, _) =
    let
      val cardH :: _ = cardSort cardToPlay
      val col :: _ = shuffleList colors
    in 
      case cardH 
        of CRD _ => SIMPLE cardH
         | _ => ORDER (cardH, col)
    end
end
(****************************************************************************** 
  Создание набора игроков
 ******************************************************************************)
(* Игрок, которым первоначальное управление - ручное*)
val p1 = Player.make ("Sergey", [], Naive.ownStrategy, true)
(* Все остальные игроки сразу играют в автоматическом режиме *)
val p2  = Player.make ("Pavel", [], Naive.ownStrategy, false)
val p3  = Player.make ("Saveliy", [], Naive.ownStrategy, false)
val p4  = Player.make ("Igor", [], Naive.ownStrategy, false)
(*val p5  = Player.make ("Gennadiy", [], Naive.ownStrategy, false)
val p6  = Player.make ("Irina", [], Naive.ownStrategy, false)
val p7  = Player.make ("Elsa", [], Naive.ownStrategy, false)
val p8  = Player.make ("Sevastian", [], Naive.ownStrategy, false)
val p9  = Player.make ("Arseniy", [], Naive.ownStrategy, false)
val p10 = Player.make ("Olga", [], Naive.ownStrategy, false)*)
(******************************************************************************)

(****************************************************************************** 
  Запуск игры
 ******************************************************************************)
val res = game [p1, p2, p3, p4(*, p5, p6, p7, p8, p9, p10*)]
(******************************************************************************)
