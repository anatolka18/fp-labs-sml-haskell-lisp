(****************************************************************************** 
  Шаблон с тестами для выполнения заданий лабораторной работы №3
 ******************************************************************************)

(****************************************************************************** 
  Загрузка файла с лабораторной работой 
 ******************************************************************************)
use "lab-3.sml";

(****************************************************************************** 
  ТЕСТЫ К РЕШЕНИЯМ
  Здесь приведены по большей части тривиальные тесты. Их успешное выполнение 
  не гарантирует того, что Ваше решение функционирует правильно.
 ******************************************************************************)
(****************************************************************************** 
  Задание 1 precedes
 ******************************************************************************)
val test1_precedes1 = precedes (op =) [1, 2, 3, 4, 5] 5 3 = false
val test1_precedes2 = precedes (op =) [1, 2, 3, 4, 5] 3 5 = true
val test1_precedes3 = precedes (op =) [] 3 5 = false
                        handle IllegalGame => true
(******************************************************************************)

(****************************************************************************** 
  Задание 2 colorIsLT и rankIsLT
 ******************************************************************************)
val test2_colorIsLT1 = colorIsLT RED BLUE = false
val test2_colorIsLT2 = colorIsLT BLUE RED = true
val test2_colorIsLT3 = colorIsLT YELLOW GREEN = true
val test2_rankIsLT1 = rankIsLT (NUM 1) (NUM 5) = true
val test2_rankIsLT2 = rankIsLT (NUM 5) (NUM 0) = false
val test2_rankIsLT3 = rankIsLT (NUM 9) (NUM 1) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 3 isLT
 ******************************************************************************)
val test3_isLT1 = isLT (CRD (NUM 1, RED), CRD (NUM 2, BLUE)) = false
val test3_isLT2 = isLT (WILD, WILD_DRAW_FOUR) = true
val test3_isLT5 = isLT (WILD_DRAW_FOUR, WILD_DRAW_FOUR) = true
val test3_isLT3 = isLT (WILD_DRAW_FOUR, WILD) = false
val test3_isLT4 = isLT (WILD, CRD (NUM 3, BLUE)) = false
val test3_isLT6 = isLT (WILD_DRAW_FOUR, CRD (NUM 3, BLUE)) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 4 cardSort
 ******************************************************************************)
val test4_cardSort1 = cardSort [CRD (NUM 1, RED), CRD (NUM 2, RED)]
                      = [CRD (NUM 2, RED), CRD (NUM 1, RED)]
val test4_cardSort2 = cardSort [ CRD (NUM 2, YELLOW), CRD (NUM 5, GREEN)
                               , CRD (NUM 8, YELLOW), CRD (REVERSE, YELLOW)
                               , WILD, CRD (SKIP, RED), CRD (DRAW_TWO, GREEN) ] 
      = [ WILD ,CRD (SKIP, RED), CRD (DRAW_TWO, GREEN), CRD (NUM 5, GREEN)
, CRD (REVERSE, YELLOW), CRD (NUM 8, YELLOW), CRD (NUM 2, YELLOW)]

(******************************************************************************)

(****************************************************************************** 
  Задание 5 cardValue
 ******************************************************************************)
val test5_cardValue1 = cardValue (CRD (NUM 9, RED)) = 9
val test5_cardValue2 = cardValue WILD = 50
val test5_cardValue3 = cardValue (CRD (DRAW_TWO,GREEN)) = 20
(******************************************************************************)

(****************************************************************************** 
  Задание 6 sumCards
 ******************************************************************************)
val test6_sumCards1 = sumCards [WILD, CRD (NUM 0, RED)] = 50
val test6_sumCards2 = sumCards [ CRD (NUM 2, YELLOW), CRD (NUM 5, GREEN), CRD (NUM 8, YELLOW), CRD (REVERSE, YELLOW)
, WILD, CRD (SKIP, RED), CRD (DRAW_TWO, GREEN) ] = 125
val test6_sumCards3 = sumCards [] = 0
(******************************************************************************)

(****************************************************************************** 
  Задание 7 oppositeDir
 ******************************************************************************)
val test7_oppositeDir1 = oppositeDir CLOCKWISE = COUNTERCLOCKWISE
val test7_oppositeDir2 = oppositeDir COUNTERCLOCKWISE = CLOCKWISE
(******************************************************************************)

(****************************************************************************** 
  Задание 8 removeCard
 ******************************************************************************)
val test8_removeCard1 = removeCard ([], CRD (NUM 2, BLUE), List.Empty) = [WILD]
                            handle List.Empty => true
val test8_removeCard2 = 
    removeCard ([CRD (NUM 2, BLUE)], CRD (NUM 2, BLUE), List.Empty) = []
                            handle _ => false
val test8_removeCard3 = 
  removeCard ( [ CRD (NUM 2, YELLOW), CRD (NUM 5, GREEN), CRD (NUM 8, YELLOW), CRD (REVERSE, YELLOW)
, WILD, CRD (SKIP, RED), CRD (DRAW_TWO, GREEN) ]
, CRD (NUM 5, GREEN)
, List.Empty) = [ CRD (NUM 2, YELLOW), CRD (NUM 8, YELLOW), CRD (REVERSE, YELLOW)
, WILD, CRD (SKIP,RED), CRD (DRAW_TWO,GREEN)]
                            handle _ => false
(******************************************************************************)

(****************************************************************************** 
  Задание 9 cardCount
 ******************************************************************************)
val test9_cardCount1 = cardCount (CRD (NUM 0, RED)) = 1
val test9_cardCount2 = cardCount (CRD (NUM 2, RED)) = 2
val test9_cardCount3 = cardCount WILD = 4
val test9_cardCount4 = cardCount WILD_DRAW_FOUR = 4
(******************************************************************************)

(****************************************************************************** 
  Задание 10 deck
 ******************************************************************************)
val test10_deck = deck =
  [WILD,WILD,WILD,WILD,WILD_DRAW_FOUR,WILD_DRAW_FOUR,WILD_DRAW_FOUR,
   WILD_DRAW_FOUR,CRD (NUM 0,YELLOW),CRD (NUM 1,YELLOW),CRD (NUM 1,YELLOW),
   CRD (NUM 2,YELLOW),CRD (NUM 2,YELLOW),CRD (NUM 3,YELLOW),
   CRD (NUM 3,YELLOW),CRD (NUM 4,YELLOW),CRD (NUM 4,YELLOW),
   CRD (NUM 5,YELLOW),CRD (NUM 5,YELLOW),CRD (NUM 6,YELLOW),
   CRD (NUM 6,YELLOW),CRD (NUM 7,YELLOW),CRD (NUM 7,YELLOW),
   CRD (NUM 8,YELLOW),CRD (NUM 8,YELLOW),CRD (NUM 9,YELLOW),
   CRD (NUM 9,YELLOW),CRD (REVERSE,YELLOW),CRD (REVERSE,YELLOW),
   CRD (SKIP,YELLOW),CRD (SKIP,YELLOW),CRD (DRAW_TWO,YELLOW),
   CRD (DRAW_TWO,YELLOW),CRD (NUM 0,BLUE),CRD (NUM 1,BLUE),CRD (NUM 1,BLUE),
   CRD (NUM 2,BLUE),CRD (NUM 2,BLUE),CRD (NUM 3,BLUE),CRD (NUM 3,BLUE),
   CRD (NUM 4,BLUE),CRD (NUM 4,BLUE),CRD (NUM 5,BLUE),CRD (NUM 5,BLUE),
   CRD (NUM 6,BLUE),CRD (NUM 6,BLUE),CRD (NUM 7,BLUE),CRD (NUM 7,BLUE),
   CRD (NUM 8,BLUE),CRD (NUM 8,BLUE),CRD (NUM 9,BLUE),CRD (NUM 9,BLUE),
   CRD (REVERSE,BLUE),CRD (REVERSE,BLUE),CRD (SKIP,BLUE),CRD (SKIP,BLUE),
   CRD (DRAW_TWO,BLUE),CRD (DRAW_TWO,BLUE),CRD (NUM 0,GREEN),
   CRD (NUM 1,GREEN),CRD (NUM 1,GREEN),CRD (NUM 2,GREEN),CRD (NUM 2,GREEN),
   CRD (NUM 3,GREEN),CRD (NUM 3,GREEN),CRD (NUM 4,GREEN),CRD (NUM 4,GREEN),
   CRD (NUM 5,GREEN),CRD (NUM 5,GREEN),CRD (NUM 6,GREEN),CRD (NUM 6,GREEN),
   CRD (NUM 7,GREEN),CRD (NUM 7,GREEN),CRD (NUM 8,GREEN),CRD (NUM 8,GREEN),
   CRD (NUM 9,GREEN),CRD (NUM 9,GREEN),CRD (REVERSE,GREEN),
   CRD (REVERSE,GREEN),CRD (SKIP,GREEN),CRD (SKIP,GREEN),CRD (DRAW_TWO,GREEN),
   CRD (DRAW_TWO,GREEN),CRD (NUM 0,RED),CRD (NUM 1,RED),CRD (NUM 1,RED),
   CRD (NUM 2,RED),CRD (NUM 2,RED),CRD (NUM 3,RED),CRD (NUM 3,RED),
   CRD (NUM 4,RED),CRD (NUM 4,RED),CRD (NUM 5,RED),CRD (NUM 5,RED),
   CRD (NUM 6,RED),CRD (NUM 6,RED),CRD (NUM 7,RED),CRD (NUM 7,RED),
   CRD (NUM 8,RED),CRD (NUM 8,RED),CRD (NUM 9,RED),CRD (NUM 9,RED),
   CRD (REVERSE,RED),CRD (REVERSE,RED),CRD (SKIP,RED),CRD (SKIP,RED),
   CRD (DRAW_TWO,RED),CRD (DRAW_TWO,RED)]
(******************************************************************************)

(****************************************************************************** 
  Задание 11 deal
 ******************************************************************************)
(* Опишите свой тест *)
(* Чтобы проверить, что функция работает правильно, нужно убедиться в том , что 
 *    - объединение всех карт на столе составляет исходную колоду карт
 *    - у каждого игрока после раздачи 7 карт
 *    - порядок игроков после раздачи соответствует их порядку в исходном списке
 *    - в колоду "Сброс" выделена одна карта *)

(******************************************************************************)

(****************************************************************************** 
  Задание 12 nextPlayer
 ******************************************************************************)
val p1 = Player.make ("Sergey", [], False.ownStrategy, false)
val p2  = Player.make ("Pavel", [], False.ownStrategy, false)
val p3  = Player.make ("Saveliy", [], False.ownStrategy, false)
val p4  = Player.make ("Igor", [], False.ownStrategy, false)
val p5  = Player.make ("Gennadiy", [], False.ownStrategy, false)
val p6  = Player.make ("Irina", [], False.ownStrategy, false)
val p7  = Player.make ("Elsa", [], False.ownStrategy, false)
val p8  = Player.make ("Sevastian", [], False.ownStrategy, false)
val p9  = Player.make ("Arseniy", [], False.ownStrategy, false)
val p10 = Player.make ("Olga", [], False.ownStrategy, true)

val test12_nextPlayer1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [], [], PROCEED, CLOCKWISE, [] )
    val newDsk = nextPlayer dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    Player.getName plr = "Pavel"
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 13 deskNormalize
 ******************************************************************************)
val test13_deskNormalize1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [CRD (REVERSE, RED)]
                        , []
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = deskNormalize dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    Desk.getDirection newDsk = COUNTERCLOCKWISE
    andalso Player.getName plr = "Sergey"
  end

val test13_deskNormalize2 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [WILD]
                        , [CRD (NUM 1, RED)]
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = deskNormalize dsk
    val plr :: _ = Desk.getPlayers newDsk
    val newPile = Desk.getPile newDsk
    val newDeck = Desk.getDeck newDsk
  in
    Desk.getDirection newDsk = CLOCKWISE
    andalso Player.getName plr = "Pavel"
    andalso newPile = [CRD (NUM 1, RED)]
    andalso newDeck = [WILD]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 14 draw
 ******************************************************************************)
val test14_draw1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [CRD (REVERSE, RED)]
                        , [CRD (NUM 1, RED)]
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = draw dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    Player.getCards plr = [CRD (NUM 1, RED)]
    andalso null (Desk.getDeck newDsk)
  end

val test14_draw2 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [CRD (REVERSE, RED), CRD (NUM 1, RED), WILD]
                        , []
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = draw dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    Player.getCards plr = [WILD]
    orelse 
    Player.getCards plr = [CRD (NUM 1, RED)]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 15 drawTwo
 ******************************************************************************)
val test15_drawTwo1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [CRD (REVERSE, RED)]
                        , [CRD (NUM 1, RED), WILD]
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = drawTwo dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    cardSort (Player.getCards plr) = [WILD, CRD (NUM 1, RED)]
    andalso null (Desk.getDeck newDsk)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 16 drawFour
 ******************************************************************************)
val test16_drawFour1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [ CRD (REVERSE, RED), CRD (NUM 5, BLUE)
                          , WILD_DRAW_FOUR ]
                        , [CRD (NUM 1, RED), WILD]
                        , PROCEED, CLOCKWISE, [] )
    val newDsk = drawFour dsk
    val plr :: _ = Desk.getPlayers newDsk
  in
    cardSort (Player.getCards plr) = [ WILD_DRAW_FOUR, WILD, CRD (NUM 1, RED)
                                     , CRD (NUM 5, BLUE) ]
    andalso null (Desk.getDeck newDsk)
    andalso Desk.getPile newDsk = [CRD (REVERSE, RED)]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 17 playableWhenWild
 ******************************************************************************)
val test17_playableWhenWild1 = 
  playableWhenWild RED (CRD (NUM 5, RED)) = true
val test17_playableWhenWild2 = 
  playableWhenWild RED (CRD (REVERSE, GREEN)) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 18 playableWhenExec
 ******************************************************************************)
val test18_playableWhenExec1 = 
  playableWhenExec SKIP (CRD (NUM 5, RED)) = false
val test18_playableWhenExec2 = 
  playableWhenExec DRAW_TWO (CRD (DRAW_TWO, GREEN)) = true
(******************************************************************************)

(****************************************************************************** 
  Задание 19 playableWhenProc
 ******************************************************************************)
val test19_playableWhenProc1 = 
  playableWhenProc (NUM 3, RED) WILD = true
val test19_playableWhenExec2 = 
  playableWhenProc (NUM 3, RED) (CRD (DRAW_TWO, GREEN)) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 20 cardsToPlay
 ******************************************************************************)
val test20_cardsToPlay1 = 
  let 
    val p1 = Player.setCards ( p1, [ WILD, CRD (NUM 5, RED)
                                   , CRD (DRAW_TWO, GREEN)
                                   , CRD (NUM 7, RED)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [ CRD (REVERSE, RED) ]
                        , []
                        , PROCEED, CLOCKWISE, [] )
  in
    cardSort (cardsToPlay dsk) = [WILD, CRD (NUM 7, RED), CRD (NUM 5, RED)]
  end

val test20_cardsToPlay2 = 
  let 
    val p1 = Player.setCards ( p1, [ WILD, CRD (NUM 5, RED)
                                   , CRD (DRAW_TWO, GREEN)
                                   , CRD (NUM 7, RED)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [ CRD (DRAW_TWO, BLUE) ]
                        , []
                        , PROCEED, CLOCKWISE, [] )
  in
    cardSort (cardsToPlay dsk) = [WILD,CRD (DRAW_TWO,GREEN),CRD (SKIP,BLUE)]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 21 requiredColor
 ******************************************************************************)
val test21_requiredColor1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [ CRD (REVERSE, RED)]
                        , []
                        , PROCEED, CLOCKWISE, [] )
  in
    requiredColor dsk = RED
  end

val test21_requiredColor2 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]
                        , [ CRD (REVERSE, RED), WILD]
                        , []
                        , GIVE BLUE, CLOCKWISE, [] )
  in
    requiredColor dsk = BLUE
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 22 hasColor
 ******************************************************************************)
val test22_hasColor1 = 
      hasColor (GREEN, [WILD, CRD (NUM 0, GREEN)]) = true
val test22_hasColor2 = 
      hasColor (RED, [WILD, CRD (NUM 0, GREEN)]) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 23 countCards
 ******************************************************************************)
val test23_countCards1 = 
  let 
    val p1 = Player.setCards ( p1, [ WILD, CRD (NUM 5, RED)
                                   , CRD (DRAW_TWO, GREEN)
                                   , CRD (NUM 7, RED)
                                   , CRD (SKIP, BLUE) ] )
    val p2 = Player.setCards (p2, [WILD, CRD (NUM 5, RED)])
    val dsk = Desk.make ( [p1, p2, p3]
                        , [ CRD (REVERSE, RED) ]
                        , []
                        , PROCEED, CLOCKWISE, [] )
  in
    countCards dsk = [5, 2, 0]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 24 Naive.ownStrategy
 ******************************************************************************)
val test24_Naive_ownStrategy1 = 
  Naive.ownStrategy ( EXECUTE
                    , [ WILD
                      , CRD (NUM 5, RED)
                      , CRD (DRAW_TWO, GREEN)
                      , CRD (NUM 7, RED)
                      , CRD (SKIP, BLUE) ]
                    , [ CRD (SKIP, BLUE) ]
                    , CRD (SKIP, RED)
                    , CLOCKWISE
                    , [], [] )
  = SIMPLE (CRD (SKIP, BLUE))

val test24_Naive_ownStrategy2 = 
  let 
    val mv = Naive.ownStrategy ( PROCEED
                               , [ WILD
                                 , CRD (NUM 5, RED)
                                 , CRD (DRAW_TWO, GREEN)
                                 , CRD (NUM 7, RED)
                                 , CRD (SKIP, BLUE) ]
                               , [ WILD
                                 , CRD (NUM 5, RED)
                                 , CRD (NUM 7, RED)
                                 , CRD (SKIP, BLUE) ]
                               , CRD (SKIP, RED)
                               , CLOCKWISE
                               , [], [] )
    val ORDER (crd, _) = mv handle _ => ORDER (CRD (NUM 0, BLUE), BLUE)
  in
    crd = WILD
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 25 moveAction
 ******************************************************************************)
val test25_moveAction1 = 
  let 
    val p1 = Player.setCards ( p1, [ WILD, CRD (NUM 5, RED)
                                   , CRD (DRAW_TWO, GREEN)
                                   , CRD (NUM 7, RED)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (REVERSE, RED)]
                        , []
                        , EXECUTE, CLOCKWISE, [] )
    val newDsk = moveAction (SIMPLE (CRD (REVERSE, RED)), dsk, true)
    val plr :: _ = Desk.getPlayers newDsk
  in
    Player.getName plr = "Saveliy"
    andalso Desk.getDirection newDsk = COUNTERCLOCKWISE
    andalso Desk.getState newDsk = PROCEED
  end

val test25_moveAction2 = 
  let 
    val p1 = Player.setCards ( p1, [ WILD, CRD (NUM 5, RED)
                                   , CRD (DRAW_TWO, GREEN)
                                   , CRD (NUM 7, RED)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (DRAW_TWO, RED)]
                        , []
                        , EXECUTE, CLOCKWISE, [] )
    val newDsk = moveAction (SIMPLE (CRD (DRAW_TWO, RED)), dsk, true)
    val plr :: _ = Desk.getPlayers newDsk
  in
    Player.getName plr = "Pavel"
    andalso Desk.getDirection newDsk = CLOCKWISE
    andalso Desk.getState newDsk = EXECUTE
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 26 askPlayerForCard
 ******************************************************************************)
val test26_askPlayerForCard1 = 
  let 
    val args = ( EXECUTE
               , [ CRD (NUM 5, RED) ]
               , [ CRD (NUM 5, RED) ]
               , CRD (SKIP, RED)
               , CLOCKWISE
               , [], [] )
    val (plr, mv) = askPlayerForCard (p1, args)
  in
    mv = SIMPLE (CRD (NUM 5, GREEN))
  end

(* Тест предполагает, что при выборе хода нужно перейти в автоматический режим.
 * При запросе нужно ввести число не равное 0 *)
(*val test26_askPlayerForCard2 = 
  let 
    val args = ( EXECUTE
               , [ CRD (NUM 5, RED) ]
               , [ CRD (NUM 5, RED) ]
               , CRD (SKIP, RED)
               , CLOCKWISE
               , [], [] )
    val (plr, mv) = askPlayerForCard (p10, args)
  in
    mv = SIMPLE (CRD (NUM 5, GREEN))
    andalso not (Player.isOnManual plr)
  end

(* Тест предполагает, что при выборе хода нужно остаться в ручном режиме.
 * При запросе нужно ввести 0 *)
val test26_askPlayerForCard3 = 
  let 
    val args = ( EXECUTE
               , [ CRD (NUM 5, RED) ]
               , [ CRD (NUM 5, RED) ]
               , CRD (SKIP, RED)
               , CLOCKWISE
               , [], [] )
    val (plr, mv) = askPlayerForCard (p10, args)
    val _ = print (Bool.toString (Player.isOnManual plr))
  in
    mv = SIMPLE (CRD (NUM 5, RED))
    andalso Player.isOnManual plr
  end *)
(******************************************************************************)

(****************************************************************************** 
  Задание 27 play
 ******************************************************************************)
val test27_play1 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, GREEN)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (REVERSE, GREEN)]
                        , []
                        , PROCEED, CLOCKWISE, [] )
    val playCards = cardsToPlay dsk
    val newDsk = play (dsk, playCards)
    val [plr1, plr2, plr3] = Desk.getPlayers newDsk
  in
    Player.getName plr1 = "Pavel"
    andalso Player.getName plr3 = "Sergey"
    andalso Player.getCards plr3 = [CRD (SKIP, BLUE)]
    andalso Desk.getDirection newDsk = CLOCKWISE
    andalso Desk.getState newDsk = PROCEED
    andalso Desk.getPile newDsk = [CRD (NUM 5, GREEN), CRD (REVERSE, GREEN)]
    andalso Desk.getMemo newDsk = [SIMPLE (CRD (NUM 5, GREEN))]
  end

val test27_play2 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, RED)
                                   , CRD (SKIP, BLUE) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (REVERSE, RED)]
                        , []
                        , PROCEED, CLOCKWISE, [] )
    val playCards = cardsToPlay dsk
  in
    Desk.getDirection (play (dsk, playCards)) = COUNTERCLOCKWISE
      handle IllegalMove _ => true
           | _ => false  
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 28 execution
 ******************************************************************************)
val test28_execution1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (DRAW_TWO, GREEN)]
                        , [ CRD (NUM 5, GREEN)
                          , CRD (SKIP, BLUE) ]
                        , EXECUTE, CLOCKWISE, [] )
    val newDsk = execution dsk
    val [plr1, plr2, plr3] = Desk.getPlayers newDsk
  in
    Player.getName plr1 = "Pavel"
    andalso Player.getName plr3 = "Sergey"
    andalso cardSort (Player.getCards plr3) = [ CRD (NUM 5, GREEN)
                                              , CRD (SKIP, BLUE) ]
    andalso Desk.getState newDsk = PROCEED
    andalso null (Desk.getDeck newDsk)
    andalso Desk.getMemo newDsk = [PASS]
  end

val test28_execution2 = 
  let 
    val dsk = Desk.make ( [ Player.make ( "Efim"
                                      , []
                                      , False.ownStrategy
                                      , false
                                      )
                        , Player.make ( "Gerasim"
                                      , []
                                      , False.ownStrategy
                                      , false
                                      )
                        , Player.make ( "Samuil"
                                      , []
                                      , False.ownStrategy
                                      , false
                                      )
                        , Player.make ( "Erofei"
                                      , []
                                      , False.ownStrategy
                                      , false
                                      )
                        ]
                      , [ CRD (SKIP, GREEN) ]
                      , []
                      , PROCEED
                      , CLOCKWISE
                      , []
                      )
    val newDsk = execution dsk
    val [plr1, plr2, plr3, plr4] = Desk.getPlayers newDsk
  in
    Player.getName plr1 = "Gerasim"
    andalso Player.getName plr3 = "Erofei"
    andalso cardSort (Player.getCards plr3) = []
    andalso Desk.getState newDsk = PROCEED
    andalso null (Desk.getDeck newDsk)
    andalso Desk.getMemo newDsk = [PASS]
    
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 29 yourTurn
 ******************************************************************************)
val test29_yourTurn1 = 
  let 
    val dsk = Desk.make ( [p1, p2, p3]
                        , [CRD (DRAW_TWO, GREEN)]
                        , [ CRD (NUM 5, GREEN)
                          , CRD (SKIP, BLUE) ]
                        , EXECUTE, CLOCKWISE, [] )
    val newDsk = yourTurn dsk
    val [plr1, plr2, plr3] = Desk.getPlayers newDsk
  in
    Player.getName plr1 = "Pavel"
    andalso Player.getName plr3 = "Sergey"
    andalso cardSort (Player.getCards plr3) = [ CRD (NUM 5, GREEN)
                                              , CRD (SKIP, BLUE) ]
    andalso Desk.getState newDsk = PROCEED
    andalso null (Desk.getDeck newDsk)
    andalso Desk.getMemo newDsk = [PASS]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 30 playerLoss
 ******************************************************************************)
val test30_playerLoss1 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, GREEN)
                                   , CRD (SKIP, BLUE) ] )
  in 
    playerLoss p1 = ("Sergey", 25)
  end

val test30_playerLoss2 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, GREEN)
                                   , WILD ] )
  in 
    playerLoss p1 = ("Sergey", 55)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 31 checkWinner
 ******************************************************************************)
val test30_checkWinner1 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, GREEN)
                                   , CRD (SKIP, BLUE) ] )
    val p2 = Player.setCards ( p2, [ CRD (NUM 4, YELLOW)
                                   , CRD (NUM 1, GREEN)
                                   , CRD (SKIP, RED) ] )
    val p3 = Player.setCards ( p3, [ CRD (NUM 4, RED)
                                   , WILD
                                   , CRD (DRAW_TWO, YELLOW) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [], [], PROCEED, CLOCKWISE, [] )
  in 
    checkWinner dsk = NONE
  end

val test30_checkWinner2 = 
  let 
    val p1 = Player.setCards ( p1, [ CRD (NUM 5, GREEN)
                                   , CRD (SKIP, BLUE) ] )
    val p2 = Player.setCards ( p2, [ CRD (NUM 4, YELLOW)
                                   , CRD (NUM 1, GREEN)
                                   , CRD (SKIP, RED) ] )
    val dsk = Desk.make ( [p1, p2, p3]
                        , [], [], PROCEED, CLOCKWISE, [] )
  in 
    checkWinner dsk 
    = SOME ("Saveliy", [ ("Sergey", 25), ("Pavel", 25), ("Saveliy", 0)])
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 32 game
 ******************************************************************************)
(* Тестирование следует проводить отладив все  *)
(* Функция должна успешно начинать и завершать игру *)
(******************************************************************************)

