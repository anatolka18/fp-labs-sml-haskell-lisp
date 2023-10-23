(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №3

  НЕ СЛЕДУЕТ УДАЛЯТЬ ИЛИ ПЕРЕСТАВЛЯТЬ МЕСТАМИ ЭЛЕМЕНТЫ, 
  ПРЕДСТАВЛЕННЫЕ В ШАБЛОНЕ (ВКЛЮЧАЯ КОММЕНТАРИИ). 
  ЭЛЕМЕНТЫ РЕШЕНИЯ СЛЕДУЕТ ВПИСЫВАТЬ В ПРОМЕЖУТКИ,
  ОПРЕДЕЛЕННЫЕ КОММЕНТАРИЯМИ.
 ******************************************************************************)

(****************************************************************************** 
  Загрузка предварительных определений типов данных и вспомогательных функций 
 ******************************************************************************)
use "lab-3-use.sml";

(****************************************************************************** 
  Задание 1 precedes
 ******************************************************************************)
fun precedes _ [] _ _ =  raise IllegalGame
  | precedes f (h :: lst) a1 a2 = 
      f (h, a1) orelse not (f (h, a2)) andalso precedes f lst a1 a2
(******************************************************************************)

(****************************************************************************** 
  Задание 2 colorIsLT и rankIsLT
 ******************************************************************************)
fun colorIsLT a1 a2 = precedes isSameColor colors a1 a2

fun rankIsLT a1 a2 = precedes isSameRank ranks a1 a2
(******************************************************************************)

(****************************************************************************** 
  Задание 3 isLT
 ******************************************************************************)
fun isLT (_, WILD_DRAW_FOUR) = true
  | isLT (WILD_DRAW_FOUR, WILD) = false
  | isLT (_, WILD) = true
  | isLT (CRD (r1, c1), CRD (r2, c2)) = 
      if isSameColor (c1, c2) then rankIsLT r1 r2 else colorIsLT c1 c2
  | isLT (_, _) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 4 cardSort
 ******************************************************************************)
fun cardSort lst = ListMergeSort.sort isLT lst
(******************************************************************************)

(****************************************************************************** 
  Задание 5 cardValue
 ******************************************************************************)
fun cardValue a =
  case a 
    of CRD (NUM r, _) => r
     | CRD _ => 20
     | _ => 50
(******************************************************************************)

(****************************************************************************** 
  Задание 6 sumCards
 ******************************************************************************)
fun sumCards lst = foldl (fn (l, acc) => acc + cardValue l) 0 lst
(******************************************************************************)

(****************************************************************************** 
  Задание 7 oppositeDir
 ******************************************************************************)
fun oppositeDir d = 
  case d
    of CLOCKWISE => COUNTERCLOCKWISE
     | COUNTERCLOCKWISE => CLOCKWISE
(******************************************************************************)

(****************************************************************************** 
  Задание 8 removeCard
 ******************************************************************************)
fun removeCard (cs, c, e) =
  let
    fun removeCardIter ([], _) = raise e
      | removeCardIter (h :: lst, acc) = 
          if isSameCard (h, c) then List.revAppend (acc, lst)
          else removeCardIter (lst, h :: acc)
  in
    removeCardIter (cs, [])
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 9 cardCount
 ******************************************************************************)
fun cardCount a = 
  case a 
    of (CRD (NUM 0, _)) => 1
     | (CRD _) => 2
     | _ => 4
(******************************************************************************)

(****************************************************************************** 
  Задание 10 deck
 ******************************************************************************)
val deck =
  let 
    val allCards =
      WILD
      :: WILD_DRAW_FOUR
      :: List.concat (map (fn c => map (fn r => CRD (r, c)) ranks) colors)
  in
    foldr (fn (lst, acc) => addNCopies (lst, acc, cardCount lst)) [] allCards
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 11 deal
  Заготовка для вывода сообщения:
  val _ = print ("\nPlayer " ^ Player.getName plr ^ " deals.\n")
 ******************************************************************************)
fun deal (pList as (plr :: pTl)) =
      let
        val _ = print ("\nPlayer " ^ Player.getName plr ^ " deals.\n")
        fun dealIter (deckCur, plrCur :: plrT, pLNew) =
              let val (deckCurAfter, plrCardsAfter) = pass (deckCur, [], 7)
              in dealIter ( deckCurAfter
                          , plrT
                          , Player.setCards (plrCur, plrCardsAfter) :: pLNew
                          )
              end
          | dealIter (upCard :: deckCur, [], pLNew) = 
              Desk.make ( List.rev pLNew
                        , [upCard]
                        , deckCur
                        , PROCEED
                        , CLOCKWISE
                        , []
                        )
          | dealIter _ = raise IllegalGame
      in dealIter (shuffleList deck, pList, [])
      end
  | deal _ = raise IllegalGame
(******************************************************************************)

(****************************************************************************** 
  Задание 12 nextPlayer
 ******************************************************************************)
fun nextPlayer d =
  let
    val pList as (plr :: pTl) = Desk.getPlayers d
    val (plrR :: pTlR) = List.rev pList
  in
    case Desk.getDirection d
      of CLOCKWISE => Desk.whoseTurn (Desk.setPlayers (d, pTl @ [plr]))
       | COUNTERCLOCKWISE => 
           Desk.whoseTurn (Desk.setPlayers (d, plrR :: List.rev pTlR))
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 13 deskNormalize
 ******************************************************************************)
fun deskNormalize d =
  let 
    val pileC :: _ = Desk.getPile d
    val pileN :: deckN = shuffleList (pileC :: Desk.getDeck d)
  in case pileC
       of (WILD | WILD_DRAW_FOUR) => 
            deskNormalize (Desk.setDeck (Desk.setPile (d, [pileN]), deckN))
        | CRD (REVERSE, _) =>
            Desk.setDirection (d, oppositeDir (Desk.getDirection d))
        | (CRD (SKIP, _) | CRD (DRAW_TWO, _)) => 
            Desk.setState (nextPlayer d, EXECUTE)
        | _ => nextPlayer d
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 14 draw
 ******************************************************************************)
fun draw d = 
  case Desk.getDeck d
    of [] => let
               val pC :: dC = Desk.getPile d
             in 
               draw (Desk.setDeck (Desk.setPile (d, [pC]), shuffleList dC))
             end
     | _ => let
              val dH :: dT = Desk.getDeck d
              val plr :: plrs = Desk.getPlayers d
            in Desk.setDeck ( Desk.setPlayers 
                                ( d, Player.setCards ( plr
                                                     , dH :: Player.getCards plr
                                                     ) :: plrs )
                            , dT
                            )
            end
(******************************************************************************)

(****************************************************************************** 
  Задание 15 drawTwo
 ******************************************************************************)
fun drawTwo d = (draw o draw) d
(******************************************************************************)

(****************************************************************************** 
  Задание 16 drawFour
 ******************************************************************************)
fun drawFour d = (drawTwo o drawTwo) d
(******************************************************************************)

(****************************************************************************** 
  Задание 17 playableWhenWild
 ******************************************************************************)
fun playableWhenWild col card =
  case card
    of CRD (_, c) => isSameColor (col, c)
     | _ => true
(******************************************************************************)

(****************************************************************************** 
  Задание 18 playableWhenExec
 ******************************************************************************)
fun playableWhenExec rank card =
  case card
    of CRD (r, _) => isSameRank (rank, r)
     | _ => false
(******************************************************************************)

(****************************************************************************** 
  Задание 19 playableWhenProc
 ******************************************************************************)
fun playableWhenProc (rank, col) card =
  case card
    of CRD (r, c) => isSameRank (rank, r) orelse isSameColor (col, c)
     | _ => true
(******************************************************************************)

(****************************************************************************** 
  Задание 20 cardsToPlay
 ******************************************************************************)
fun cardsToPlay d = 
  let 
    val plr :: _ = Desk.getPlayers d
    val pileC :: _ = Desk.getPile d
    val plrCard = Player.getCards plr
  in 
    case (Desk.getState d, pileC)
      of (PROCEED, CRD (r, c)) => 
           List.filter (playableWhenProc (r, c)) plrCard
       | (EXECUTE, CRD (r, _)) => List.filter (playableWhenExec r) plrCard
       | (GIVE c, _) => List.filter (playableWhenWild c) plrCard
       | _ => raise IllegalGame
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 21 requiredColor
 ******************************************************************************)
fun requiredColor d =
  case Desk.getState d
    of GIVE col => col
     | _ => let 
              val CRD (_, col) :: _ = Desk.getPile d
            in col
            end
(******************************************************************************)

(****************************************************************************** 
  Задание 22 hasColor
 ******************************************************************************)
fun hasColor (col, cs) = 
  isSome (List.find (fn CRD (_, c) => isSameColor (c, col) | _ => false) cs)
(******************************************************************************)

(****************************************************************************** 
  Задание 23 countCards
 ******************************************************************************)
fun countCards d = 
  map (fn f => List.length (Player.getCards f)) (Desk.getPlayers d)
(******************************************************************************)

(****************************************************************************** 
  Модуль Manual с функцией стратегии для тестирования
  функция Manual.ownStrategy выводит на экран все аргументы, переданные в 
  функцию стратегии, и позволяет пользователю выбрать нужный ход вручную.
  Функция может вернуть значение PASS в случае, если пользователь
  делает некорректный выбор из предложенных списков. 

  ФУНКЦИЮ МОЖНО РАСКОММЕНТИРОВАТЬ ПОСЛЕ РЕАЛИЗАЦИИ ФУНКЦИИ cardSort
 ******************************************************************************)
structure Manual :> STRATEGY = struct
  fun ownStrategy ( st, crdsOnHand, playCrds, crdOnPile
                  , dir, lastMoveList, crdCountList ) =
    let
      val _ = print ("State: " ^ stateToString st ^ "\n")
      val crdsOnHandSorted = cardSort crdsOnHand
      val _ = print ( "All cards on hand: \n" 
                      ^ listToStringLn cardToString crdsOnHandSorted ^ "\n" )
      val playCrdsSorted = cardSort playCrds
      val _ = print ( "Playable cards: \n" 
                      ^ listToStringLn cardToString playCrdsSorted ^ "\n" )
      val _ = print ("A card on the pile: " ^ cardToString crdOnPile ^ "\n")
      val _ = print ("Game direction: " ^ directionToString dir ^ "\n")
      val _ = print ( "A list of last moves: \n" 
                      ^ listToStringLn moveToString lastMoveList ^ "\n" )
      val _ = 
        print ( "A list of the number of cards in the hands of the players: \n" 
                ^ listToString Int.toString crdCountList ^ "\n" )
      val _ = print ( "Enter the number of the selected card "
                      ^ "from the list of playable cards (from 0). \n" )
      val _ = print ( "Any other number to switch to automatic mode. \n")
      val str = valOf (TextIO.inputLine TextIO.stdIn)
      val numberOfCard = (valOf (Int.fromString str)) handle Option => ~1
      val mv = (SIMPLE (List.nth (playCrdsSorted, numberOfCard)))
                handle Subscript => PASS
    in
      case mv
        of SIMPLE (CRD _) => mv
         | PASS => mv
         | SIMPLE crd =>
             let 
               val _ = print ( "Enter the number of color from list (from 0) \n"
                               ^ listToString colorToString colors ^ "\n" )
               val _ = print ( "Any other number to switch "
                               ^ "to automatic mode. \n" )
               val str = valOf (TextIO.inputLine TextIO.stdIn)
               val numberOfCol = (valOf (Int.fromString str)) 
                                  handle Option => ~1
             in 
                (ORDER (crd, List.nth (colors, numberOfCol)))
                 handle Subscript => PASS
             end 
         | _ => raise IllegalGame 
    end
end
(******************************************************************************)

(****************************************************************************** 
  Задание 24 Naive.ownStrategy
 ******************************************************************************)
structure Naive :> STRATEGY = struct
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
(******************************************************************************)

(****************************************************************************** 
  Задание 25 moveAction
 ******************************************************************************)
fun moveAction (mv, dsk, hasColor) =
  case mv
    of ORDER (WILD_DRAW_FOUR, col) =>
         if hasColor 
         then nextPlayer (drawFour (Desk.setState (dsk, GIVE col)))
         else nextPlayer 
                ( Desk.addMemo 
                  ( drawFour (nextPlayer (Desk.setState (dsk, GIVE col)))
                  , PASS
                  )
                )
     | ORDER (WILD, col) => nextPlayer (Desk.setState (dsk, GIVE col))
     | SIMPLE (CRD (REVERSE, _)) =>
         nextPlayer ( Desk.setDirection ( Desk.setState (dsk, PROCEED)
                                        , oppositeDir (Desk.getDirection dsk) 
                                        )
                    )
     | SIMPLE (CRD (NUM _, _)) => nextPlayer (Desk.setState (dsk, PROCEED))
     | _ =>  nextPlayer (Desk.setState (dsk, EXECUTE))
(******************************************************************************)

(****************************************************************************** 
  Задание 26 askPlayerForCard
 ******************************************************************************)
fun askPlayerForCard (plr, args) =
  if Player.isOnManual plr 
  then case Manual.ownStrategy args
         of PASS => askPlayerForCard (Player.automate plr, args)
          | action => (plr, action)
  else (plr, (Player.getStrategy plr) args)
(******************************************************************************)

(****************************************************************************** 
  Задание 27 play
  Заготовка для вывода сообщения:
  val _ = print (playerName ^ " made move\n")
  val _ = print (moveToString mv ^ "\n")
 ******************************************************************************)
fun play (dsk, playCards) = 
  let
    val plr :: plrs = Desk.getPlayers dsk
    val pileC :: _ = Desk.getPile dsk
    val args = ( Desk.getState dsk
               , Player.getCards plr
               , playCards
               , pileC
               , Desk.getDirection dsk
               , Desk.getMemo dsk
               , countCards dsk 
               )
    val (plrAfAsk, strat) = askPlayerForCard (plr, args)
    val namePlr = Player.getName plrAfAsk
    val pickCard = case strat
                     of PASS => raise IllegalMove (namePlr, args)
                      | SIMPLE card => card
                      | ORDER (card, _) => card
    val _ = removeCard (playCards, pickCard, IllegalMove (namePlr, args))
    val _ = print (namePlr ^ " made move\n")
    val _ = print (moveToString strat ^ "\n")
    val plrCard = Player.getCards plr
    val plrCardAfPlay = removeCard ( plrCard
                                   , pickCard
                                   , IllegalMove (namePlr, args) 
                                   )
    val pileAfPlay = pickCard :: (Desk.getPile dsk)
    val deskAfPlay = 
      Desk.setPlayers ( Desk.addMemo (Desk.setPile (dsk, pileAfPlay), strat)
                      , Player.setCards (plr, plrCardAfPlay) :: plrs
                      )
  in moveAction (strat, deskAfPlay, hasColor (requiredColor dsk, plrCard))
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 28 execution
 ******************************************************************************)
fun execution d =
  let
    val plH :: _ = Desk.getPile d
    val mNew = Desk.addMemo (d, PASS)
  in case Desk.getState d
       of EXECUTE => ( case plH
                         of CRD (SKIP, _) => 
                              nextPlayer (Desk.setState (mNew, PROCEED))
                          | CRD (DRAW_TWO, _) =>
                              nextPlayer (Desk.setState (drawTwo mNew, PROCEED))
                          | _ => raise IllegalGame )
        | _ => ( raise IllegalGame )
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 29 yourTurn
 ******************************************************************************)
fun yourTurn d =
  let
    val plC = cardsToPlay d
  in
    if null plC
    then case Desk.getState d
         of EXECUTE => execution d
          | _ => let
                   val dAfDraw = draw d
                   val plCardAfDraw = cardsToPlay dAfDraw
                 in
                   if null plCardAfDraw
                   then nextPlayer (Desk.addMemo (dAfDraw, PASS))
                   else play (dAfDraw, plCardAfDraw)
                 end
    else play (d, plC)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 30 playerLoss
 ******************************************************************************)
fun playerLoss plr = (Player.getName plr, sumCards (Player.getCards plr))
(******************************************************************************)

(****************************************************************************** 
  Задание 31 checkWinner
 ******************************************************************************)
fun checkWinner d =
  let
    val plrs = Desk.getPlayers d
    val wnr = List.find (fn f => null (Player.getCards f)) plrs
  in case wnr 
       of SOME plr => SOME (Player.getName plr, List.map playerLoss plrs)
        | _ => NONE
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 32 game
 ******************************************************************************)
fun game plrs =
  let
    fun gameIter d = 
      case checkWinner d
        of SOME plr => plr
         | _ => gameIter (yourTurn d)
  in
    gameIter (deskNormalize (deal plrs))
  end
(******************************************************************************)