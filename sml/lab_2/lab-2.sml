(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №2

  НЕ СЛЕДУЕТ УДАЛЯТЬ ИЛИ ПЕРЕСТАВЛЯТЬ МЕСТАМИ ЭЛЕМЕНТЫ, 
  ПРЕДСТАВЛЕННЫЕ В ШАБЛОНЕ (ВКЛЮЧАЯ КОММЕНТАРИИ). 
  ЭЛЕМЕНТЫ РЕШЕНИЯ СЛЕДУЕТ ВПИСЫВАТЬ В ПРОМЕЖУТКИ,
  ОПРЕДЕЛЕННЫЕ КОММЕНТАРИЯМИ.
 ******************************************************************************)

(****************************************************************************** 
  Определение выражений языка MUPL как конструкторов значений типа expr 
 ******************************************************************************)
datatype expr = VAR of string
              | INT of int
              | ADD of expr * expr
              | IF_GREATER of expr * expr * expr * expr
              | FUN of (string * string) * expr
              | CALL of expr * expr
              | LET of (string * expr) * expr
              | PAIR of expr * expr
              | HEAD of expr
              | TAIL of expr
              | NULL 
              | IS_NULL of expr
              | CLOSURE of (string * expr) list * expr
(******************************************************************************)

(****************************************************************************** 
  Описание исключения, которое будет подниматься в случае нарушения семантики 
  выражения языка MUPL
 ******************************************************************************)
exception Expr
(******************************************************************************)

(****************************************************************************** 
  Функция превращения строки в строку, в которой имеются знаки кавычек
 ******************************************************************************)
fun strToString str = String.concat ["\"", str, "\""]
(******************************************************************************)

(****************************************************************************** 
  Задание 1 exprToString и pairToString
 ******************************************************************************)
fun exprToString (VAR s) = String.concat ["VAR ", strToString s]
  | exprToString (INT i) = String.concat ["INT ", Int.toString i]
  | exprToString (ADD (e1, e2)) = String.concat [ "ADD ("
                                                , exprToString e1
                                                , ", "
                                                , exprToString e2
                                                , ")"
                                                ]
  | exprToString (FUN ((s1, s2), e)) = String.concat [ "FUN (("
                                                     , strToString s1
                                                     , ", "
                                                     , strToString s2
                                                     , "), "
                                                     , exprToString e
                                                     , ")"
                                                     ]
  | exprToString (CALL (e1, e2)) = String.concat [ "CALL ("
                                                 , exprToString e1
                                                 , ", "
                                                 , exprToString e2
                                                 , ")"
                                                 ]
  | exprToString (LET ((s, e1), e2)) = String.concat [ "LET (("
                                                     , strToString s
                                                     , ", "
                                                     , exprToString e1
                                                     , "), "
                                                     , exprToString e2
                                                     , ")"
                                                     ]
  | exprToString (IF_GREATER (e1, e2, e3, e4)) = 
      String.concat [ "IF_GREATER ("
                    , exprToString e1
                    , ", "
                    , exprToString e2
                    , ", "
                    , exprToString e3
                    , ", "
                    , exprToString e4
                    , ")"
                    ]
  | exprToString (PAIR (e1, e2)) = String.concat [ "PAIR ("
                                                 , exprToString e1
                                                 , ", "
                                                 , exprToString e2
                                                 , ")"
                                                 ]
  | exprToString (HEAD e) = String.concat [ "HEAD ("
                                          , exprToString e
                                          , ")"
                                          ]
  | exprToString (TAIL e) = String.concat [ "TAIL ("
                                          , exprToString e
                                          , ")"
                                          ]
  | exprToString (IS_NULL e) = String.concat [ "IS_NULL ("
                                             , exprToString e
                                             , ")"
                                             ] 
  | exprToString NULL = "NULL"
  | exprToString (CLOSURE (env, f)) = 
      "CLOSURE ([" ^ String.concatWith ", " (map pairToString env) ^ "], "
                   ^ exprToString f ^ ")"
and pairToString (var, expr) = String.concat [ "("
                                             , strToString var
                                             , ", "
                                             , exprToString expr
                                             , ")"
                                             ]
(******************************************************************************)

(****************************************************************************** 
  Функция valOfInt
 ******************************************************************************)
fun valOfInt (INT n) = n
  | valOfInt e = 
      ( print ("The expression " ^ exprToString e ^ " is not a number.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 2 funName
 ******************************************************************************)
fun funName (FUN ((s1, _), _)) = s1
  | funName e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 3 funArg
 ******************************************************************************)
fun funArg (FUN ((_, s2), _)) = s2
  | funArg e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 4 funBody
 ******************************************************************************)
fun funBody (FUN ((_, _), e)) = e
  | funBody e = 
      ( print ("The expression " ^ exprToString e ^ " is not a function.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 5 pairHead
 ******************************************************************************)
fun pairHead (PAIR (e1, _)) = e1
  | pairHead e = 
      ( print ("The expression " ^ exprToString e ^ " is not a pair.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 6 pairTail
 ******************************************************************************)
fun pairTail (PAIR (_, e2)) = e2
  | pairTail e = 
      ( print ("The expression " ^ exprToString e ^ " is not a pair.\n"); 
        raise Expr )
(******************************************************************************)

(****************************************************************************** 
  Задание 7 closureFun
 ******************************************************************************)
fun closureFun (CLOSURE (_, f)) = f
  | closureFun e = 
      ( print ("The expression " ^ exprToString e ^ " is not a closure.\n"); 
        raise Expr )

(******************************************************************************)

(****************************************************************************** 
  Задание 8 closureEnv
 ******************************************************************************)
fun closureEnv (CLOSURE (env, _)) = env
  | closureEnv e = 
      ( print ("The expression " ^ exprToString e ^ " is not a closure.\n"); 
        raise Expr )

(******************************************************************************)

(****************************************************************************** 
  Задание 9 envLookUp
 ******************************************************************************)
fun envLookUp ([], str) =
  ( print ("Unbound variable " ^ str ^ ".\n");
    raise Expr )
  | envLookUp ((s, e) :: tL, str) =
      if s = str then e
      else envLookUp (tL, str)
(******************************************************************************)

(****************************************************************************** 
  Задание 10 evalUnderEnv
 ******************************************************************************)
fun evalUnderEnv (VAR name) env = envLookUp (env, name)
  | evalUnderEnv (INT n) _ = INT n
  | evalUnderEnv (ADD (e1, e2)) env = 
      INT (valOfInt (evalUnderEnv e1 env) + valOfInt (evalUnderEnv e2 env))
  | evalUnderEnv (IF_GREATER (e1, e2, e3, e4)) env = 
      if valOfInt (evalUnderEnv e1 env) > valOfInt (evalUnderEnv e2 env)
      then evalUnderEnv e3 env
      else evalUnderEnv e4 env
  | evalUnderEnv (PAIR (e1, e2)) env = PAIR (evalUnderEnv e1 env
                                            , evalUnderEnv e2 env
                                            )
  | evalUnderEnv (HEAD a) env = pairHead (evalUnderEnv a env)
  | evalUnderEnv (TAIL a) env = pairTail (evalUnderEnv a env)
  | evalUnderEnv NULL _ = NULL
  | evalUnderEnv (IS_NULL e) env = 
      if evalUnderEnv e env = NULL then INT 1
      else INT 0
  | evalUnderEnv (LET ((s, e1), e2)) env =
      evalUnderEnv e2 ((s, evalUnderEnv e1 env) :: env)
  | evalUnderEnv (CLOSURE (env, f)) _ = CLOSURE (env, f)
  | evalUnderEnv (FUN ((s1, s2), e)) env =
      CLOSURE (env, FUN ((s1, s2), e))
  | evalUnderEnv (CALL (e1, e2)) env =
      let
        val v1 = evalUnderEnv e1 env
        val f = closureFun v1
        val name = funName f
        val envV1 = if name = ""
                    then (funArg f, evalUnderEnv e2 env) :: closureEnv v1
                    else (name, v1) :: (funArg f, evalUnderEnv e2 env)
                                    :: closureEnv v1
      in
        evalUnderEnv (funBody f) envV1
      end
(******************************************************************************)

(****************************************************************************** 
  Функция evalExp
 ******************************************************************************)
fun evalExp expr = evalUnderEnv expr []
(******************************************************************************)

(****************************************************************************** 
  Задание 11 ifNull
 ******************************************************************************)
fun ifNull (e1, e2, e3) = IF_GREATER (IS_NULL e1, INT 0, e2, e3)
(******************************************************************************)

(****************************************************************************** 
  Задание 12 mLet
 ******************************************************************************)
fun mLet (h :: t) e = LET (h, mLet t e)
  | mLet [] e = e
(******************************************************************************)

(****************************************************************************** 
  Задание 13 ifEq
 ******************************************************************************)
fun ifEq (e1, e2, e3, e4) = 
  LET ( ( "_x"
        , FUN ( ("", "_y")
              , IF_GREATER (VAR "_y", INT 2, e3, e4)
              )
        )
      , CALL ( VAR "_x"
             , ADD ( IF_GREATER (e1, e2, INT 0, INT 1)
                   , IF_GREATER (e2, e1, INT 0, INT 2)
                   )
             )
      )
(******************************************************************************)

(****************************************************************************** 
  Задание 14 convertListToMUPL
 ******************************************************************************)
fun convertListToMUPL (h :: t) = PAIR (h, convertListToMUPL t)
  | convertListToMUPL [] = NULL
(******************************************************************************)

(****************************************************************************** 
  Задание 15 convertListFromMUPL
 ******************************************************************************)
fun convertListFromMUPL (PAIR (h, t)) = h :: convertListFromMUPL t
  | convertListFromMUPL NULL = []
  | convertListFromMUPL _ = raise Expr
(******************************************************************************)

(****************************************************************************** 
  Задание 16 mMap
 ******************************************************************************)
val mMap =
  FUN ( ("mMap", "f")
      , FUN ( ("", "lst")
            , ifNull ( VAR "lst"
                     , NULL 
                     , PAIR ( CALL (VAR "f", HEAD (VAR "lst"))
                            , CALL ( CALL (VAR "mMap", VAR "f")
                                   , TAIL (VAR "lst")
                                   )
                            )
                     )
            )
      )
(******************************************************************************)

(****************************************************************************** 
  Задание 17 mMapAddN
 ******************************************************************************)
fun mMapAddN n =
  LET ( ("s", n)
      , CALL (mMap, FUN (("", "lst"), ADD (VAR "lst", VAR "s")))
      )
(******************************************************************************)

(****************************************************************************** 
  Задание 18 multAnyXPosY
 ******************************************************************************)
val multAnyXPosY =
    FUN ( ("multAnyXPosY", "_x")
        , FUN ( ("", "_y")
              , IF_GREATER ( VAR "_y"
                           , INT 0
                           , ADD ( CALL ( CALL (VAR "multAnyXPosY", VAR "_x")
                                        , ADD (VAR "_y", INT ~1)
                                        )
                                 , VAR "_x"
                                 )
                           , INT 0
                           )
              )
        )
(******************************************************************************)

(****************************************************************************** 
  Задание 19 fact
 ******************************************************************************)
val fact =
  FUN ( ("fact", "_x")
      , LET ( ("factIter"
              , FUN ( ("_factIter", "acc")
                    , FUN ( ("", "n")
                            , ifEq ( VAR "n"
                                   , INT 0
                                   , VAR "acc"
                                   , CALL ( CALL ( VAR "_factIter"
                                                 , CALL ( CALL ( multAnyXPosY
                                                               , VAR "acc"
                                                               )
                                                        , VAR "n"
                                                        )
                                                 )
                                          , ADD (VAR "n", INT ~1)
                                          )
                                   )
                          )
                    )
              )
            , CALL (CALL (VAR "factIter", INT 1), VAR "_x")
            )
      )
(******************************************************************************)

(****************************************************************************** 
  Задание 20 delDuplicates
 ******************************************************************************)
val delDuplicates =
  FUN ( ("", "l")
      , ifNull ( VAR "l"
               , VAR "l"
               , LET
                   ( ( "rev"
                     , FUN ( ("revI", "_s")
                           , FUN ( ("", "accLR")
                                 , ifNull ( VAR "_s"
                                          , VAR "accLR"
                                          , CALL ( CALL ( VAR "revI"
                                                        , TAIL (VAR "_s")
                                                        )
                                                 , PAIR ( HEAD (VAR "_s")
                                                        , VAR "accLR"
                                                        )
                                                 )
                                          )
                                 )
                           )
                     )
                   , LET
                       ( ( "f"
                         , FUN
                             ( ("delI", "s")
                             , FUN ( ("", "accRL")
                                     , ifNull ( TAIL (VAR "s")
                                              , PAIR ( HEAD (VAR "s")
                                                     , VAR "accRL")
                                              , ifEq ( HEAD (VAR "s")
                                                     , HEAD (TAIL (VAR "s"))
                                                     , CALL
                                                         ( CALL ( VAR "delI"
                                                                , TAIL (VAR "s")
                                                                )
                                                         , VAR "accRL"
                                                         )
                                                      , CALL
                                                          ( CALL ( VAR "delI"
                                                                 , TAIL
                                                                     (VAR "s")
                                                                 )
                                                          , PAIR
                                                              ( HEAD (VAR "s")
                                                              , VAR "accRL"
                                                              )
                                                          )
                                                     )
                                              )
                                   )
                             )
                         )
                         , CALL ( CALL ( VAR "rev"
                                       , CALL ( CALL (VAR "f", VAR "l")
                                              , NULL
                                              )
                                       )
                                , NULL
                                )
                       )
                   )
               )
      )
(******************************************************************************)

