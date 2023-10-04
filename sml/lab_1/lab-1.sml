(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №1

  НЕ СЛЕДУЕТ УДАЛЯТЬ ИЛИ ПЕРЕСТАВЛЯТЬ МЕСТАМИ ЭЛЕМЕНТЫ, 
  ПРЕДСТАВЛЕННЫЕ В ШАБЛОНЕ (ВКЛЮЧАЯ КОММЕНТАРИИ). 
  ЭЛЕМЕНТЫ РЕШЕНИЯ СЛЕДУЕТ ВПИСЫВАТЬ В ПРОМЕЖУТКИ,
  ОПРЕДЕЛЕННЫЕ КОММЕНТАРИЯМИ.
 ******************************************************************************)

(****************************************************************************** 
  Загрузка определений модулей MyDate и Fixed и вспомогательных списков данных 
 ******************************************************************************)
use "lab-1-use.sml";

(****************************************************************************** 
  Задание 1 isLeapYear
 ******************************************************************************)
fun isLeapYear (year : int, isJ : bool) : bool =
  if isJ then year mod 4 = 0
  else year mod 400 = 0 orelse year mod 4 = 0 andalso not (year mod 100 = 0)
(******************************************************************************)

(****************************************************************************** 
  Задание 2 isLongMonth
 ******************************************************************************)
fun isLongMonth (month : int) : bool =
  if month < 8 then month mod 2 = 1
  else month mod 2 = 0
(******************************************************************************)

(****************************************************************************** 
  Задание 3 daysInMonth
 ******************************************************************************)
fun daysInMonth (d : date, isJ : bool) : int =
  let val m = #2 d
  in
    if isLongMonth m then 31
    else
      if m = 2 then
        if isLeapYear (#3 d, isJ) then 29 else 28
      else 30
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 4 isDayOK
 ******************************************************************************)
fun isDayOK (d : date, isJ : bool) : bool =
  let val day = #1 d
  in day >= 1 andalso day <= daysInMonth(d, isJ)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 5 isMonthOK
 ******************************************************************************)
fun isMonthOK (d : date) : bool = 
  let val m = #2 d
  in m >= 1 andalso m <= 12
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 6 isCorrectDate
 ******************************************************************************)
fun isCorrectDate (d : date, isJ : bool) : bool =
  #3 d > 0 andalso isMonthOK d andalso isDayOK (d, isJ) 
(******************************************************************************)

(****************************************************************************** 
  Задание 7 incDateByNum
 ******************************************************************************)
fun incDateByNum (d : date, days : int, isJ : bool) : date =
  let fun addD (d1 : date, daysAdd : int) : date =
    if daysAdd = 0 then d1
    else let val dayInM = daysInMonth (d1, isJ)
             val day = #1 d1
             val month = #2 d1
             val year = #3 d1
         in if daysAdd <= dayInM - day then (day + daysAdd, month, year)
            else let val dToNextM = daysAdd - dayInM + day
                 in if month = 12 then addD ((0, 1, year + 1), dToNextM)
                    else addD ((0, month + 1, year), dToNextM)
                 end
         end
  in addD (d, days)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 8 decDateByNum
 ******************************************************************************)
fun decDateByNum (d : date, days : int, isJ : bool) : date =
  let fun subD (d1 : date, daysSub : int) : date =
    if daysSub = 0 then d1
    else let val day = #1 d1
             val month = #2 d1
             val year = #3 d1
         in if daysSub < day then (day - daysSub, month, year)
            else let
                   val prM = if month = 1 then 12 else month - 1
                   val dayInPrM = daysInMonth ((0, prM, year), isJ)
                   val dToPrM = daysSub - day
                 in if prM = 12 then subD ((dayInPrM, prM, year - 1), dToPrM)
                    else subD ((dayInPrM, prM, year), dToPrM)
                 end
         end
  in subD (d, days)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 9 newStyleCorrection
 ******************************************************************************)
fun newStyleCorrection (data : date) : int =
  let 
    val y = #3 data
    val month = #2 data
    fun days29 (yearCount: int, count : int) : int =
      if yearCount > y then count
      else if yearCount mod 400 = 0 then days29 (yearCount + 100, count)
           else days29 (yearCount + 100, count + 1)
  in if y mod 100 = 0 andalso not (y mod 400 = 0)
     then if month > 2 orelse (month, #1 data) = (2, 29)
          then days29 (0, ~2)
          else days29 (0, ~3)
     else days29 (0, ~2)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 10 toJulianDay
 ******************************************************************************)
fun toJulianDay (data : date) : date =
  decDateByNum (data, newStyleCorrection data, true)
(******************************************************************************)

(****************************************************************************** 
  Задание 11 toGrigorianDay
 ******************************************************************************)
fun toGrigorianDay (data : date) : date =
  let val y = #3 data
      val m = #2 data
      val dDec1 = #1 data - 1
  in
    if (m, dDec1 + 1) = (2, 29) andalso y mod 100 = 0 then
      incDateByNum ( incDateByNum ((dDec1, m, y) 
                   , newStyleCorrection (dDec1, m, y), false)
                   , 1
                   , false )
    else incDateByNum (data, newStyleCorrection data, false)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 12 younger
 ******************************************************************************)
fun younger (date1 : date, date2 : date) : bool =
  let val (d1, m1, y1) = date1
      val (d2, m2, y2) = date2
  in not (y1 < y2 orelse (y1 = y2 andalso (m1 < m2
                  orelse (m1 = m2 andalso d1 <= d2))))
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 13 youngest
 ******************************************************************************)
fun youngest (stud : (string * date) list) : (string * date) option =
  if null stud then NONE
  else if null (tl stud) then SOME (hd stud)
       else let 
              val hdS = hd stud
              val m = valOf (youngest (tl stud))
            in if younger (#2 hdS, #2 m) then SOME hdS
               else SOME m
            end
(******************************************************************************)

(****************************************************************************** 
  Задание 14 getNthFixed
 ******************************************************************************)
fun getNthFixed (n : int, lst : fixed list) : fixed = (* Индекс *)
  if n = 0 then hd lst
  else getNthFixed (n - 1, tl lst)
(******************************************************************************)

(****************************************************************************** 
  Задание 15 numToDigits
 ******************************************************************************)
fun numToDigits (num : int, numD : int) : int list =
  if numD = 0 then [] 
  else num mod 10 :: numToDigits (num div 10, numD - 1)
(******************************************************************************)

(****************************************************************************** 
  Задание 16 listElements
 ******************************************************************************)
fun listElements (pos : int list, lists : fixed list list) : int list =
  if null pos then []
  else getNthFixed (hd pos, hd lists) :: listElements (tl pos, tl lists)
(******************************************************************************)

(****************************************************************************** 
  Задание 17 listSum
 ******************************************************************************)
fun listSum (lst : fixed list) : fixed = 
  if null lst then 0
  else hd lst + listSum (tl lst)
(******************************************************************************)

(****************************************************************************** 
  Задание 18 maxSmaller
 ******************************************************************************)
fun maxSmaller (lst : fixed list, amount : fixed) : fixed =
  let fun findMS (l : fixed list, m : fixed) : fixed =
        if null l then m
        else let val hdL = hd l
             in if hdL < amount andalso hdL > m
                then findMS (tl l, hdL)
                else findMS (tl l, m)
             end
  in findMS (lst, 0)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 19 dateToCorrectionNums
 ******************************************************************************)
fun dateToCorrectionNums (data : date) : int list = 
  let val y = #3 data 
  in #2 data - 1 :: numToDigits (y, 4) @ [y mod 4]
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 20 firstNewMoon
 ******************************************************************************)
fun firstNewMoon (data : date) : (fixed * date) option =
  let val (m, y) = (#2 data, #3 data)
      val listCN = dateToCorrectionNums (if m <= 2 
                                                    then (#1 data, m, y - 1)
                                                    else data)
      val sum = Fixed.fromInt (newStyleCorrection data) 
                      + listSum (listElements (listCN , corrections))
      val fnum  = sum - maxSmaller (reductions, sum - 100000)
      val fTId = Fixed.toInt fnum
  in
    if isCorrectDate ((fTId, m, y), false) then SOME (fnum, (fTId, m, y))
    else NONE
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 21 winterSolstice
 ******************************************************************************)
fun winterSolstice (y : int) : date = 
  ( Fixed.toInt (2250000 + 24220 * y 
                         - 100000 * (y div 4 - y div 100 + y div 400)), 12, y )
(******************************************************************************)

(****************************************************************************** 
  Задание 22 chineseNewYearDate
 ******************************************************************************)
fun chineseNewYearDate (y : int) : date =
  let val yDec1 = y - 1
      val dPrNM = valOf (firstNewMoon (1, 12, yDec1))
      val num  = #1 dPrNM
      val data = #2 dPrNM
      val dDec = #1 data
  in if younger (data, winterSolstice yDec1) 
     then incDateByNum (data, Fixed.toInt (num + 2953059) - dDec , false)
     else incDateByNum (data, Fixed.toInt (num + 5906118) - dDec , false)
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 23 getNthString
 ******************************************************************************)
fun getNthString (n : int, lst : string list) : string = (* Индекс *)
  if n = 0 then hd lst
  else getNthString(n - 1, tl lst)
(******************************************************************************)

(****************************************************************************** 
  Задание 24 dateToString
 ******************************************************************************)
fun dateToString (data : date) : string =
  getNthString (#2 data - 1, months) ^ " " ^ Int.toString (#1 data)
                                     ^ ", " ^ Int.toString (#3 data)
(******************************************************************************)

(****************************************************************************** 
  Задание 25 chineseYear
 ******************************************************************************)
fun chineseYear (y : int) : string * string * string * string =
  let val nY = (y + 2396) mod 60
      val nC = nY mod 10
      val nT = nY mod 12
  in
    ( getNthString (nC, celestialChi) ^ "-" ^ getNthString (nT, terrestrialChi)
    , getNthString (nC div 2, celestialColor)
    , getNthString (nT, terrestrialEng)
    , getNthString (nC, celestialEng) )
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 26 dateToChineseYear
 ******************************************************************************)
fun dateToChineseYear (data : date) : string * string * string * string =
  let val y = #3 data
  in if younger (chineseNewYearDate y, data) then chineseYear (y - 1)
     else chineseYear y
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 27 dateToAnimal
 ******************************************************************************)
fun dateToAnimal (data : date) : string =
  #3 (dateToChineseYear data)
(******************************************************************************)

(****************************************************************************** 
  Задание 28 animal
 ******************************************************************************)
fun animal (name : string * date) : string =
  dateToAnimal (#2 name)
(******************************************************************************)

(****************************************************************************** 
  Задание 29 extractAnimal
 ******************************************************************************)
fun extractAnimal (n : (string * date) list, nA : string) (* фильтр *)
                  : (string * date) list =
  if null n then []
  else let val h = hd n
       in if animal h = nA then h :: extractAnimal (tl n, nA)
          else extractAnimal (tl n, nA)
       end
(******************************************************************************)

(****************************************************************************** 
  Задание 30 extractAnimals
 ******************************************************************************)
fun extractAnimals (n : (string * date) list, nA : string list)
                   : (string * date) list =
  if null nA then []
  else extractAnimal (n, hd nA) @ extractAnimals (n, tl nA)
(******************************************************************************)

(****************************************************************************** 
  Задание 31 youngestFromAnimals
 ******************************************************************************)
fun youngestFromAnimals (n : (string * date) list, nA : string list)
                        : (string * date) option =
  youngest (extractAnimals (n, nA))

(******************************************************************************)

(****************************************************************************** 
  Задание 32 oldStyleStudents
 ******************************************************************************)
fun oldStyleStudents (n : (string * date) list) : (string * date) list =
  if null n then []
  else let val h = hd n 
       in (#1 h, toGrigorianDay (#2 h)) :: oldStyleStudents (tl n)
       end
(******************************************************************************)

(****************************************************************************** 
  Задание 33 youngestFromOldStyleAnimals
 ******************************************************************************)
fun youngestFromOldStyleAnimals (n : (string * date) list, nA : string list)
                                : (string * date) option = 
  let val nY = youngestFromAnimals (oldStyleStudents n, nA)
  in 
    if isSome nY
    then let val vOFnY = valOf nY
         in SOME (#1 vOFnY, toJulianDay (#2 vOFnY))
         end
    else nY
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 34 listOfStringDates
 ******************************************************************************)
fun listOfStringDates (n : (string * date) list) : (string * string) list =
  if null n then []
  else let val h = hd n 
       in (#1 h, dateToString (#2 h)) :: listOfStringDates (tl n)
       end
(******************************************************************************)

(****************************************************************************** 
  Задание 35 oldStyleStudentStringDates
 ******************************************************************************)
fun oldStyleStudentStringDates (n : (string * date) list) 
                               : (string * string) list =
  listOfStringDates (oldStyleStudents n)
(******************************************************************************)
