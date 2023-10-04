(****************************************************************************** 
  Шаблон для выполнения заданий лабораторной работы №1
 ******************************************************************************)

(****************************************************************************** 
  Загрузка файла с лабораторной работой 
 ******************************************************************************)
use "lab-1.sml";

(****************************************************************************** 
  Вспомогательные функции
 ******************************************************************************)
(* Удаляет элемент c из списка cs *)
fun rem (cs, c) = 
  if null cs then raise List.Empty 
  else let val head = hd cs
       in if head = c then tl cs
          else head :: rem (tl cs, c)
       end

(* Проверяет, что списки l1 и l2 состоят из одних и тех же элементов *)
fun sameList (l1, l2) =
  if null l1 then 
    if null l2 then true
    else false
  else (sameList (tl l1, rem (l2, hd l1)) handle List.Empty => false)

(****************************************************************************** 
  ТЕСТЫ К РЕШЕНИЯМ
  Здесь приведены по большей части тривиальные тесты. Их успешное выполнение 
  не гарантирует того, что Ваше решение функционирует правильно.
 ******************************************************************************)
(****************************************************************************** 
  Задание 1 isLeapYear
 ******************************************************************************) 
val test1_IsLeapYear1 = isLeapYear (2000, false) = true 
val test1_IsLeapYear2 = isLeapYear (2015, false) = false
val test1_IsLeapYear3 = isLeapYear (2016, true) = true
val test1_IsLeapYear4 = isLeapYear (2017, true) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 2 isLongMonth
 ******************************************************************************)
val test2_IsLongMonth1 = isLongMonth 12 = true 
val test2_IsLongMonth2 = isLongMonth 2 = false
val test2_IsLongMonth3 = isLongMonth 8 = true
val test2_IsLongMonth4 = isLongMonth 1 = true
(******************************************************************************)

(****************************************************************************** 
  Задание 3 daysInMonth
 ******************************************************************************)
val test3_DaysInMonth1 = daysInMonth ((1,9,2020), false) = 30 
val test3_DaysInMonth2 = daysInMonth ((1,1,1900), true) = 31 
val test3_DaysInMonth3 = daysInMonth ((1,2,2020), false) = 29 
val test3_DaysInMonth4 = daysInMonth ((1,2,2021), true) = 28 
val test3_DaysInMonth5 = daysInMonth ((1,7,2020), false) = 31 
val test3_DaysInMonth6 = daysInMonth ((1,8,1948), true) = 31 
(******************************************************************************)

(****************************************************************************** 
  Задание 4 isDayOK
 ******************************************************************************)
val test4_IsDayOK1 = isDayOK ((1,9,2020), false) = true
val test4_IsDayOK2 = isDayOK ((31,6,2020), true) = false
val test4_IsDayOK3 = isDayOK ((50,4,1534), true) = false
val test4_IsDayOK4 = isDayOK ((16,8,1948), false) = true
val test4_IsDayOK5 = isDayOK ((140,1,2020), true) = false
val test4_IsDayOK6 = isDayOK ((~3,12,1534), true) = false
val test4_IsDayOK7 = isDayOK ((1,3,2020), false) = true
(******************************************************************************)

(****************************************************************************** 
  Задание 5 isMonthOK
 ******************************************************************************)
val test5_IsMonthOK1 = isMonthOK (1,9,2020) = true
val test5_IsMonthOK2 = isMonthOK (31,6,2020) = true
val test5_IsMonthOK3 = isMonthOK (1,13,2020) = false
val test5_IsMonthOK4 = isMonthOK (1,~1,2020) = false
val test5_IsMonthOK5 = isMonthOK (31,0,2020) = false
val test5_IsMonthOK6 = isMonthOK (1,15,2020) = false
(******************************************************************************)

(****************************************************************************** 
  Задание 6 isCorrectDate
 ******************************************************************************)
val test6_IsCorrectDate1 = isCorrectDate ((1,9,2016), true) = true
val test6_IsCorrectDate2 = isCorrectDate ((21,13,1900), false) = false
val test6_IsCorrectDate3 = isCorrectDate ((~12,3,1857), false) = false
val test6_IsCorrectDate4 = isCorrectDate ((29,2,1857), false) = false
val test6_IsCorrectDate5 = isCorrectDate ((8,8,1345), true) = true
val test6_IsCorrectDate6 = isCorrectDate ((40,5,1900), false) = false
val test6_IsCorrectDate7 = isCorrectDate ((29,2,2020), false) = true
(******************************************************************************)

(****************************************************************************** 
  Задание 7 incDateByNum
 ******************************************************************************)
val test7_IncDateByNum1 = incDateByNum ((1,1,2020), 3, true) = (4,1,2020)
val test7_IncDateByNum2 = incDateByNum ((28,2,2004), 3, false) = (2,3,2004)
val test7_IncDateByNum3 = incDateByNum ((31,12,2020), 1, false) = (1,1,2021)
val test7_IncDateByNum4 = incDateByNum ((31,12,2020), 100, false) = (10,4,2021)
val test7_IncDateByNum5 = incDateByNum ((17,1,2016), 46, true) = (3,3,2016)
val test7_IncDateByNum6 = incDateByNum ((13,9,2019), 500, false) = (25,1,2021)
val test7_IncDateByNum7 = incDateByNum ((9,2,1700), 20, false) = (1,3,1700)
(******************************************************************************)

(****************************************************************************** 
  Задание 8 decDateByNum
 ******************************************************************************)
val test8_DecDateByNum1 = decDateByNum ((4,1,2020), 3, true) = (1,1,2020)
val test8_DecDateByNum2 = decDateByNum ((2,3,2004), 3, false) = (28,2,2004)
val test8_DecDateByNum3 = decDateByNum ((1,1,2021), 1, false) = (31,12,2020)
val test8_DecDateByNum4 = decDateByNum ((9,3,2020), 20, false) = (18,2,2020)
val test8_DecDateByNum5 = decDateByNum ((9,3,1700), 20, false) = (17,2,1700)
val test8_DecDateByNum6 = decDateByNum ((9,3,1700), 20, true) = (18,2,1700)
(******************************************************************************)

(****************************************************************************** 
  Задание 9 newStyleCorrection
 ******************************************************************************)
val test9_NewStyleCorrection1 = newStyleCorrection (1,3,2016) = 13
val test9_NewStyleCorrection1x = newStyleCorrection (28,2,2016) = 13
val test9_NewStyleCorrection2 = newStyleCorrection (28,2,1700) = 10
val test9_NewStyleCorrection3 = newStyleCorrection (1,1,400) = 1
val test9_NewStyleCorrection4 = newStyleCorrection (28,2,1600) = 10
val test9_NewStyleCorrection5 = newStyleCorrection (27,4,300) = 1
val test9_NewStyleCorrection6 = newStyleCorrection (5,2,500) = 1
val test9_NewStyleCorrection7 = newStyleCorrection (23,8,2400) = 16
val test9_NewStyleCorrection8 = newStyleCorrection (23,3,500) = 2
val test9_NewStyleCorrection9 = newStyleCorrection (29,2,2024) = 13
val test9_NewStyleCorrection10 = newStyleCorrection (25,9,1800) = 12
val test9_NewStyleCorrection11 = newStyleCorrection (1,3,2016) = 13
val test9_NewStyleCorrection12 = newStyleCorrection (28,2,1600) = 10
val test9_NewStyleCorrection17 = newStyleCorrection (1,3,1600) = 10
val test9_NewStyleCorrection13 = newStyleCorrection (1,1,2021) = 13
val test9_NewStyleCorrection14 = newStyleCorrection (1,10,2007) = 13
val test9_NewStyleCorrection15 = newStyleCorrection (28,8,4745) = 34
val test9_NewStyleCorrection18 = newStyleCorrection (29,1,300) = 0
val test9_NewStyleCorrection19 = newStyleCorrection (28,2,1700) = 10
(******************************************************************************)

(****************************************************************************** 
  Задание 10 toJulianDay
 ******************************************************************************)
val test10_ToJulianDay1 = toJulianDay (7,1,2016) = (25,12,2015)
val test10_ToJulianDay2 = toJulianDay (14,1,2021) = (1,1,2021)
val test10_ToJulianDay3 = toJulianDay (28,2,1600) = (18,2,1600)
val test10_ToJulianDay4 = toJulianDay (10,1,2400) = (25,12,2399)
val test10_ToJulianDay5 = toJulianDay (23,9,2020) = (10,9,2020)
val test10_ToJulianDay6 = toJulianDay (24,2,1800) = (13,2,1800)
val test10_ToJulianDay7 = toJulianDay (24,2,1700) = (14,2,1700)
val test10_ToJulianDay8 = toJulianDay (4,5,202) = (4,5,202)
val test10_ToJulianDay9 = toJulianDay (4,7,2020) = (21,6,2020)
val test10_ToJulianDay10 = toJulianDay (1,1,400) = (31,12,399)
val test10_ToJulianDay11 = toJulianDay (1,3,2021) = (16,2,2021)
(******************************************************************************)

(****************************************************************************** 
  Задание 11 toGrigorianDay
 ******************************************************************************)
val test11_ToGrigorianDay1 = toGrigorianDay (25,12,2015) = (7,1,2016)
val test11_ToGrigorianDay2 = toGrigorianDay (1,1,2021) = (14,1,2021)
val test11_ToGrigorianDay3 = toGrigorianDay (16,2,2020) = (29,2,2020)
val test11_ToGrigorianDay4 = toGrigorianDay (16,2,2019) = (1,3,2019)
val test11_ToGrigorianDay5 = toGrigorianDay (24,5,400) = (25,5,400)
val test11_ToGrigorianDay6 = toGrigorianDay (19,9,202) = (19,9,202)
val test11_ToGrigorianDay7 = toGrigorianDay (29,2,300) = (1,3,300)
val test11_ToGrigorianDay8 = toGrigorianDay (31,12,2075) = (13,1,2076)
val test11_ToGrigorianDay9 = toGrigorianDay (31,12,2200) = (15,1,2201)
val test11_ToGrigorianDay10 = toGrigorianDay (19,8,1403) = (28,8,1403)
val test11_ToGrigorianDay11 = toGrigorianDay (19,2,2000) = (3,3,2000)
val test11_ToGrigorianDay12 = toGrigorianDay (29,2,1700) = (11,3,1700)
(******************************************************************************)

(****************************************************************************** 
  Задание 12 younger
 ******************************************************************************)
val test12_Younger1 = younger ((2,3,4), (1,2,3)) = true
val test12_Younger2 = younger ((1,2,3), (2,3,4)) = false
val test12_Younger3 = younger ((3,2,4), (3,2,4)) = false
val test12_Younger4 = younger ((2,2,4), (3,2,4)) = false
val test12_Younger5 = younger ((3,2,4), (3,3,4)) = false
val test12_Younger6 = younger ((3,3,4), (3,2,4)) = true
val test12_Younger7 = younger ((1,9,2015), (1,9,1955)) = true
(******************************************************************************)

(****************************************************************************** 
  Задание 13 youngest
 ******************************************************************************)
val test13_Youngest1 = 
  youngest [ ("Ivan", (1,9,1980))
           , ("Svetlana", (1,9,2015))
           , ("Alex", (1,9,1955)) ] 
  = SOME ("Svetlana", (1,9,2015))
val test13_Youngest2 = 
  youngest [ ("Ivan", (1,9,1980))
           , ("Svetlana", (1,9,2015))
           , ("Alex", (1,9,1955))
           , ("Marina", (7,8,2015)) ] 
  = SOME ("Svetlana", (1,9,2015))
val test13_Youngest3 = 
  youngest [ ("Ivan", (1,9,1980))
           , ("Svetlana", (1,9,2015))
           , ("Alex", (1,9,1955))
           , ("Marina", (7,8,2015))
           , ("Nick", (2,5,2020)) ] 
  = SOME ("Nick", (2,5,2020))
val test13_Youngest4 = 
  youngest [ ("Ivan", (1,9,1980))
           , ("Svetlana", (1,9,2015))
           , ("Alex", (1,9,1955))
           , ("Marina", (7,8,2016))
           , ("Larina", (1,9,1976)) ] 
  = SOME ("Marina", (7,8,2016))
val test13_Youngest5 = 
youngest [ ("Ivan", (1,9,1980))
         , ("Svetlana", (1,9,2015))
         , ("Alex", (1,9,1955))
         , ("Nina", (7,8,2016))
         , ("Marina", (7,8,2016))
         , ("Larina", (1,9,1976)) ] 
  = SOME ("Marina", (7,8,2016))
(******************************************************************************)

(****************************************************************************** 
  Задание 14 getNthFixed
 ******************************************************************************)
val test14_GetNthFixed1 = getNthFixed (3, [25, ~615, 834, ~38, 0]) = ~38
val test14_GetNthFixed2 = getNthFixed (0, [25, ~615, 834, ~38, 0]) = 25
val test14_GetNthFixed3 = getNthFixed (1, [25, ~615, 834, ~38, 0]) = ~615
val test14_GetNthFixed4 = getNthFixed (2, [25, ~615, 834, ~38, 0]) = 834
val test14_GetNthFixed5 = getNthFixed (4, [25, ~615, 834, ~38, 0]) = 0
val test14_GetNthFixed6 = getNthFixed (0, [6, 4]) = 6
val test14_GetNthFixed7 = getNthFixed (2, [~3, ~2, 5]) = 5
val test14_GetNthFixed8 = getNthFixed (4, [1, 2, ~3, ~4, 89]) = 89
(******************************************************************************)

(****************************************************************************** 
  Задание 15 numToDigits
 ******************************************************************************)
val test15_numToDigits1 = numToDigits (25313, 4) = [3, 1, 3, 5]
val test15_numToDigits2 = numToDigits (111, 4) = [1, 1, 1, 0]
val test15_numToDigits3 = numToDigits (1234, 5) = [4, 3, 2, 1, 0]
val test15_numToDigits4 = numToDigits (1, 5) = [1, 0, 0, 0, 0]
val test15_numToDigits5 = numToDigits (8, 1) = [8]
val test15_numToDigits6 = numToDigits (6371, 3) = [1, 7, 3]
val test15_numToDigits7 = numToDigits (12, 4) = [2, 1, 0, 0]
val test15_numToDigits8 = numToDigits (87654372, 5) = [2, 7, 3, 4, 5]
val test15_numToDigits9 = numToDigits (25313, 2) = [3, 1]
val test15_numToDigits10 = numToDigits (111, 2) = [1, 1]
(******************************************************************************)

(****************************************************************************** 
  Задание 16 listElements
 ******************************************************************************)
val test16_listElements1 = 
  listElements ( [3,2,5,6]
               , [ [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 ]
               ) 
  = [4, 3, 6, 7]
val test16_listElements2 = 
  listElements ( [5,1,3,0,2]
               , [ [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 ]
               ) 
  = [6, 2, 4, 1, 3]
val test16_listElements3 = 
  listElements ( [1,1,1,1]
               , [ [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 ]
               ) 
  = [2, 2, 2, 2]

val test16_listElements4 = 
  listElements ( [6,5,2,3]
               , [ [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 ]
               ) 
  = [7, 6, 3, 4]
val test16_listElements5 = 
  listElements ( [3,2,5]
               , [ [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 , [1,2,3,4,5,6,7]
                 ]
               ) 
  = [4, 3, 6]
(******************************************************************************)

(****************************************************************************** 
  Задание 17 listSum
 ******************************************************************************)
val test17_listSum1 = listSum [3,2,5,6] = 16
val test17_listSum2 = listSum [0,1,2,3] = 6
val test17_listSum3 = listSum [] = 0
val test17_listSum4 = listSum [3] = 3
val test17_listSum5 = listSum [0,0] = 0
(******************************************************************************)

(****************************************************************************** 
  Задание 18 maxSmaller
 ******************************************************************************)
val test18_MaxSmaller1 = maxSmaller ([25, 0, 3800, 834], 3654) = 834 
val test18_MaxSmaller2 = maxSmaller ([25, 0, 3800, 834], 4000) = 3800 
val test18_MaxSmaller3 = maxSmaller ([25, 0, 3800, 834], 56) = 25 
val test18_MaxSmaller4 = maxSmaller ([25, 4567, 3800, 834], 3) = 0 
val test18_MaxSmaller5 = maxSmaller ([25, 1, 3800, 834], 3) = 1 
val test18_MaxSmaller6 = maxSmaller ([25, 1, 3800, 834], 1) = 0 
val test18_MaxSmaller7 = maxSmaller ([], 1) = 0 
(******************************************************************************)

(****************************************************************************** 
  Задание 19 dateToCorrectionNums
 ******************************************************************************)
val test19_dateToCorrectionNums1 = 
  dateToCorrectionNums (20, 9, 2021) = [8,1,2,0,2,1]
val test19_dateToCorrectionNums2 = 
  dateToCorrectionNums (15, 1, 1987) = [0,7,8,9,1,3]
val test19_dateToCorrectionNums3 = 
  dateToCorrectionNums (3, 12, 2020) = [11,0,2,0,2,0]
val test19_dateToCorrectionNums4 = 
  dateToCorrectionNums (20, 11, 1974) = [10,4,7,9,1,2]
(******************************************************************************)

(****************************************************************************** 
  Задание 20 firstNewMoon
 ******************************************************************************)
val test20_FirstNewMoon1 = firstNewMoon (1,9,2016) = SOME (170823, (1,9,2016))
val test20_FirstNewMoon2 = firstNewMoon (1,9,2015) = SOME (1333882, (13,9,2015))
val test20_FirstNewMoon3 = firstNewMoon (4,3,2015) = SOME (2053882, (20,3,2015))
val test20_FirstNewMoon4 = firstNewMoon (1,1,2014) = SOME (140823, (1,1,2014))
val test20_FirstNewMoon5 = firstNewMoon (1,12,2005) = SOME (123882, (1,12,2005))
val test20_FirstNewMoon6 = firstNewMoon (1,8,2008) = SOME (153882, (1,8,2008))
val test20_FirstNewMoon7 = firstNewMoon (1,4,2003) = SOME (190823, (1,4,2003))
val test20_FirstNewMoon8 = firstNewMoon (1,5,2003) = SOME (130823, (1,5,2003))
val test20_FirstNewMoon9 = firstNewMoon (1,2,2003) = SOME (183882, (1,2,2003))
val test20_FirstNewMoon10 = firstNewMoon (1,7,2000) = SOME (163882, (1,7,2000))
val test20_FirstNewMoon11 = firstNewMoon (1,12,2020) = SOME (1503882, (15,12,2020))
val test20_FirstNewMoon12 = firstNewMoon (28,2,1938) = NONE
val test20_FirstNewMoon13 = firstNewMoon (28,2,6310) = SOME (2690823,(26,2,6310))
(******************************************************************************)

(****************************************************************************** 
  Задание 21 winterSolstice
 ******************************************************************************)
val test21_WinterSolstice3 = winterSolstice 2014 = (22, 12, 2014)
val test21_WinterSolstice4 = winterSolstice 2021 = (21, 12, 2021)
val test21_WinterSolstice5 = winterSolstice 2015 = (22, 12, 2015)
val test21_WinterSolstice6 = winterSolstice 2023 = (22, 12, 2023)
(******************************************************************************)

(****************************************************************************** 
  Задание 22 chineseNewYearDate
 ******************************************************************************)
val test22_ChineseNewYearDate1 = chineseNewYearDate 2021 = (12,2,2021)
val test22_ChineseNewYearDate2 = chineseNewYearDate 2020 = (25,1,2020)
val test22_ChineseNewYearDate3 = chineseNewYearDate 2022 = (1,2,2022)
val test22_ChineseNewYearDate4 = chineseNewYearDate 3742 = (18,2,3742)
val test22_ChineseNewYearDate5 = chineseNewYearDate 1925 = (24,1,1925)
val test22_ChineseNewYearDate7 = chineseNewYearDate 2039 = (24,1,2039)
val test22_ChineseNewYearDate8 = chineseNewYearDate 6310 = (28,1,6310) 
(******************************************************************************)

(****************************************************************************** 
  Задание 23 getNthString
 ******************************************************************************)
val test23_GetNthString1 = 
  getNthString (2, ["hi", "there", "how", "are", "you"]) = "how"
val test23_GetNthString2 = 
  getNthString (4, ["hi", "there", "how", "are", "you"]) = "you"
val test23_GetNthString3 = 
  getNthString (0, ["hi", "there", "how", "are", "you"]) = "hi"
(******************************************************************************)

(****************************************************************************** 
  Задание 24 dateToString
 ******************************************************************************)
val test24_DateToString1  = dateToString (5,5,1980) = "May 5, 1980"
val test24_DateToString2  = dateToString (1,9,2016) = "September 1, 2016"
val test24_DateToString3  = dateToString (28,9,2026) = "September 28, 2026"
(******************************************************************************)

(****************************************************************************** 
  Задание 25 chineseYear
 ******************************************************************************)
val test25_ChineseYear1 = 
  chineseYear 1980 = ("Geng-Shen","White","Monkey","Metal")
val test25_ChineseYear2 = 
  chineseYear 2021 = ("Xin-Chou","White","Cow","Wrought metal")
(******************************************************************************)

(****************************************************************************** 
  Задание 26 dateToChineseYear
 ******************************************************************************)
val test26_DateToChineseYear1 = 
  dateToChineseYear (1,9,1980) = ("Geng-Shen","White","Monkey","Metal")
val test26_DateToChineseYear2 = 
  dateToChineseYear (1,9,2021) = ("Xin-Chou","White","Cow","Wrought metal")
val test26_DateToChineseYear3 =
  dateToChineseYear (22, 1, 201) = ("Xin-Si","White","Snake","Wrought metal")
(******************************************************************************)

(****************************************************************************** 
  Задание 27 dateToAnimal
 ******************************************************************************)
val test27_DateToAnimal1 = dateToAnimal (1,9,1980) = "Monkey"
val test27_DateToAnimal2 = dateToAnimal (1,9,2021) = "Cow"
val test27_DateToAnimal3 = dateToAnimal (1,9,2003) = "Sheep"
val test27_DateToAnimal4 = dateToAnimal (1,9,2002) = "Horse"
val test27_DateToAnimal5 = dateToAnimal (22, 1, 201) = "Snake"
(******************************************************************************)

(****************************************************************************** 
  Задание 28 animal
 ******************************************************************************)
val test28_Animal1 = animal ("Ivan", (1,9,1980)) = "Monkey"
val test28_Animal2 = animal ("Svetlana", (1,9,2021)) = "Cow"
val test28_Animal3 = animal ("Lena", (1,9,1984)) = "Rat"
val test28_Animal4 = animal ("Andrew", (1,9,1953)) = "Snake"
val test28_Animal5 = animal ("Svetlana", (22, 1, 201)) = "Snake"
(******************************************************************************)

(****************************************************************************** 
  Задание 29 extractAnimal
 ******************************************************************************)
 val test29_ExtractAnimal1 = 
  extractAnimal ( [ ("Ivan", (1,9,1980))
                  , ("Svetlana", (1,9,2015))
                  ]
                , "Monkey" ) 
  = [("Ivan", (1, 9, 1980))]
val test29_ExtractAnimal2 = 
  let val tmp = extractAnimal ( [ ("Ivan", (1,9,1980))
                                , ("Svetlana", (1,9,2015))
                                , ("Alex", (1,9,1955)) 
                                ]
                              , "Sheep" )
  in
    sameList (tmp, [("Alex", (1,9,1955)), ("Svetlana", (1,9,2015))])
  end 
val test29_ExtractAnimal3 = 
  extractAnimal ( [ ("Ivan", (1,9,1980))
                  , ("Svetlana", (1,9,2015))
                  , ("Lena", (1,9,1984))
                  ]
                , "Rat" ) 
  = [("Lena", (1,9,1984))]
val test29_ExtractAnimal4 = 
  let val tmp = extractAnimal ( [ ("Ivan", (1,9,1980))
                                , ("Svetlana", (1,9,2015))
                                , ("Alex", (1,9,1955)) 
                                , ("Lena", (1,9,1984))
                                , ("Tolik", (17,5,2010))
                                , ("Jack", (17,3,2022))
                                ]
                              , "Tiger" )
  in
    sameList (tmp, [("Tolik", (17,5,2010)), ("Jack", (17,3,2022))])
  end 

val test29_ExtractAnimal5 = 
  extractAnimal ( [ ("Ivan", (31, 1, 211))
                , ("Svetlana", (1, 9, 2015))
                , ("Alex", (1, 9, 1996))
                , ("Anton", (31, 8, 1995))
                ]
                , "Rabbit")
  = [("Ivan",(31,1,211))] 
(******************************************************************************)

(****************************************************************************** 
  Задание 30 extractAnimals
 ******************************************************************************)
val test30_ExtractAnimals1 = 
  extractAnimals ( [ ("Ivan", (1,9,1980))
                   , ("Svetlana", (1,9,2015)) 
                   ]
                 , ["Monkey"] ) 
  = [("Ivan", (1,9,1980))]
val test30_ExtractAnimals2 = 
  let val tmp = extractAnimals ( [ ("Ivan", (1,9,1980))
                                 , ("Svetlana", (1,9,2015)) 
                                 , ("Alex", (1,9,1955)) 
                                 ]
                               , ["Monkey", "Sheep"] )
  in
    sameList ( tmp
             , [ ("Svetlana", (1,9,2015))
               , ("Ivan", (1,9,1980))
               , ("Alex", (1,9,1955))
               ]
             )
  end

val test30_ExtractAnimals3 = 
  extractAnimals ([("Ivan", (31, 1, 211)), ("Svetlana", (1, 9, 2015))
                  , ("Alex", (1, 9, 1996)), ("Anton", (31, 8, 1995))]
                  , ["Rabbit"])
  = [("Ivan",(31,1,211))] 
(******************************************************************************)

(****************************************************************************** 
  Задание 31 youngestFromAnimals
 ******************************************************************************)
val test31_YoungestFromAnimals1 = 
  youngestFromAnimals ( [ ("Ivan", (1,9,1980))
                        , ("Svetlana", (1,9,2015)) 
                        , ("Alex", (1,9,1955)) 
                        ]
                      , ["Monkey", "Sheep"] ) 
  = SOME ("Svetlana", (1,9,2015))

val test31_YoungestFromAnimals2 = 
  youngestFromAnimals ( [ ("Ivan", (31, 1, 211))
                          , ("Alexei", (3, 10, 2001))
                          , ("Victor", (3, 10, 2001))]
                          , ["Rabbit"])
  = SOME ("Ivan",(31,1,211))
(******************************************************************************)

(****************************************************************************** 
  Задание 32 oldStyleStudents
 ******************************************************************************)
val test32_oldStyleStudents1 = 
  let 
    val tmp = oldStyleStudents [ ("Pushkin Alexander", (26,5,1799))
                               , ("Tolstoy Lev", (28,8,1828))
                               , ("Piotr I", (30,5,1672))
                               , ("Ivan Grozniy", (25,8,1530))
                               ]
  in
    sameList ( tmp 
             , [ ("Pushkin Alexander",(6,6,1799))
               , ("Piotr I",(9,6,1672))
               , ("Tolstoy Lev",(9,9,1828))
               , ("Ivan Grozniy",(4,9,1530)) ]
             )
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 33 youngestFromOldStyleAnimals
 ******************************************************************************)
val test33_youngestFromOldStyleAnimals1 = 
  youngestFromOldStyleAnimals ( [ ("Pushkin Alexander", (26,5,1799))
                                , ("Tolstoy Lev", (28,8,1828))
                                , ("Piotr I", (30,5,1672))
                                , ("Ivan Grozniy", (25,8,1530))
                                ]
                              , ["Rat", "Tiger"]
                              )
  = SOME ("Tolstoy Lev",(28,8,1828))
val test33_youngestFromOldStyleAnimals2 = 
  youngestFromOldStyleAnimals ( []
                              , []
                              )
  = NONE
val test33_youngestFromOldStyleAnimals3 = 
  youngestFromOldStyleAnimals ( [ ("Ivan", (31, 1, 211))
                                , ("Alexei", (3, 10, 2001))
                                , ("Victor", (3, 10, 2001))]
                                , ["Rabbit"])
  = SOME ("Ivan",(31,1,211))
(******************************************************************************)

(****************************************************************************** 
  Задание 34 listOfStringDates
 ******************************************************************************)
val test34_listOfStringDates1 = 
  let 
    val tmp = listOfStringDates [ ("Pushkin Alexander", (26,5,1799))
                                , ("Tolstoy Lev", (28,8,1828))
                                , ("Piotr I", (30,5,1672))
                                , ("Ivan Grozniy", (25,8,1530))
                                ]
  in
    sameList ( tmp 
             , [ ("Piotr I","May 30, 1672")
               , ("Tolstoy Lev","August 28, 1828")
               , ("Pushkin Alexander","May 26, 1799")
               , ("Ivan Grozniy","August 25, 1530") ] 
             )
  end
(******************************************************************************)

(****************************************************************************** 
  Задание 35 oldStyleStudentStringDates
 ******************************************************************************)
val test35_oldStyleStudentStringDates1 = 
  let 
    val tmp = oldStyleStudentStringDates [ ("Pushkin Alexander", (26,5,1799))
                                         , ("Tolstoy Lev", (28,8,1828))
                                         , ("Piotr I", (30,5,1672))
                                         , ("Ivan Grozniy", (25,8,1530))
                                         ]
  in
    sameList ( tmp 
             , [ ("Pushkin Alexander","June 6, 1799")
               , ("Tolstoy Lev","September 9, 1828")
               , ("Piotr I","June 9, 1672")
               , ("Ivan Grozniy","September 4, 1530") ]
             )
  end 
(******************************************************************************)

