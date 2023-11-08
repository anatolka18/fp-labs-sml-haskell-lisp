import Lab4

-- Первоначальный набор тестов для лабораторной работы №4 
-- по мере реализации решений заданий снимайте комментарий 
-- с тестов соответствующей функции и пополняйте их своими тестами
-- кроме того, снимайте комментарий соответствующих строк вывода 
-- результатов тестов в функции main в конце файла и пополняйте эту 
-- функцию своими строками

-- Для проведения тестов запустите этот файл и выполните запуск main

--------------------------------------------------------------------------------
-- Вспомогательные определения
--------------------------------------------------------------------------------ones = 1 : ones
-- Функция "округления вещественного значения до 8 знаков после запятой
truncTo8 f =  (fromInteger $ truncate $ f * (10 ^ 8)) / (10.0 ^^ 8)

-- Функция сравнения на равенство двух вещественных чисел 
-- до восьмого знака после запятой 
(=!=) a b = a1 <= b1 && b1 <= a1
  where a1 = truncTo8 a
        b1 = truncTo8 b 

-- Функция сравнения двух списков вещественных чисел на равенство, 
-- где числа сравниваются до восьмого знака после запятой 
(=!!=) [] [] = True
(=!!=) (x : xs) (y : ys) = (x =!= y) && (xs =!!= ys)
(=!!=) _ _ = False

-- бесконечная последовательность единиц
ones = 1 : ones

--------------------------------------------------------------------------------
-- Тесты
--------------------------------------------------------------------------------
------  Задание 1 nat 
test_nat_1 = take 5 (drop 24 nat) == [25, 26, 27, 28, 29]
test_nat_2 = take 5 (drop 124 nat) == [125, 126, 127, 128, 129]

------  Задание 2 fibonacci 
test_fibonacci_1 = take 8 fibonacci == [0,1,1,2,3,5,8,13]

------ Задание 3 factorial 
test_factorial_1 = take 5 factorial == [1,1,2,6,24] 
test_factorial_2 = take 1 factorial == [1]

------ Задание 4 hailstone 
test_hailstone_1 = take 5 (hailstone 2) == [2,1,4,2,1]
test_hailstone_2 = take 5 (hailstone 7) == [7,22,11,34,17]

------ Задание 5 hailstoneStepNum 
test_hailstoneStepNum_1 = hailstoneStepNum 2 == 1
test_hailstoneStepNum_2 = hailstoneStepNum 1 == 0
test_hailstoneStepNum_3 = hailstoneStepNum 3 == 7

------ Задание 6 hailstonePeak 
test_hailstonePeak_1 = hailstonePeak 2 == 2
test_hailstonePeak_3 = hailstonePeak 3 == 16

------ Задание 7 powers 
------ для вещественнозначного аргумента тесты не требуются
test_powers_1 = take 5 (powers 1) == [1,1,1,1,1] 
test_powers_2 = head (drop 4 (powers 1.1)) =!= 1.4641
test_powers_3 = take 5 (powers (-1)) == [1,-1,1,-1,1] 

------ Задание 8 findCloseEnough 
test_findCloseEnough_1 = 
  findCloseEnough 2 ([1, 4, 7, 5, 5, 4, 4, 25] ++ ones) == 5
test_findCloseEnough_2 = 
  findCloseEnough 1 ([1, 4, 7, 5, 6, 4, 4, 25] ++ ones) == 6
test_findCloseEnough_3 = 
  findCloseEnough 0 ([1, 4, 7, 5, 6, 4, 4, 25] ++ ones) == 4
------ Задание 9 streamSum 
test_streamSum_1 = 
  take 6 (streamSum ([1, 2, 3, 4, 5] ++ ones)) == [0,1,3,6,10,15]

------ Задание 10 expSummands 
test_expSummands_1 = 
  (take 5 (expSummands 1)) =!!= [1.0,1.0,0.5,1/6,1/24]

------ Задание 11 expStream 
test_expStream_1 = take 7 (expStream 3) =!!= [0.0,1.0,4.0,8.5,13.0,16.375,18.4]

------ Задание 12 expAppr 
test_expAppr_1 = abs (expAppr epsilon'' 1 - exp 1) < epsilon
test_expAppr_2 = abs (expAppr epsilon'' 5 - exp 5) < epsilon

------ Задание 13 derivativeAppr 
test_derivativeAppr_1 = 
  abs (derivativeAppr sin epsilon'' x - cos x) < epsilon
    where x = pi

------ Задание 14 derivativeStream 
test_derivativeStream_1 = 
  take 5 lst == [5.0, 4.5, 4.25, 4.125, 4.0625]
    where 
      lst = map (\f -> f 2.0) (derivativeStream square)
      square x = x * x

------ Задание 15 derivative 
test_derivative_1 = abs (derivative sin x - cos x) < epsilon
  where x = pi
test_derivative_2 = abs (derivative (\y -> y * y) x - 2.0 * x) < epsilon
  where x = 10

------ Задание 16 funAkStream
------ тесты реализуются самостоятельно

------ Задание 17 invF
test_invF_1 = abs (invF (\x -> x * x) 4.3 16 - 4) < epsilon
test_invF_2 = abs (invF (\x -> x * x * x) 4.7 125 - 5) < epsilon

------ Задание 18 average 
test_average_1 = (average 4 8) == 6

------ Задание 19 averageDump 
test_averageDump_1 = (averageDump (\x -> x + x) 8) == 12

------ Задание 20 newtonTransform 
test_newtonTransform_1 = abs (newtonTransform (\x -> x * x / 2) 4 - 2) < epsilon

------ Задание 21 eitken 
------ тесты реализуются самостоятельно

------ Задание 22 fixedPoint
------ тесты реализуются самостоятельно

------ Задание 23 fixedPointOfTransform 
------ тесты реализуются самостоятельно

------ Задание 24 sqrt1 
test_sqrt1_1 = abs (sqrt1 25 - 5) < epsilon

------ Задание 25 cubert1 
test_cubert1_1 = abs (cubert1 125 - 5) < epsilon

------ Задание 26 sqrt2 
test_sqrt2_1 = abs (sqrt2 25 - 5) < epsilon

------ Задание 27 cubert2 
test_cubert2_1 = abs (cubert2 125 - 5) < epsilon

------ Задание 28 extremum 
test_extremum_1 = abs (x - 0) < epsilon && str == "minimum"
  where (x, str) = extremum (\x -> x * x)
test_extremum_2 = abs (x + 0.75) < epsilon && str == "minimum"
  where (x, str) = extremum (\x -> 2 * x * x + 3 * x - 3)
test_extremum_3 = getVerdict res
  where 
    res = extremum (\x -> 2 * x * x * x - 6 * x - 3)
    getVerdict (x, str)
      | abs (x - 1) < epsilon && str == "minimum" = True
      | abs (x + 1) < epsilon && str == "maximum" = True
      | otherwise = False
---- Задание 29 myPi 
test_myPi_1 = abs (myPi - pi) < epsilon 

main = do 
  putStrLn ("1 test_nat_1 " ++ (show test_nat_1))
  putStrLn ("1 test_nat_2 " ++ (show test_nat_2))
  putStrLn ("2 test_fibonacci_1 " ++ (show test_fibonacci_1))
  putStrLn ("3 test_factorial_1 " ++ (show test_factorial_1))
  putStrLn ("3 test_factorial_2 " ++ (show test_factorial_2))
  putStrLn ("4 test_hailstone_1 " ++ (show test_hailstone_1))
  putStrLn ("4 test_hailstone_2 " ++ (show test_hailstone_2))
  putStrLn ("5 test_hailstoneStepNum_1 " ++ (show test_hailstoneStepNum_1))
  putStrLn ("5 test_hailstoneStepNum_2 " ++ (show test_hailstoneStepNum_1))
  putStrLn ("5 test_hailstoneStepNum_3 " ++ (show test_hailstoneStepNum_1))
  putStrLn ("6 test_hailstonePeak_1 " ++ (show test_hailstonePeak_1))
  putStrLn ("6 test_hailstonePeak_3 " ++ (show test_hailstonePeak_3))
  putStrLn ("7 test_powers_1 " ++ (show test_powers_1))
  putStrLn ("7 test_powers_2 " ++ (show test_powers_2))
  putStrLn ("7 test_powers_3 " ++ (show test_powers_3))
  putStrLn ("8 test_findCloseEnough_1 " ++ (show test_findCloseEnough_1))
  putStrLn ("8 test_findCloseEnough_2 " ++ (show test_findCloseEnough_2))
  putStrLn ("8 test_findCloseEnough_3 " ++ (show test_findCloseEnough_3))
  putStrLn ("9 test_streamSum_1 " ++ (show test_streamSum_1))
  putStrLn ("10 test_expSummands_1 " ++ (show test_expSummands_1))
  putStrLn ("11 test_expStream_1 " ++ (show test_expStream_1))
  putStrLn ("12 test_expAppr_1 " ++ (show test_expAppr_1))
  putStrLn ("12 test_expAppr_2 " ++ (show test_expAppr_2))
  putStrLn ("13 test_derivativeAppr_1 " ++ (show test_derivativeAppr_1))
  putStrLn ("14 test_derivativeStream_1 " ++ (show test_derivativeStream_1))
  putStrLn ("15 test_derivative_1 " ++ (show test_derivative_1))
  putStrLn ("15 test_derivative_2 " ++ (show test_derivative_2))
  putStrLn ("17 test_invF_1 " ++ (show test_invF_1))
  putStrLn ("17 test_invF_2 " ++ (show test_invF_2))
  putStrLn ("18 test_average_1 " ++ (show test_average_1))
  putStrLn ("19 test_averageDump_1 " ++ (show test_averageDump_1))
  putStrLn ("20 test_newtonTransform_1 " ++ (show test_newtonTransform_1))
  putStrLn ("24 test_sqrt1_1 " ++ (show test_sqrt1_1))
  putStrLn ("25 test_cubert1_1 " ++ (show test_cubert1_1))
  putStrLn ("26 test_sqrt2_1 " ++ (show test_sqrt2_1))
  putStrLn ("27 test_cubert2_1 " ++ (show test_cubert2_1))
  putStrLn ("28 test_extremum_1 " ++ (show test_extremum_1))
  putStrLn ("28 test_extremum_2 " ++ (show test_extremum_2))
  putStrLn ("28 test_extremum_3 " ++ (show test_extremum_3))
  putStrLn ("29 test_myPi_1 " ++ (show test_myPi_1))
  putStrLn "all tests done"