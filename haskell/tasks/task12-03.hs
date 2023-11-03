f12 a = map head (iterate removeDig [a ..])
          where
            -- Функция для удаления чисел, содержащих цифры предыдущих чисел
            removeDig (x : xs) = filter (not . containsDig x) xs
            -- Функция для проверки, содержит ли число цифры другого числа
            containsDig x y = 
              if y == 0 then False
              else if containsDigHelp x y then True
                   else containsDig x (y `div` 10)
            containsDigHelp x y =
              if x == 0 then y `mod` 10 == 0
              else y `mod` (countDig x 1) == x
            -- Функция для подсчета количества разрядов в числе
            countDig x k = 
              if x /= 0 then countDig (x `div` 10) (k * 10)
              else k

main = do
  print $ take 15 $ f12 5
  print $ take 50 $ f12 14
  print $ take 80 $ f12 29
