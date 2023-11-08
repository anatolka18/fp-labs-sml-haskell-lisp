
-- Шаблон для выполнения заданий Лабораторной работы №4 

-- ниеже перечисляются имена, доступные после загрузки данного модуля
-- (например в файле с тестами)
-- по мере реализации решений заданий снимайте комментарий 
-- с соответствующей функции
module Lab4 
  ( epsilon
  , epsilon'
  , epsilon''
  , nat
  , fibonacci
  , factorial
  , hailstone
  , hailstoneStepNum
  , hailstonePeak
  , powers
  , findCloseEnough
  , streamSum
  , expSummands
  , expStream
  , expAppr
  , derivativeAppr
  , derivativeStream
  , derivative
  , funAkStream
  , invF
  , average
  , averageDump
  , newtonTransform
  , eitken
  , fixedPoint
  , fixedPointOfTransform
  , sqrt1
  , cubert1
  , sqrt2
  , cubert2
  , extremum
  , myPi
  ) where
--------------------------------------------------------------------------------
-- Вспомогательные определения
--------------------------------------------------------------------------------
import Data.List
epsilon  = 0.001
epsilon' = 0.00001
epsilon'' = 0.00000001
--------------------------------------------------------------------------------

-- Задание 1 nat 
nat = [1 ..]
-- Задание 2 fibonacci 
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
-- Задание 3 factorial 
factorial = 1 : zipWith (*) factorial nat
-- Задание 4 hailstone 
hailstone x = iterate gradin x
  where
    gradin x = if x `mod` 2 == 1
               then 3 * x + 1
               else x `div` 2
-- Задание 5 hailstoneStepNum 
hailstoneStepNum a = res
  where Just res = elemIndex 1 (hailstone a)
-- Задание 6 hailstonePeak 
hailstonePeak a =  maximum (takeWhile (>1) (hailstone a))
-- Задание 7 powers 
powers x = iterate (*x) 1
-- Задание 8 streamSum 
streamSum stream = 0 : zipWith (+) stream (streamSum stream)
-- Задание 9 findCloseEnough 
findCloseEnough eps lst = res
    where
      lstNew = zip lst (tail lst)
      Just (_, res) = find (\(x,y) -> abs (x - y) <= eps) lstNew
--  Задание 10 expSummands 
expSummands x = zipWith (/) (powers x) (map fromIntegral factorial)
--  Задание 11 expStream 
expStream x = streamSum (expSummands x)
--  Задание 12 expAppr 
expAppr eps x = findCloseEnough eps (expStream x)
--  Задание 13 derivativeAppr 
derivativeAppr f dx = \x -> (f (x + dx) - f x) / dx
--  Задание 14 derivativeStream 
derivativeStream f = map (derivativeAppr f) (powers 0.5)
--  Задание 15 derivative 
derivative f =
  \x -> (findCloseEnough epsilon' (map (\f -> f x) (derivativeStream f)))
--  Задание 16 funAkStream 
funAkStream g = iterate (\f x -> derivative f x / g x) (\f -> f)
--  Задание 17 invF 
invF f y0 x = findCloseEnough epsilon (streamSum lst2)
    where
      lst1 = map (\f -> f y0) (funAkStream (derivative f))
      lst2 = zipWith (*) lst1 (expSummands (x - f y0))
--  Задание 18 average 
average x y = (x + y) / 2
--  Задание 19 averageDump 
averageDump f x = average (f x) x
--  Задание 20 newtonTransform 
newtonTransform g x = x - g x / derivative g x
--  Задание 21 eitken 
eitken s = zipWith (/) lst1 lst2
    where
      sn = tail s
      snPlus1 = tail sn
      lst1 = (zipWith (-) (zipWith (*) s snPlus1) (zipWith (*) (sn) (sn)))
      lst2 = (zipWith (-) (zipWith (+) snPlus1 s) (map (*2) sn))
--  Задание 22 fixedPoint 
fixedPoint f x = iterate f x
--  Задание 23 fixedPointOfTransform 
fixedPointOfTransform f g x0 =
  findCloseEnough epsilon' (fixedPoint (g f) x0)
--  Задание 24 sqrt1 
sqrt1 x = fixedPointOfTransform (\y -> x / y) averageDump 1.0
--  Задание 25 cubert1 
cubert1 x = fixedPointOfTransform (\y -> x / (y * y)) averageDump 1.0
--  Задание 26 sqrt2 
sqrt2 x = fixedPointOfTransform (\y -> y * y - x) newtonTransform 1.0
--  Задание 27 cubert2 
cubert2 x = fixedPointOfTransform (\y -> y * y * y - x) newtonTransform 1.0
--  Задание 28 extremum 
extremum f = res
         where
          ex = fixedPointOfTransform f' newtonTransform 1.0
          f'' = derivative f' ex
          f' = derivative f
          res = if f'' < -epsilon then (ex, "maximum")
                else if f'' > epsilon then (ex, "minimum")
                     else (ex, "inflection")
--  Задание 29 myPi 
myPi = findCloseEnough epsilon'' (eitken res) * 4
  where
    res = streamSum (zipWith (/) (powers (-1)) [1, 3 ..])