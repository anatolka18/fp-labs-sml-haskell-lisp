-- Определение типа данных Vector и его конструктора
data Vector a = Vec3 (a, a, a)

-- Селекторы для компонент вектора
xCoor v = x
  where Vec3 (x, _, _) = v
yCoor v = y
  where Vec3 (_, y, _) = v
zCoor v = z
  where Vec3 (_, _, z) = v

-- Экземпляр класса Show для типа Vector
instance Show a => Show (Vector a) where
    show v = "(" ++ show (xCoor v) 
                 ++ ", " 
                 ++ show (yCoor v) 
                 ++ ", " 
                 ++ show (zCoor v) 
                 ++ ")"

-- Экземпляр класса Eq для типа Vector
instance Eq a => Eq (Vector a) where
    (==) v1 v2 = (xCoor v1 == xCoor v2)
                 && (yCoor v1 == yCoor v2)
                 && (zCoor v1 == zCoor v2)

-- Экземпляр класса Num для типа Vector
instance (Num a, Floating a) => Num (Vector a) where
    (+) v1 v2 = Vec3 ( xCoor v1 + xCoor v2
                     , yCoor v1 + yCoor v2
                     , zCoor v1 + zCoor v2 )
    (*) v1 v2 = Vec3 ( y1 * z2 - y2 * z1
                     , z1 * x2 - z2 * x1
                     , x1 * y2 - x2 * y1 )
          where x1 = xCoor v1
                x2 = xCoor v2
                y1 = yCoor v1
                y2 = yCoor v2
                z1 = zCoor v1
                z2 = zCoor v2
    negate v = Vec3 (- xCoor v, - yCoor v, - zCoor v)
    abs v = Vec3 (sqrt (x * x + y * y + z * z), 0, 0) 
          where x = xCoor v
                y = yCoor v
                z = zCoor v
    signum v = Vec3 (x / absV, y / absV, z / absV)
             where absV = xCoor (abs v)
                   x = xCoor v
                   y = yCoor v
                   z = zCoor v
    fromInteger n = Vec3 (n1, 0, 0)
      where n1 = fromInteger n

-- Экземпляр класса Fractional для типа Vector
instance (Fractional a, Floating a) => Fractional (Vector a) where
    (/) v1 v2 = Vec3 ( ratio * xCoor v
                     , ratio * yCoor v
                     , ratio * zCoor v )
          where v = v2 * v1
                ratio = xCoor (abs v1) / (xCoor (abs v2) * xCoor (abs v))
    fromRational n = Vec3 (n1, 0, 0)
      where n1 = fromRational n

-- Экземпляр класса Floating для типа Vector
instance Floating a => Floating (Vector a) where
    sqrt v = Vec3 (sqrt (xCoor v), sqrt (yCoor v), sqrt (zCoor v))

-------------------------------------------------------------------------------
--                                  ПРИМЕРЫ
-------------------------------------------------------------------------------
main = do
  -- предопределяем три вектора
  let v1 = Vec3 (1.0, 2.5 ,4.2)
  let v2 = Vec3 (5.1, -4.4, 1.8)
  let v3 = Vec3 (1.6, 0.0, -0.5)
  -- вывод значений 
  print $ v1
  print $ xCoor v1
  print $ yCoor v2
  print $ zCoor v3
  -- проверка раюоты функции (==)
  print $ (==) v1 v2
  print $ (==) v2 v3
  -- арифметические операции над векторами
  print $ v1 + v2
  print $ v3 - v2
  print $ v3 * v1
  print $ v2 / v1
  print $ negate v1
  print $ abs v2
  print $ signum v3
  -- смешанные арифметические операции, подключающие fromInteger
  print $ 3 * v2
  print $ v1 / 2
  print $ 3 / v1