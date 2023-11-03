f11 x eps n = take (n + 1) lst
  where
    lst = 1.0 : x : elemLst lst 1.0
    elemLst (y0 : ys @ (y1 : _)) m = 
      if abs (y0 - y1) < eps then []
      else (x / m) * y1 * y1 - y0 : elemLst ys (m + 1.0)

main = do
  let y1 = f11 0.2 0.005 500
  putStrLn "f11 0.2 0.005 500"
  print $ y1 !! 10
  print $ y1 !! 100
  print $ y1 !! 500
  let y2 = f11 1.5 0.001 500
  putStrLn "f11 1.5 0.001 500"
  print $ y2 !! 10
  print $ y2 !! 100
  print $ y2 !! 500
  let y3 = f11 2.0 0.001 500
  putStrLn "f11 2.0 0.001 10"
  print $ y3 !! 10
  print $ y3 !! 5
  let y4 = f11 1.2 0.001 500
  putStrLn "f11 1.2 0.001 500"
  print $ y4 !! 10
  print $ y4 !! 100
  print $ y4 !! 500

