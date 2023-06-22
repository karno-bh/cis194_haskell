toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)
 
toDigits = reverse . toDigitsRev
  

doubleSecond xs = doubleSecH xs False
    where doubleSecH [] _ = []
          doubleSecH (y:ys) False = y : doubleSecH ys True
          doubleSecH (y:ys) True  = y * 2 : doubleSecH ys False

doubleEveryOther = reverse. doubleSecond . reverse

sumdigits []  = 0

sumdigits (x:xs)
  | x > 9     = sumdigits ((toDigits x) ++ xs)
  | otherwise = x + (sumdigits xs)

sumCardDigits = sumdigits . doubleEveryOther . toDigits

validate n = mod (sumCardDigits n) 10 == 0
          

main :: IO ()
main =  do
    putStrLn "doubleSecond of [1,2,3,4,5]"
    print $ doubleSecond [1,2,3,4,5]
    print $ toDigitsRev 1234
    print $ toDigitsRev (-1)
    print $ toDigitsRev 0
    print $ toDigits 1234
    print $ toDigits (-1)
    print $ toDigits 0
    print (doubleEveryOther [8,7,6,5])
    print (doubleEveryOther [1,4,3])
    print (sumCardDigits 4012888888881881)
    print (sumCardDigits 4012888888881882)
    print (validate 4012888888881881)
    print (validate 4012888888881882)

