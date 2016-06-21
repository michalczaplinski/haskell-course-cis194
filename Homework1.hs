toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ map (sum . toDigits) x

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0



main :: IO()
main = do
  print (toDigits 12345)
  print (doubleEveryOther [12345])
  print (sumDigits [1,23, 4, 5])
  print (validate 4012888888881881)
  print (validate 4012888888881882)
