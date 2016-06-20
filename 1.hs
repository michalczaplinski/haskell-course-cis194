toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOther (init (init x)) ++ [(last (init x)) * 2] ++ [last x]
