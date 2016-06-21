type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 2 = [(a, b), (a, c), (b, c)]
  | n > 2 = hanoi (n-1) a b c ++ [(a, b)] ++ 


main :: IO()
main = do
  print (hanoi 2 "a" "b" "c")
  print "done"
