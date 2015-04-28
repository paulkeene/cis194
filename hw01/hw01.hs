-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = map f (zip xs [(len - 1), (len - 2)..0])
  where
    len = length xs
    f (x, i) = if i `mod` 2 == 1 then (2 * x) else x


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . (concatMap toDigits)


-- Exercise 4

validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits


-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
