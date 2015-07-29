import Data.Array import Data.Char import Data.List import Data.Maybe import Data.Numbers.Fibonacci
import Data.Numbers.Primes
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Math.Combinatorics.Exact.Binomial
import Numeric
import qualified Data.Map as Map
import qualified Data.Set as Set

import PEHelpers

problem001 :: Int
problem001 = sum $ union [3,6..999] [5,10..999]

problem002 :: Int
problem002 = sum . takeWhile (< 4000000) . filter even . map fib $ [1..]

problem003 :: Int
problem003 = last $ primeFactors 600851475143

problem004 :: Int
problem004 = maximum [n*m | n <- [100..999], m <- [100..n], isPalindrome (n*m)]

problem005 :: Int
problem005 = foldl (\acc (p,n) -> p^n*acc) 1 . Map.toList
    . foldl1 (Map.unionWith max) . map (countAll . primeFactors) $ [2..20]

problem005' :: Int
problem005' = foldr1 lcm [1..20]

problem006 :: Int
problem006 = (sum [1..100])^2 - (sum $ map (^2) [1..100])

problem007 :: Int
problem007 = primes !! 10000

problem008 :: Int
problem008 = maximum $ map product $ grpsOf 13 data008
    where grpsOf n xs
            | n > (length xs) = []
            | otherwise = map digitToInt (take n xs) : (grpsOf n $ tail xs)

problem008' :: Int
problem008' = maximum . map product . foldr (zipWith (:)) (repeat [])
    . take 13 . tails . map digitToInt $ data008

problem009 :: Int
problem009 = head [a*b*c | b <- [2..499], a <- [1..b - 1],
                    let c = 1000 - b - a, a^2 + b^2 == c^2]

problem010 :: Int
problem010 = sum . takeWhile (< 2000000) $ primes

problem011 :: Int
problem011 = maximum . map (\q -> product . map (\i -> nums ! i) $ q) $ quads
    where nums =
            listArray ((0, 0), (19, 19)) . map read . words $ data011
          quads = [[(i,j),(i,j+1),(i,j+2),(i,j+3)] | i <- [0..19],
                    j <- [0..16]]
               ++ [[(i,j),(i+1,j),(i+2,j),(i+3,j)] | i <- [0..16],
                    j <- [0..19]]
               ++ [[(i,j),(i+1,j+1),(i+2,j+2),(i+3,j+3)] | i <- [0..16],
                    j <- [0..16]]
               ++ [[(i,j),(i+1,j-1),(i+2,j-2),(i+3,j-3)] | i <- [0..16],
                    j <- [3..19]]

problem012 :: Int
problem012 = head . filter ((> 500) . noOfDivisors) $ triangleNums

problem013 :: Int
problem013 = read . take 10 . show . sum $ data013

problem014 :: Int
problem014 = fst . maximumBy (comparing snd) . assocs . collatzLengths $ 10^6

problem015 :: Int
problem015 = choose 40 20

problem016 :: Int
problem016 = digitSum $ 2^1000

problem017 :: Int
problem017 = sum . map numLetterCount $ [1..1000]

problem018 :: Int
problem018 = maximum .
    foldl1 (\acc xs -> zipWith (+) (pascalRow max 0 acc) xs) $ data018

problem019 :: Int
problem019 = count 7 .
    map (\(y,m) -> let (_,_,d) = toWeekDate $ fromGregorian y m 1 in d)
    $ [(y,m) | y <- [1901..2000], m <- [1..12]]

problem020 :: Int
problem020 = digitSum . fac $ 100

problem021 :: Int
problem021 = sum . filter isAmicable $ [1..9999]

problem022 :: Int
problem022 = sum . zipWith (*) [1..] . map wordScore . sort $ data022
    where wordScore = sum . map charScore
          charScore c = (+) 1 . fromMaybe 0 . elemIndex c $ ['A'..]

problem023 :: Int
problem023 = sum . filter (not . abSum) $ [1..28123]
    where abSum n = any ((!) abArray . (-) n) . takeWhile (< n) $ abundants
          abArray = listArray (1, 28123) .
            map (\n -> elem n $ takeWhile (<= n) abundants) $ [1..]

problem024 :: Int
problem024 = read . lexPerm "0123456789" $ 999999

problem025 :: Int
problem025 = head . filter ((==) 1000 . length . show . fib) $ [0..]

problem026 :: Int
problem026 = fst . maximumBy (comparing snd) . zip [1..] . map recLen $ [1..999]
    where recLen = length . recurringSegment . drop 1 . expandFrac 1

problem027 :: Int
problem027 = fst . maximumBy (comparing snd) .
    map (\(a,b) -> (a*b, matches a b)) $ [(a, b) | a <- [-1000..1000], b <- bs]
        where bs = concatMap (\p -> [p, (-p)]) $ takeWhile (< 1000) primes
              matches a b = length . takeWhile (f a b) $ [0..]
              f a b n = isPrime $ n^2 + a*n + b

problem028 :: Int
problem028 = (16*n^3 + 30*n^2 + 26*n + 3) `div` 3
    where n = 500

problem028' :: Int
problem028' = sum . scanl1 (+) . (1:) . concatMap (replicate 4) $ [2,4..1000]

problem029 :: Int
problem029 = length . nub $ [a^b | a <- [2..100], b <- [2..100]]

problem029' :: Int
problem029' = Set.size . Set.fromList $ [a^b | a <- [2..100], b <- [2..100]]

problem030 :: Int
problem030 = sum
    . filter (\n -> n == (sum . map ((^5) . digitToInt) . show $ n))
    $ [2..6*9^5]

problem031 :: Int
problem031 = coinDecomp 200 200

problem032 :: Int
problem032 = Set.foldr (+) 0 . Set.fromList $ pattern 1 4 ++ pattern 2 3
    where pattern a b =
            [n | as <- concatMap permutations $ chooseFrom ['1'..'9'] a,
                 bs <- concatMap permutations $ chooseFrom (['1'..'9'] \\ as) b,
                 let n = (read as) * (read bs),
                 n < 10000,
                 (length . nub $ '0':as ++ bs ++ show n) == 10]

problem033 :: Int
problem033 = snd . reduce . foldr1 (\(n, d) (na, da) -> (n * na, d * da))
    . filter lucky $ [(n, d) | d <- [11..99] \\ [20,30..90], n <- [10..d-1]]
        where lucky (n, d) = nr < 10 && n * dr == d * nr
                where (nr, dr) = reduceStupid (n, d)

problem034 :: Int
problem034 = sum . filter (\n -> digitFacSum n == n) $ [10..50000]

problem035 :: Int
problem035 = length . filter isCircularPrime . takeWhile (<10^6) $ primes

problem036 :: Int
problem036 = sum . filter (isPalindrome . decToBin) . takeWhile (<10^6)
                $ palindromeInts

problem037 :: Int
problem037 = sum . take 11 . filter (isTruncPrime . show) . drop 4 $ primes
    where isTruncPrime p = isLTruncPrime p && isRTruncPrime p
          isLTruncPrime = and . map (isPrime . read) . init . tails
          isRTruncPrime = and . map (isPrime . read) . tail . inits

--problem038 :: Int
--problem038 =

problem040 :: Int
problem040 = product . map (digitToInt . (!!) champernowne .
                        ((+) (-1)) . (10^)) $ [0..6]

problem067 :: Int
problem067 = maximum .
    foldl1 (\acc xs -> zipWith (+) (pascalRow max 0 acc) xs) $ data067

problem096 :: Int
problem096 = sum . map (top3 . elems . head . solveSudoku . readSudoku) $ data096
    where top3 (h:t:u:_) = 100*h + 10*t + u
