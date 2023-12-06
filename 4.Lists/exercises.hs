import Data.List (sort)
import Data.Char

{--- Triads generator ---}
divisors :: Int -> [Int]
divisors x = [d | d <- [2..x-1], mod x d == 0]

{- Assumes lists are sorted in ascending order -}
disjoint :: (Eq a, Ord a) => [a] -> [a] -> Bool
disjoint _ [] = True
disjoint [] _ = True
disjoint xxs@(x:xs) yys@(y:ys)
    | x == y    = False
    | x > y     = disjoint xs yys
    | otherwise = disjoint xxs ys

coprime :: Int -> Int -> Bool
coprime x y = disjoint (divisors x) (divisors y)

{- If x, y are coprime any multiple p*x, p*y, p*z will be a triad as well -}
triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1..m], y <- [x+1..n], 
                        coprime x y, 
                        z <- [y+1..n], x*x + y*y == z*z]
            where m = floor (fromIntegral n / sqrt 2)

{--- Playing with list functions ---}

nondec :: Ord a => [a] -> Bool
nondec xs = and (zipWith (<=) xs (tail xs))

position :: (Eq a) => a -> [a] -> Int
position x xs
    = head ([j | (j,y) <- zip [0..] xs, y == x] ++ [-1])

{--- Common words ---}
commonWords :: Int -> [Char] -> [Char]
commonWords n = concat . map showRun . take n . reverse . sort .
                countRuns . sort . words . map toLower

showRun :: (Int, [Char]) -> [Char]
showRun (n, w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [[Char]] -> [(Int, [Char])]
countRuns [] = []
countRuns (w:ws) = (1 + length us, w):countRuns vs
                   where (us, vs) = span (==w) ws
