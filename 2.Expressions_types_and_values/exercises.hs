import Data.Char
import Data.String

modernise :: String -> String
modernise = unwords . map capitalise . words
    where capitalise :: String -> String
          capitalise [] = []
          capitalise (c:cs) = [toUpper c] ++ cs

first :: (a -> Bool) -> [a] -> Maybe a
first p = maybeHead . filter p
    where
        maybeHead :: [a] -> Maybe a
        maybeHead []     = Nothing
        maybeHead (x:xs) = Just x

expo :: Integer -> Integer -> Integer
expo x n
    | n == 0    = 1
    | n == 1    = x
    | even n    = let e = expo x (div n 2) in e ^ 2
    | otherwise = x * (expo x (n - 1))