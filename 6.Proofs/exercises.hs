import Data.Char

{-
    Foldr implementation
    (foldr1 already exists. It's foldr but restricted to non empty lists)

    foldr f e [x, y, z] = f x (f y (f z e))
-}

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f e []   = e
foldr2 f e (x:xs) = f x (foldr2 f e xs)

{- Fusion Law: f . foldr g a = foldr h b for some h and b 
   Actually b = f a
   and f (g x y) = h x (f y)
-}

minimum, maximum :: Ord a => [a] -> a
minimum = foldr1 min
maximum = foldr1 max

{- 
    Foldl implementation
    foldl f e [x, y, z] = f (f (f e x) y) z
-}
foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f e [] = e
foldl2 f e (x:xs) = foldl2 f (f e x) xs

parts :: String -> (Integer, Float)
parts ds = (intPart es, floatPart fs)
    where (es, (dot:fs)) = break (== '.') ds


intPart :: String -> Integer
intPart = foldl shiftl 0 . map toDigit
    where shiftl n d = n * 10 + d

floatPart :: String -> Float
floatPart = foldr shiftr 0 . map toDigit
    where shiftr d x = (d + x) / 10

toDigit :: Num b => Char -> b
toDigit x = fromIntegral (fromEnum x - fromEnum '0')