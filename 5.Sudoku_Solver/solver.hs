type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . many prune . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
          where 
            choice d = if blank d then digits else [d]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp xss


valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxes g) 

nodups :: (Eq a) => [a] -> Bool
nodups []     = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> Matrix a
boxes = map concat . concat .
        map cols .
        group . map group

group [] = []
group xs = take 3 xs : group (drop 3 xs)

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxes . pruneBy cols . pruneBy rows

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where
    fixed = [d | [d] <- row]

pruneBy f = f . map pruneRow . f

remove :: [Digit] -> [Digit] -> [Digit]
remove _ [x] = [x]
remove ds xs = filter (`notElem` ds) xs

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
  where y = f x
