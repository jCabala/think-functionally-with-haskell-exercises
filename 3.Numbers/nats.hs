data Nat = Zero | Succ Nat deriving(Eq, Ord, Show)

instance Num Nat where
    m + Zero     = m
    m + Succ n   = Succ (m + n)
    
    m * Zero     = Zero
    m * (Succ n) = m * n + m
    
    m - Zero        = m
    Zero - Succ m   = error "Negative result"
    Succ m - Succ n = m - n

    abs n           = n
    signum Zero     = Zero
    signum (Succ n) = n

    fromInteger x
        | x <= 0    = Zero
        | otherwise = Succ (fromInteger (x - 1))

infinity :: Nat
infinity = Succ infinity

divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat x y
    | y == Zero = error "division by 0"
    | x < y     = (Zero, x)
    | otherwise = (Succ divRes, modRes)
    where
        (divRes, modRes) = divModNat (x - y) y