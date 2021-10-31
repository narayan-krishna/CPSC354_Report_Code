type Row = Int 
data Abacus = Abacus Row Row Row deriving Show

shiftRight :: Abacus -> Int -> Abacus
shiftRight (Abacus r0 r1 r2) 0 
    | r0 + 1 < 10 = 
            Abacus (r0 + 1) r1 r2
    | otherwise = 
        shiftRight(Abacus 0 r1 r2) 1

shiftRight (Abacus r0 r1 r2) 1 
    | r1 + 1 < 10 = 
            Abacus r0 (r1 + 1) r2
    | otherwise = shiftRight(Abacus r0 0 r2) 2
    
shiftRight (Abacus r0 r1 r2) 2
    | r2 + 1 < 10 = 
            Abacus r0 r1 (r2 + 1)
    | otherwise = Abacus 10 10 10

addAbacus :: Abacus -> Int -> Abacus
addAbacus (Abacus r0 r1 r2) x
    | x == 0 = Abacus r0 r1 r2
    | otherwise = addAbacus (shiftRight (Abacus r0 r1 r2) 0) (x - 1)

abacusToInt :: Abacus -> Int
abacusToInt (Abacus r0 r1 r2) = r0 + r1*10 + r2*100

main = do
    print $ abacusToInt (addAbacus (addAbacus (Abacus 0 0 0) 138) 229)
    print $ addAbacus (Abacus 0 0 0) 138
    print $  (addAbacus (addAbacus (Abacus 0 0 0) 138) 229)