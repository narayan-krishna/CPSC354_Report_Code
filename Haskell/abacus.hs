type Row = Int  -- types/type synonyms
data Abacus = Abacus Row Row Row deriving Show
                -- deriving show because we need to be able
                -- to print abacus data

-- declarative programming and pattern matching
-- thinking about an abacus physically... what can we do but continuously
-- shift beads to the right

shiftRight :: Abacus -> Int -> Abacus 
shiftRight (Abacus r0 r1 r2) 0 -- at first, we shift the bottom row
    | r0 + 1 < 10 = 
            Abacus (r0 + 1) r1 r2
    | otherwise = shiftRight(Abacus 0 r1 r2) 1 -- but what if its full?

shiftRight (Abacus r0 r1 r2) 1  -- shift the next row
    | r1 + 1 < 10 = 
            Abacus r0 (r1 + 1) r2 -- what if its full?
    | otherwise = shiftRight(Abacus r0 0 r2) 2 -- shift the row after that
    
shiftRight (Abacus r0 r1 r2) 2
    | r2 + 1 < 10 = 
            Abacus r0 r1 (r2 + 1) -- what if its full?
    | otherwise = Abacus 10 10 10 -- we simply stay at our complete abacus

addAbacus :: Abacus -> Int -> Abacus
addAbacus (Abacus r0 r1 r2) x
    | x == 0 = Abacus r0 r1 r2
    | otherwise = addAbacus (shiftRight (Abacus r0 r1 r2) 0) (x - 1)

abacusToInt :: Abacus -> Int
abacusToInt (Abacus r0 r1 r2) = r0 + r1*10 + r2*100

main = do
    print $ abacusToInt (addAbacus (addAbacus (Abacus 0 0 0) 138) 229)
    print $ abacusToInt (addAbacus (addAbacus (Abacus 0 0 0) 133) 314)
    print $  addAbacus (addAbacus (Abacus 0 0 0) 138) 229
