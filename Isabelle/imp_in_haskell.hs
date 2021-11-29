type Vname = String 
data Aexp = N Int | V Vname | Plus Aexp Aexp

type Val = Int 
type State = Vname -> Val

aval :: Aexp -> State -> Val
aval (N n) s = n
aval (V x) s = s x
aval (Plus a1 a2) s = aval a1 s + aval a2 s

main = do
    print $ aval (Plus (N 3) (V "x")) (\x -> 4)
