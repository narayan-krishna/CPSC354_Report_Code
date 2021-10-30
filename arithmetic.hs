-- A Virtual Machine (VM) for Arithmetic (template)

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Eq,Show) -- for equality and printing

-- Rational numbers
data QQ =  QQ II PP
  deriving (Eq,Show) -- for equality and printing

------------------------
-- Arithmetic on the  VM
------------------------

----------------
-- NN Arithmetic
----------------

-- add natural numbers
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- multiply natural numbers
multN :: NN -> NN -> NN
multN O m = O
multN (S n) m = addN (multN n m) m

----------------
-- II Arithmetic
----------------

--an integer is a pair (n,m) to represent a value (n-m)
--data II = II NN NN

addI :: II -> II -> II
addI (II a b) (II m n) = II (addN a m) (addN b n)

multI :: II -> II -> II
multI (II a b) (II m n) = II (addN (multN a m) (multN b n)) (addN (multN a n) (multN b m))

subtrI :: II -> II -> II
subtrI (II a b) (II m n) = II (addN a n) (addN b m)

negI :: II -> II
negI (II a b) = II b a

----------------
-- QQ Arithmetic
----------------

addP :: PP -> PP -> PP
addP I m = T m
addP (T m) n = T (addP m n)

multP :: PP -> PP -> PP
multP I m = m
multP (T m) n = addP(multP m n) m

ii_pp :: PP -> II
ii_pp I = II (S O) O
ii_pp (T m) =  addI (ii_pp m) (II (S O) O)

addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ m n) = QQ (addI(multI a (ii_pp n)) (multI m (ii_pp b))) (multP b n)

multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ m n) = QQ (multI a m) (multP b n)

----------------
-- Normalisation
----------------

normalizeI :: II -> II
normalizeI (II m O) = II m O
normalizeI (II O m) = II O m
normalizeI (II (S m) (S n)) = normalizeI (II m n)
----------------------------------II (m - 1) (n - 1)

----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

nn_int :: Integer -> NN
nn_int 0 = O
nn_int x = S (nn_int (x-1))

int_nn :: NN->Integer
int_nn O = 0
int_nn (S m) = (int_nn m) + 1

ii_int :: Integer -> II
-- ii_int 0 = II O O 
ii_int x = II (nn_int x) O


int_ii :: II -> Integer
int_ii (II m O) = int_nn m
int_ii (II m n) = int_ii (normalizeI(II m n))

pp_int :: Integer -> PP
pp_int 1 = I
pp_int x = T(pp_int (x-1))

int_pp :: PP->Integer
int_pp I = 1 
int_pp (T m) = (int_pp m) + 1

float_int :: Integer->Float
float_int n = fromInteger(n)

float_qq :: QQ -> Float
float_qq (QQ m n) = float_int(int_ii m) / float_int(int_pp n)

------------------------------
-- Normalisation by evaluation
------------------------------

nbv :: II -> II
nbv (II m n) = ii_int (int_ii (II m n))

----------
-- Testing
----------
main = do
    print $ addN (S (S O)) (S O)
    print $ multN (S (S O)) (S (S (S O)))
    print "integers"
    print $ addI (II (S O) O) (II O O)
    print $ subtrI (II (S O) O) (II (S (S (S O))) O)
    print "subtr 2"
    -- print $ subtrI (II (S (S (S O))) (S O)) II (S O 
    print $ multI (II (S O) O) (II (S (S (S O))) (S O))
    print "pos integers"
    print $ addP (T I) I
    print $ addP (T (T I)) (T I)
    print $ ii_pp (T I) 
    print "fractions"
    print $ addQ (QQ (II (S O) O) (T I)) (QQ (II (S(S O)) O) I) 
    print $ multQ (QQ (II (S O) O) (T I)) (QQ (II (S(S O)) O) I) 
    print "normalize"
    print $ normalizeI (II (S (S (S O))) (S O))
    print $ normalizeI (II (S(S(S O))) (S(S(S O))))
    print $ nn_int 7
    print $ int_nn (S (S (S O)))
    print $ ii_int 7
    print $ int_ii (II (S (S (S O))) (S O))
    print $ pp_int 8 
    print $ int_pp(addP (T (T I)) (T I))
    print $ float_qq (QQ (II (S O) O) (T I))
    print $ nbv (II (S (S (S O))) (S O))
    print $ nbv (II (S(S(S O))) (S(S(S O))))

