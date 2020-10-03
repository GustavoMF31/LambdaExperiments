data NonZeroNat = One
                | Succ NonZeroNat
                deriving Show

data Nat = OneLessThan NonZeroNat
           deriving Show

data NInteger = Negative NonZeroNat
             | IntegerZero
             | Positive NonZeroNat
             deriving Show

nzNatAdd :: NonZeroNat -> NonZeroNat -> NonZeroNat
nzNatAdd x One = Succ x
nzNatAdd x (Succ y) = Succ $ nzNatAdd x y

intAdd :: NInteger -> NInteger -> NInteger
intAdd IntegerZero y = y
intAdd x IntegerZero = x
intAdd (Positive m) (Positive n) = Positive (nzNatAdd m n)
intAdd (Negative m) (Negative n) = Negative (nzNatAdd m n)
intAdd (Negative m) (Positive n) = nzNatSub n m
intAdd (Positive m) (Negative n) = nzNatSub m n

nzNatSub :: NonZeroNat -> NonZeroNat -> NInteger
nzNatSub One One = IntegerZero
nzNatSub One (Succ x) = Negative x
nzNatSub (Succ x) One = Positive x
nzNatSub (Succ x) (Succ y) = nzNatSub x y

-- Where and how many of it there are
data FractionPrimeFactorPlace = Numerator NonZeroNat
                              | Denominator NonZeroNat
                              | Nowhere

-- The prime at index 0 is 2
newtype Prime = PrimeAtIndex Nat
data Fraction = PrimeFactorPlacer (Prime -> FractionPrimeFactorPlace)
data SignedFraction = PositiveFrac Fraction
                    | NegativeFrac Fraction

oneHalf :: SignedFraction
oneHalf = PositiveFrac $ PrimeFactorPlacer oneHalf'

oneHalf' :: Prime -> FractionPrimeFactorPlace
oneHalf' (PrimeAtIndex (OneLessThan One)) = Denominator One
oneHalf' _ = Nowhere

addFrac :: Fraction -> Fraction -> Fraction
addFrac x y = _todo
