module Theory where

data Booly = False' | True' deriving (Eq, Show)
data Optional a = Nada | Only a deriving (Eq, Show)
newtype Listy a = Listy [a] deriving (Eq, Show)
newtype ZipListy a = ZipListy [a] deriving (Eq, Show)
data Validation a b = Failure a | Success b deriving (Eq, Show)


-- Section 01: Semigroup and Monoid

-- Exercise 1: Implement Semigroup and Monoid instance for the following types:
-- 1. Booly
-- 2. Optional
-- 3. Listy

-- Semigroup declaration for Booly
instance Semigroup Booly where
    (<>) True' True' = True'
    (<>) a b = False'

-- Monoid instance declaration for Booly
instance Monoid Booly where
    mempty = undefined

-- Semigroup declaration for Optional
instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada _ = Nada
    (<>) _ Nada = Nada
    (<>) (Only a) (Only b) = Only (a <> b)

-- Monoid declaration for Optional
instance Monoid a => Monoid (Optional a) where
    mempty = undefined

-- Semigroup declaration for Listy
instance Semigroup (Listy a) where
    (<>) (Listy a) (Listy b) =  Listy (a ++ b)

-- Monoid declaration for Listy
instance Monoid (Listy a) where
    mempty = undefined

-- Semigroup declaration for Validation
instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
    Failure _ <> b = b
    a  <> _ = a

-- Exercise 2: Implement Functor instance for the following types

-- Functor declaration for Optional
instance Functor Optional where
    fmap f Nada = Nada
    fmap f (Only a) = Only (f a)

-- Functor declaration for Validation
instance Functor (Validation a) where
    fmap f (Success x) = Success (f x)  
    fmap f (Failure x) = Failure x  

instance Functor Listy where
    fmap f (Listy a) = Listy (foldl (\acc x -> acc ++ [(f x)]) [] a)

instance Functor ZipListy where
    fmap f (ZipListy a) = ZipListy (foldl (\acc x -> acc ++ [(f x)]) [] a)

-- Exercise 3: Implement Applicative instances for the following types

instance Applicative Optional where
    pure = Only
    Nada <*> _ = Nada
    (Only f) <*> something = fmap f something


instance Applicative Listy where
    pure = undefined
    (<*>) = undefined


instance Applicative ZipListy where
    pure = undefined
    (<*>) = undefined

