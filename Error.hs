
data EitherError a b = EeLeft [a] | EeRight b
                        deriving (Show)

instance Functor (EitherError a) where
   fmap _ (EeLeft x) = EeLeft x
   fmap f (EeRight x) = EeRight $ f x

instance Applicative (EitherError a) where
   pure = EeRight
   (EeLeft x) <*> (EeLeft y) = EeLeft $ x ++ y
   (EeLeft x) <*> _ = EeLeft x
   _ <*> (EeLeft x) = EeLeft x
   (EeRight f) <*> something = fmap f something

{-
instance Monoid (EitherError a b) where
   mempty = EeLeft []
   mappend (EeLeft x) <*> (EeLeft y) = EeLeft $ x ++ y
   mappend (EeLeft x) <*> _ = EeLeft x
   mappend _ <*> (EeLeft x) = EeLeft x
-}

--this probably shouldn't be a monad
instance Monad (EitherError a) where
   return x = EeRight x
   (EeLeft x) >>= f = EeLeft x
   (EeRight x) >>= f = f x
   --fail msg = EeLeft [msg]


-- # tests

x1 = fmap (+3) $ EeRight 5
x2 = fmap (+3) $ EeLeft []

x3 = (+) <$> EeLeft ["test"] <*> EeLeft ["yo"]
x4 = (+) <$> EeRight 5 <*> pure 1

--for x5-x8

greaterThan5 :: Int -> EitherError String Int
greaterThan5 x = if x > 5 then EeRight x else EeLeft ["not greater than 5"]

lessThan10 :: Int -> EitherError String Int
lessThan10 x = if x < 10 then EeRight x else EeLeft ["not less than 10"]

addThree a b c = a+b+c

--x5 (EeRight 29):
x5 = addThree <$> greaterThan5 15 <*> lessThan10 9 <*> pure 5
--x6 (EeLeft ["not greater than 5", "not less than 10"]):
x6 = addThree <$> greaterThan5 4 <*> lessThan10 11 <*> pure 5
--x7 (EeLeft ["not less than 10"]):
x7 = addThree <$> greaterThan5 15 <*> lessThan10 11 <*> pure 5
--x8 (EeLeft ["not greater than 5"]):
x8 = addThree <$> greaterThan5 4 <*> lessThan10 9 <*> pure 5




