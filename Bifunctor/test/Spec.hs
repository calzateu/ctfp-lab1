
import Prelude hiding (Functor, fmap)
import Test.QuickCheck

import Lab1
import Test.QuickCheck

-- data T a = T


-- -- main :: IO ()
-- -- main = putStrLn "Test suite not yet implemented

-- --data Maybe a = Nothing | Just a
-- -- instance Functor Maybe where
-- --   fmap f Nothing = Nothing
-- --   fmap f (Just a) = Just (f a)

-- --prop_FunctorId :: MyFuntor f => f -> f a -> Bool
-- --prop_FunctorID f (f a) = f (id a) == f a

-- sumaDos :: Int -> Int
-- sumaDos a = a + 2



-- instance Functor IO where
--   fmap h f = f >>= (pure . h)

-- instance Functor ((->) e) where
--   fmap = (.)


-- checkEqualityId :: (a -> b) -> (a -> b) -> Bool
-- checkEqualityId f g a = f a == g a

-- prop_FunctorId :: Functor f => T (f a) -> f a -> Bool
-- prop_FunctorId T f = checkEqualityId (fmap id f) f 5








class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor IO where
  fmap h f = f >>= (pure . h)

instance Functor ((->) e) where
  fmap = (.)

-- | Data type is used to fix concrete data in properties
data T a = T

-- | Values to be compared for equality
data Equal a = Equal a a
             | NotE (Equal a)
             | AndE (Equal a) (Equal a)
             | OrE  (Equal a) (Equal a) deriving (Show)

instance Show (a -> b) where
  show a= "function"


-- | Convenience sinonym for 'Equal'. Delay comparison for equality
(.==.) :: a -> a -> Equal a
(.==.) = Equal



sumaDos :: Int -> Int
sumaDos a = a + 2



--checkEqualityId :: (Functor f) => T (f a) -> (f a) -> Int -> Bool
checkEqualityId :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
checkEqualityId f g b = f b == g b

--prop_FunctorId :: Functor f => T (f a) -> f a -> Bool
prop_FunctorId :: (Eq b, Num a) => (a -> b) -> Bool
-- checkEqualityId (fmap id sumaDos ) sumaDos 5
--prop_FunctorId T f = checkEqualityId (fmap id sumaDos ) sumaDos 5
prop_FunctorId g = checkEqualityId (fmap id g) g 5


main :: IO ()
main = do
  --quickCheckWith $ prop_FunctorId sumaDos
  print $ prop_FunctorId sumaDos
  --print $ "Hola"

