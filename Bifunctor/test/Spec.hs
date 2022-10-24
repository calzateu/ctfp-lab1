
import Prelude hiding (Functor, fmap)
import Test.QuickCheck

import Lab1
import Test.QuickCheck


class Functor f where
  fmap :: (a -> b) -> f a -> f b

--instance Functor IO where
--  fmap h f = f >>= (pure . h)

instance Functor ((->) e) where
  fmap = (.)

--instance Show (a -> b) where
--  show a= "function"


sumaDos :: Int -> Int
sumaDos a = a + 2


checkEqualityId :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
checkEqualityId f g b = f b == g b

prop_FunctorId :: (Eq b) => (a -> b) -> a -> Bool
prop_FunctorId g val = checkEqualityId (fmap id g) g val



main :: IO ()
main = do
  (quickCheckWith stdArgs {maxSuccess = 100000}) $ prop_FunctorId sumaDos
  --print $ prop_FunctorId sumaDos 5

