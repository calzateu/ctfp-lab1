module Main where
import Test.QuickCheck

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

------------------- Pair instance of Bifunctor -------------------
instance Bifunctor (,) where
    bimap f g (x,y) = (f x,g y)

prop_assoc_b1 :: (Int,Int) -> Fun Int Int -> Fun Int Int -> Fun Int Int
                 -> Fun Int Int -> Bool
prop_assoc_b1 (a,b) (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
  (bimap (f1.f2) (g1.g2)) (a,b) == (bimap f1 g1) (bimap f2 g2 (a,b))

prop_id_b1 :: (Int,Int) -> Bool
prop_id_b1 (a,b) = bimap id id (a,b) == id (a,b)


------------------- Pair instance of Bifunctor -------------------
instance Bifunctor ((,,) x1) where
    bimap f g (x1,a,b) = (x1,f a,g b)

prop_assoc_b2 :: (Int,Int,Int) -> Fun Int Int -> Fun Int Int
                 -> Fun Int Int -> Fun Int Int -> Bool
prop_assoc_b2 (x1,a,b) (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
  (bimap (f1.f2) (g1.g2)) (x1,a,b) == (bimap f1 g1) (bimap f2 g2 (x1,a,b))

prop_id_b2 :: (Int,Int,Int) -> Bool
prop_id_b2 (x1,a,b) = bimap id id (x1,a,b) == id (x1,a,b)


------------------- Either instance of Bifunctor -------------------
instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ g (Right y) = Right (g y)

prop_assoc_b3 :: Either Int Int -> Fun Int Int -> Fun Int Int
                 -> Fun Int Int -> Fun Int Int -> Bool
prop_assoc_b3 x (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
  (bimap (f1.f2) (g1.g2)) x == (bimap f1 g1) (bimap f2 g2 x)

prop_id_b3 :: Either Int Int  -> Bool
prop_id_b3 x  = bimap id id x  == id x




main :: IO ()
main = do
  print "Hola"
  quickCheck prop_assoc_b1
  quickCheck prop_id_b1
  quickCheck prop_assoc_b2
  quickCheck prop_id_b2
  quickCheck prop_assoc_b3
  quickCheck prop_id_b3

