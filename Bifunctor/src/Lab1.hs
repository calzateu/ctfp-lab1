module Main where
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Lab1.hs                                                                   --
--                                                                           --
-- The implementation of the Bifunctor is based on Data.Bifunctor [1] and    --
-- tested with QuickCheck [2].                                               --
--                                                                           --
-- [1] Hackage.                                                              --
-- https://hackage.haskell.org/package/bifunctors-5/docs/Data-Bifunctor.html --
--                                                                           --
-- [2] Koen Claessen and John Hughes. QuickCheck: A lightweight tool for     --
-- random testing of Haskell Programs. ICFP '00.                             --
--                                                                           --
-- Implemented with GHC 9.4.2, Tested with QuickCheck 2.14.2                 --
-- and Managed with Stack 2.9.1.                                             --
-------------------------------------------------------------------------------


-- Bifunctor implementation. For our case it takes two arguments.
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d



-- Bellow are the instances of functors and the verification of identity
-- and composition properties. This properties are tested with Integers.


------------------- Pair instance of Bifunctor -------------------
-- This is the pair instance of functor. While Functor only apply a
-- function to the second element of pair, Bifunctor apply two functions
-- to both elements respectively.
instance Bifunctor (,) where
  bimap f g (x,y) = (f x,g y)

prop_assoc_b1 :: (Int,Int) -> Fun Int Int -> Fun Int Int -> Fun Int Int
                 -> Fun Int Int -> Bool
prop_assoc_b1 (a,b) (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
  (bimap (f1.f2) (g1.g2)) (a,b) == (bimap f1 g1) (bimap f2 g2 (a,b))

prop_id_b1 :: (Int,Int) -> Bool
prop_id_b1 (a,b) = bimap id id (a,b) == id (a,b)



------------------ 3-tuple instance of Bifunctor -----------------
-- This is the 3-tuple instance of functor. This apply two functions
-- to last two elements respectively, while the first element remains
-- the same.
instance Bifunctor ((,,) x1) where
  bimap f g (x1,a,b) = (x1,f a,g b)

prop_assoc_b2 :: (Int,Int,Int) -> Fun Int Int -> Fun Int Int
                 -> Fun Int Int -> Fun Int Int -> Bool
prop_assoc_b2 (x1,a,b) (Fun _ f1) (Fun _ f2) (Fun _ g1) (Fun _ g2) =
  (bimap (f1.f2) (g1.g2)) (x1,a,b) == (bimap f1 g1) (bimap f2 g2 (x1,a,b))

prop_id_b2 :: (Int,Int,Int) -> Bool
prop_id_b2 (x1,a,b) = bimap id id (x1,a,b) == id (x1,a,b)



------------------ Either instance of Bifunctor ------------------
-- This Functor takes two functions and applies the first function if the
-- left element is available and the second function if the right element
-- is available.
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
  quickCheck prop_assoc_b1
  quickCheck prop_id_b1
  quickCheck prop_assoc_b2
  quickCheck prop_id_b2
  quickCheck prop_assoc_b3
  quickCheck prop_id_b3

