module Lab1
    ( someFunc,
    ) where

import Prelude hiding (Functor, fmap)


class Bifunctor f where
  bimap :: ( a -> a') -> ( b -> b') -> ( f a b -> f a' b')



someFunc :: IO ()
someFunc = putStrLn "someFunc"

