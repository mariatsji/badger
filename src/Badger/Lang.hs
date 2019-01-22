module Badger.Lang where

data Expr = Val Double | Sum Expr Expr

evaluate :: Expr -> Double
evaluate (Val a  ) = a
evaluate (Sum a b) = evaluate a + evaluate b
