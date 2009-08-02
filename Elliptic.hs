
module Elliptic where

-- (x,y) point or infinity
data Point = Point Int Int | Infinity

-- a, b, m
data EllipticCurve = Curve Int Int Int

instance Show EllipticCurve where
  show (Curve a b m) = 
    "y^2 == x^3 + "++(show a)++
    "x + "++(show b)++
    " (mod "++(show m)++")"

-- slow (O(n^2)) algorithm for finding points in an elliptic curve
points :: EllipticCurve -> [(Int, Int)]
points (Curve a b m) =
  do
    x <- [0..m-1]
    y <- [0..m-1]
    True <- return $ ((y^2) `mod` m) == ((x^3 + a*x + b) `mod` m)
    return (x, y)

