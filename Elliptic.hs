
module Elliptic where

import Maybe

-- (x,y) point or infinity
data Point = Point Int Int | Infinity | Debug Int Int Int Int

instance Show Point where
  show Infinity = "inf"
  show (Point x y) =
    "("++(show x)++","
    ++(show y)++")"
  show (Debug a b c d) =
    (show a) ++ " " ++ (show b) ++ " " ++
    (show c) ++ " " ++ (show d)

-- a, b, m
data EllipticCurve = Curve Int Int Int

instance Show EllipticCurve where
  show (Curve a b m) = 
    "y^2 == x^3 + "++(show a)++
    "x + "++(show b)++
    " (mod "++(show m)++")"

-- slow (O(n^2)) algorithm for finding points in an elliptic curve
points :: EllipticCurve -> [Point]
points (Curve a b m) =
  Infinity :
  do
    x <- [0..m-1]
    y <- [0..m-1]
    True <- return $ ((y^2) `mod` m) == ((x^3 + a*x + b) `mod` m)
    return $ Point x y

-- modular inverse
-- (mInverse x m) x `mod` m == 1
-- this is a slow brute force algorithm, there are better
modInverse :: Int -> Int -> Maybe Int
modInverse x m =
  listToMaybe $ do
    x' <- [0..m-1]
    True <- return $ ((x * x') `mod` m) == 1
    return x'

-- somewhat inspired by this code:
-- http://programmingpraxis.com/2009/07/31/elliptic-curve-factorization/2/

addPoints :: EllipticCurve -> Point -> Point -> Point

addPoints _ Infinity p2 = p2
addPoints _ p1 Infinity = p1

-- todo: factor out duplicate code
-- todo: check that point lies on curve
addPoints (Curve a b m) (Point x y) (Point u v)
    | (x, y) == (u, v) =
        if ((y + v) `mod` m) == 0 then
            Infinity
        else
            let n = (3 * x * x) + a in
            let (Just d) = (modInverse (2 * y) m) in
            let x' = ((n*d)^2 - (x + u)) `mod` m in
            let y' = ((n*d*(x-x'))-y) `mod` m in
            Point x' y'

addPoints (Curve a b m) (Point x y) (Point u v) =
  let (Just d) = modInverse (u - x) m in
  let n = v - y in
  let x' = ((n*d)^2 - (x + u)) `mod` m in
  let y' = ((n*d*(x-x'))-y) `mod` m in
  Point x' y'

negatePoint :: EllipticCurve -> Point -> Point
negatePoint (Curve _ _ m) (Point x y) = Point x ((-y) `mod` m)
negatePoint _ Infinity = Infinity

subtractPoints :: EllipticCurve -> Point -> Point -> Point
subtractPoints c p1 p2 = addPoints c p1 (negatePoint c p2)

