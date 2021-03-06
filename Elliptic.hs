
module Elliptic where

import Maybe

-- todo: instance Num Point (?)
-- should points be aware of their curve?
-- is this where functors come in?

-- (x,y) point or infinity
data Point = Point Int Int | Infinity

instance Show Point where
  show Infinity = "inf"
  show (Point x y) =
    "("++(show x)++","
    ++(show y)++")"

-- a, b, m
data EllipticCurve = Curve Int Int Int

instance Show EllipticCurve where
  show (Curve a b m) = 
    "y^2 == x^3 + "++(show a)++
    "x + "++(show b)++
    " (mod "++(show m)++")"

pointEq :: EllipticCurve -> Point -> Point -> Bool
pointEq _ Infinity Infinity = True
pointEq (Curve a b m) (Point x1 y1) (Point x2 y2) =
    (x1 `mod` m) == (x2 `mod` m) &&
    (y1 `mod` m) == (y2 `mod` m)
pointEq _ Infinity _ = False
pointEq _ _ Infinity = False

-- point on curve
pointOnCurve :: EllipticCurve -> Point -> Bool
pointOnCurve (Curve a b m) (Point x y) = ((y^2) `mod` m) == ((x^3 + a*x + b) `mod` m)
pointOnCurve _ Infinity = True

-- slow (O(n^2)) algorithm for finding points in an elliptic curve
-- (temporary function for testing; todo: improve or remove)
points :: EllipticCurve -> [Point]
points (Curve a b m) =
  Infinity :
  do
    x <- [0..m-1]
    y <- [0..m-1]
    True <- return $ pointOnCurve (Curve a b m) (Point x y)
    return $ Point x y

addPoints :: EllipticCurve -> Point -> Point -> Point

addPoints _ Infinity p2 = p2
addPoints _ p1 Infinity = p1

addPoints _ (Point x1 y1) (Point x2 y2)
    | x1 == x2 && y1 /= y2      = Infinity

-- todo: mod earlier to simplify calculations
addPoints (Curve a b m) (Point x1 y1) (Point x2 y2)
    | not $ pointOnCurve (Curve a b m) (Point x1 y1)  = error "Point 1 does not lie on curve"
    | not $ pointOnCurve (Curve a b m) (Point x2 y2)  = error "Point 2 does not lie on curve"
    | otherwise =
        let grab (Just x) = x in
        let a // b = a * (grab (modInverse b m)) in
        let slope =
                if (x1, y1) == (x2, y2) then
                -- slope is the tangent of the curve at that point
                    ((3 * (x1 ^ 2)) + a) // (2 * y1)
                else
                -- slope is slope of the two distinct points
                    (y2 - y1) // (x2 - x1) in
        let x3  = -x1 - x2 + slope^2 in
        let x3' = x3 `mod` m in
        let y3  = -y1 + (slope * (x1 - x3)) in
        let y3' = y3 `mod` m in
        Point x3' y3'
    
negatePoint :: EllipticCurve -> Point -> Point
negatePoint (Curve _ _ m) (Point x y) = Point x ((-y) `mod` m)
negatePoint _ Infinity = Infinity

subtractPoints :: EllipticCurve -> Point -> Point -> Point
subtractPoints c p1 p2 = addPoints c p1 (negatePoint c p2)

-- modular inverse
-- (mInverse x m) x `mod` m == 1
-- this is a slow brute force algorithm, there are better
modInverse :: Int -> Int -> Maybe Int
modInverse x m =
  listToMaybe $ do
    x' <- [0..m-1]
    True <- return $ ((x * x') `mod` m) == 1
    return x'


