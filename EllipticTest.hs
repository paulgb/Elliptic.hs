
import Elliptic
import Test.HUnit

c1 = Curve 5 3 7
p1 = Point 1 3
p2 = Point 1 4
p3 = Point 2 0
p4 = Point 6 2
p5 = Point 6 5

tests = test [
    -- Show Points
    "Show point" ~: "(1,3)" ~=? (show p1),
    "Show infinity" ~: "inf" ~=? (show Infinity),
    -- Point Equality
    (pointEq c1 p1 p1) ~? "Points equal",
    not (pointEq c1 p1 p2) ~? "Points not equal",
    (pointEq c1 Infinity Infinity) ~? "Infinity equal",
    not (pointEq c1 Infinity p3) ~? "Infinity not equal finite point 1",
    not (pointEq c1 p4 Infinity) ~? "Infinity not equal finite point 2",
    -- Show Curve
    "Show curve" ~: "y^2 == x^3 + 5x + 3 (mod 7)" ~=? (show c1),
    -- Determine if a point lies on a curve
    "Point on curve" ~: (pointOnCurve c1 p1) ~=? True,
    "Point not on curve" ~: (pointOnCurve c1 (Point 2 1)) ~=? False,
    "Infinity on curve" ~: (pointOnCurve c1 Infinity) ~=? True,
    -- Add points
    (pointEq c1 (addPoints c1 p3 p5) p1) ~? "Add Points"
    ]

main = (runTestTT tests) >> (return ())

