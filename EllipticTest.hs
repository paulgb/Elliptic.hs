
import Elliptic
import Test.HUnit

tests = test [
    "Show point" ~: "show $ Point 2 3" ~: "(2,3)" ~=? (show $ Point 2 3),
    "Show infinity" ~: "show Infinity" ~: "inf" ~=? (show Infinity)
    ]

main = (runTestTT tests) >> (return ())

