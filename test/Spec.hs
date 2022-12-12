import Lib
    ( constructCircle,
      constructNewRectangle,
      constructPoint,
      constructRectangle,
      isSquare_,
      isValidRectangle,
      modifyX,
      Circle(Circle, center),
      Point(Point),
      Rectangle(Rectangle),
      SetX(setX),
      WithX(getX) )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual )
import Test.Tasty.QuickCheck ( (===), Property, testProperty )
import Test.Tasty.DejaFu ( Basic, Program, testAuto )
import Control.Concurrent
import Control.Monad
import Control.Monad.Conc.Class
    ( MonadConc(readMVar, newEmptyMVar, putMVar), fork )
import System.Random ( StdGen, getStdGen, Random(random) )

main :: IO ()
main =
  do defaultMain tests

tests :: TestTree
tests =
   testGroup
        "OOP in haskell"
          -- object creation
        [ testProperty "Create point object"
            (prop_construct_point_objects :: Float -> Float -> Property),
          testProperty "Create circle object"
            (prop_construct_circle_objects :: Float -> Float -> Float -> Property),
          testProperty "Construct rectangle object"
            (prop_construct_rectangle_objects :: Float -> Float -> Float -> Float -> Property),

          -- reference object states in calss (data abstraction)
          testProperty "Reference X index in Point Class"
            (prop_ref_xidx_point_objects :: Float -> Float -> Property),
          testProperty "Reference X index in Circle Class"
            (prop_construct_circle_objects :: Float -> Float -> Float -> Property),
          testProperty "Reference X index in Upperleft Vertex of Rectangle Class"
            (prop_ref_xidx_rectangle_objects_upperleftvertex :: Float -> Float -> Float -> Float -> Property),
          testProperty "Reference X index in Lowerright Vertex of Rectangle Class"
            (prop_ref_xidx_rectangle_objects_lowerrightvertex :: Float -> Float -> Float -> Float -> Property),

          -- set x index values
          testProperty "Set X index of Objects in Point Class"
            (prop_set_xidx_point_objects :: Float -> Float -> Float -> Property),
          testProperty "Set X index of Objects in Circle Class"
            (prop_set_xidx_circle_objects :: Float -> Float -> Float -> Float -> Property),
          testProperty "Set X index of Upperleft Vertex of Objects in Rectangle Class"
            (prop_set_xidx_rectangle_objects_upperleftvertex :: Float -> Float -> Float -> Float -> Float -> Property),
          testProperty "Set X index of Lowerright Vertex of Objects in Rectangle Class"
            (prop_set_xidx_rectangle_objects_lowerrightvertex :: Float -> Float -> Float -> Float -> Float -> Property),
          
          -- modify x with functions
          modifyXPointTest, 
          modifyXCircleTest,
          modifyXCircleTest,
          modifyXRectangleTest,

          -- inheritance of functions from rectangle to newrectangle
          inheritanceNewRectangleIsValidTest,
          inheritanceNewRectangleIsSquareTest,

          -- concurrency test
          testAuto concurrentCreateObjects,
          testAuto concurrentObjectOperations

        ]

modifyXPointTest :: TestTree
modifyXPointTest  =
  testCase "Modify X index with functions on Objects in Point Class"
              ( assertEqual "Modify x in Objects in Point Class"
                (modifyX (+3) (Point 0 4) "")
                (Point 3 4)
              )

modifyXCircleTest :: TestTree
modifyXCircleTest =
  testCase "Modify X index with functions on Objects in Circle Class"
              ( assertEqual "Modify x in Objects in Circle Class"
                (modifyX (*4) (constructCircle 1.1 4 3.2) "")
                (Circle (Point 4.4 4) 3.2)
              )


modifyXRectangleTest :: TestTree
modifyXRectangleTest  =
    testCase "Modify X index with functions on Objects in Rectangle Class"
              ( assertEqual "Modify x in Objects in Rectangle Class"
                (modifyX (/2) (constructRectangle 1 8 8 1) "lowerRightVertex")
                (Rectangle (Point 1 8 ) (Point 4 1))
              )

inheritanceNewRectangleIsValidTest :: TestTree
inheritanceNewRectangleIsValidTest =
          testCase "Inheritance of isValidRectangle function in NewRectangle from ClassRectangle"
              ( assertEqual "Inheritance of isValidRectangle"
                (isValidRectangle (constructNewRectangle 1 5 0 2 False))
                (isValidRectangle (constructRectangle 1 5 0 2 ))
              )

inheritanceNewRectangleIsSquareTest :: TestTree
inheritanceNewRectangleIsSquareTest =
          testCase "Inheritance of isSquare function in NewRectangle from ClassRectangle"
              ( assertEqual "Inheritance of isSquare"
                (isSquare_  (constructNewRectangle 1.5 7 7.5 1 True))
                (isSquare_  (constructRectangle 1.5 7 7.5 1 ))
              )


-- Construct Objects --
prop_construct_point_objects :: Float -> Float -> Property
prop_construct_point_objects x y = constructPoint x y === Point x y

prop_construct_circle_objects :: Float -> Float -> Float -> Property
prop_construct_circle_objects x y r = constructCircle x y r === Circle (Point x y) r

prop_construct_rectangle_objects :: Float -> Float -> Float -> Float -> Property
prop_construct_rectangle_objects x1 y1 x2 y2 = constructRectangle x1 y1 x2 y2 === Rectangle (Point x1 y1) (Point x2 y2)

-- Reference X index in Classes (data abstraction) --
prop_ref_xidx_point_objects :: Float -> Float -> Property
prop_ref_xidx_point_objects x y = getX (Point x y) "" === x

prop_ref_xidx_circle_objects :: Float -> Float -> Float -> Property
prop_ref_xidx_circle_objects x y r = getX (Circle (Point x y) r) "" === x

prop_ref_xidx_rectangle_objects_upperleftvertex :: Float -> Float -> Float -> Float -> Property
prop_ref_xidx_rectangle_objects_upperleftvertex x1 y1 x2 y2 =
    getX (Rectangle (Point x1 y1) (Point x2 y2)) "upperLeftVertex" === x1

prop_ref_xidx_rectangle_objects_lowerrightvertex :: Float -> Float -> Float -> Float -> Property
prop_ref_xidx_rectangle_objects_lowerrightvertex x1 y1 x2 y2 =
    getX (Rectangle (Point x1 y1) (Point x2 y2)) "lowerRightVertex" === x2

-- Set X index in classes (inheritance) --
prop_set_xidx_point_objects :: Float -> Float -> Float -> Property
prop_set_xidx_point_objects newx oldx y=
    setX newx "" (Point oldx y) === Point newx y

prop_set_xidx_circle_objects :: Float -> Float -> Float -> Float -> Property
prop_set_xidx_circle_objects newx oldx y r =
    setX newx "" (Circle (Point oldx y) r) === Circle (Point newx y) r

prop_set_xidx_rectangle_objects_upperleftvertex :: Float -> Float -> Float -> Float -> Float -> Property
prop_set_xidx_rectangle_objects_upperleftvertex newx oldx1 y1 x2 y2 =
    setX newx "upperLeftVertex" (Rectangle (Point oldx1 y1) (Point x2 y2)) === Rectangle (Point newx y1) (Point x2 y2)

prop_set_xidx_rectangle_objects_lowerrightvertex :: Float -> Float -> Float -> Float -> Float -> Property
prop_set_xidx_rectangle_objects_lowerrightvertex newx x1 y1 oldx2 y2 =
    setX newx "lowerRightVertex" (Rectangle (Point x1 y1) (Point oldx2 y2)) === Rectangle (Point x1 y1) (Point newx y2)

-- Concurrency --
concurrentCreateObjects :: Program Basic IO String
concurrentCreateObjects = do 
    v <- Control.Monad.Conc.Class.newEmptyMVar 
     
    -- concurrently create point objects  
    fork (replicateM_ 10
            ( do gen <- getStdGen
                 let (num1, _) = random gen :: (Float, StdGen)
                     (num2, _) = random gen :: (Float, StdGen)
                     toShow = Point num1 num2
                 Control.Monad.Conc.Class.putMVar v (show toShow)
            ))

    -- concurrently create circle objects  
    fork (replicateM_ 10
            ( do gen <- getStdGen
                 let (num1, _) = random gen :: (Float, StdGen)
                     (num2, _) = random gen :: (Float, StdGen)
                     (num3, _) = random gen :: (Float, StdGen)
                     c = Circle (Point num1 num2) num3
                     toShow = center c
                 Control.Monad.Conc.Class.putMVar v (show toShow)
            ))
    Control.Monad.Conc.Class.readMVar v

concurrentObjectOperations :: Program Basic IO String
concurrentObjectOperations = do
    v2 <- Control.Monad.Conc.Class.newEmptyMVar 

    -- concurrently run operations on Rectangle objects 
    fork (do let r = constructRectangle 1 8 8 1
                 toShow = isValidRectangle r
             Control.Monad.Conc.Class.putMVar v2 (show toShow))
    
     -- concurrently run operations on NewRectangle objects 
    fork (do let nr = constructNewRectangle 1 8 8 1 True
                 toShow = isValidRectangle nr
             Control.Monad.Conc.Class.putMVar v2 (show toShow))
    Control.Monad.Conc.Class.readMVar v2