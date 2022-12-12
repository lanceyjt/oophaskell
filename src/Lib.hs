module Lib where

-- Define "Class" for Point, Circle, Rectange and NewRectangle
data Point = Point { indexX :: Float,
                     indexY :: Float }
    deriving (Show, Eq)

data Circle = Circle { center :: Point,
                       radius :: Float }
    deriving (Show, Eq)

data Rectangle = Rectangle { upperLeftVertex :: Point,
                             lowerRightVertex :: Point }
    deriving (Show, Eq)

data NewRectangle = NewRectangle { rectangle :: Rectangle,
                                   isSquare :: Bool }
    deriving (Show, Eq)


-- Functions to create objects
constructPoint :: Float -> Float -> Point
constructPoint = Point

constructCircle :: Float -> Float -> Float -> Circle
constructCircle x y r = Circle { center = constructPoint x y, 
                                 radius = r }

constructRectangle :: Float -> Float -> Float -> Float -> Rectangle
constructRectangle x1 y1 x2 y2 = Rectangle { upperLeftVertex = constructPoint x1 y1, 
                                             lowerRightVertex = constructPoint x2 y2 }
                                             
constructNewRectangle :: Float -> Float -> Float -> Float -> Bool -> NewRectangle
constructNewRectangle x1 y1 x2 y2 b = NewRectangle { rectangle = constructRectangle x1 y1 x2 y2,
                                                     isSquare = b }                            

-- Goal: Implement operations to access states in classes; 
--       Implement operations in classes;
--       Implement inheritance of operations, Point, Circle and Rectangle objects will use the same operations 

-- TypeClass WithX
class WithX a where
    getX :: a -> String -> Float

instance WithX Point where
    getX p _ = indexX p

instance WithX Circle where 
    getX c _ = (indexX . center) c

instance WithX Rectangle where
    getX r "upperLeftVertex" = (indexX . upperLeftVertex) r
    getX r "lowerRightVertex" = (indexX . lowerRightVertex) r

-- Typeclass SetX
class SetX a where
    setX :: Float -> String -> a -> a

instance SetX Point where
    setX x _ (Point _ y) = Point x y

instance SetX Circle where
    setX x _ (Circle center radius) =
        Circle (setX x "" center) radius

instance SetX Rectangle where
    setX x "upperLeftVertex" (Rectangle upperLeftVertex lowerRightVertex) =
        Rectangle (setX x "" upperLeftVertex) lowerRightVertex
    setX x "lowerRightVertex" (Rectangle upperLeftVertex lowerRightVertex) = 
        Rectangle upperLeftVertex (setX x "" lowerRightVertex)

-- apply function to modify X (inheritance)
modifyX :: (WithX a, SetX a) => (Float -> Float) -> a -> String -> a
modifyX f a s = setX (f (getX a s)) s a

-- TypeClass ClassRectangle 
-- Goal: Implement inheritance of operations, both Rectangle and NewRectangle objects will use operations in this TypeClass
class ClassRectangle a where

    getRectangle :: a -> Rectangle

    display :: a -> String
    display a = 
        let
            Rectangle{ upperLeftVertex = p1, lowerRightVertex = p2} = getRectangle a
        
         in "Rectangle " ++ show p1 ++ "," ++ show p2


instance ClassRectangle Rectangle where
    getRectangle = id


instance ClassRectangle NewRectangle where
    getRectangle = rectangle

    display b = "New " ++ 
                show (getRectangle b) ++ 
                ", isSquare " ++ show (isSquare_ (getRectangle b))

-- Check whether an object from TypeClass 'ClassRectangle' is a valid rectangle
-- A valid rectangle means the X index of the upper-left vertex is less than the X index of the lower-right
-- and the Y index of upper-left vertex is greater than the Y index of the lower-right
isValidRectangle :: ClassRectangle a => a -> Bool
isValidRectangle x = let r = getRectangle x
                         x1 = (indexX .upperLeftVertex) r
                         y1 = (indexY .upperLeftVertex) r
                         x2 = (indexX .lowerRightVertex) r
                         y2 = (indexY .lowerRightVertex)  r
                      in x1 < x2 && y1 > y2

-- Check whether an object from TypeClass 'ClassRectangle' is a square
-- A rectangle is a square if the rectange is valid 
-- and its length and width are equal
isSquare_ :: ClassRectangle a => a -> Bool
isSquare_ x = let r = getRectangle x
                  x1 = (indexX .upperLeftVertex) r
                  y1 = (indexY .upperLeftVertex) r
                  x2 = (indexX .lowerRightVertex) r
                  y2 = (indexY .lowerRightVertex)  r
               in x1 < x2 && y1 > y2 && (x2-x1 == y2-y1)       

              