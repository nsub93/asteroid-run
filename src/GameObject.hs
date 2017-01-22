module GameObject 
( GameObject
, CollisionType (TopCollision, BottomCollision, LeftCollision, RightCollision, NoCollision)
, createGameObject
, moveGameObject  
, drawGameObject
, getGameObjectCoordinates
, getGameObjectSize
, detectCollision
, getCollisionType
, getLeftCollisionCoefficient
, getRightCollisionAngleCoefficient
, getTopCollisionAngleCoefficient
, getBottomCollisionAngleCoefficient
) where

import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Geometry.Line
import Data.Maybe

data GameObject = GameObject{ x :: Float
                            , y :: Float
                            , width :: Float
                            , height :: Float
                            , image :: Picture
                            }

data CollisionType = TopCollision 
                   | BottomCollision
                   | LeftCollision
                   | RightCollision
                   | NoCollision
                   deriving (Eq) 


type Collision = (CollisionType, Float, Float, Float, Float)

createGameObject :: (Float, Float) -> (Float, Float) -> (String, Float, Float) ->  GameObject
createGameObject (nx, ny) (w, h) (path, pw ,ph) = obj
                        where 
                            cx = w / pw
                            cy = h / ph
                            pic = scale cx cy $ png path
                            obj = GameObject{ x = nx
                                            , y = ny
                                            , width = w
                                            , height = h
                                            , image = pic 
                                            }

moveGameObject :: GameObject -> Float -> Float -> GameObject
moveGameObject obj dx dy = obj { x = x', y = y' }
                        where 
                            x' = x obj + dx
                            y' = y obj + dy 

drawGameObject :: GameObject -> Picture
drawGameObject obj = translate (x obj) (y obj) $ image obj

getGameObjectCoordinates :: GameObject -> (Float, Float)
getGameObjectCoordinates obj = (x obj , y obj) 
                
getGameObjectSize :: GameObject -> (Float, Float)
getGameObjectSize obj = (width obj , height obj) 

detectCollision :: GameObject -> GameObject -> (CollisionType, Float, Float, Float, Float)
detectCollision obj1 obj2 = (collisionType, 
                             leftCollisionCoefficient, 
                             rightCollisionAngleCoefficient, 
                             topCollisionAngleCoefficient, 
                             bottomCollisionAngleCoefficient)
  where 
    (x1, y1) = getGameObjectCoordinates obj1
    (x2, y2) = getGameObjectCoordinates obj2
    (w1, h1) = getGameObjectSize obj1
    (w2, h2) = getGameObjectSize obj2

    -- Detekcija sa koje strane se desila kolizija
    leftCollision1 = intersectSegSeg (x1, y1+h1/2) (x1+w1/2, y1) (x2-w2/2, y2+h2/2) (x2-w2/2, y2-h2/2)
    leftCollision2 = intersectSegSeg (x1, y1-h1/2) (x1+w1/2, y1) (x2-w2/2, y2+h2/2) (x2-w2/2, y2-h2/2)

    rightCollision1 = intersectSegSeg (x1, y1+h1/2) (x1-w1/2, y1) (x2+w2/2, y2+h2/2) (x2+w2/2, y2-h2/2)
    rightCollision2 = intersectSegSeg (x1, y1-h1/2) (x1-w1/2, y1) (x2+w2/2, y2+h2/2) (x2+w2/2, y2-h2/2)

    topCollision1 = intersectSegSeg (x1-w1/2, y1) (x1, y1-h1/2) (x2+w2/2, y2+h2/2) (x2-w2/2, y2+h2/2)
    topCollision2 = intersectSegSeg (x1+w1/2, y1) (x1, y1-h1/2) (x2+w2/2, y2+h2/2) (x2-w2/2, y2+h2/2)

    bottomCollision1 = intersectSegSeg (x1-w1/2, y1) (x1, y1+h1/2) (x2+w2/2, y2-h2/2) (x2-w2/2, y2-h2/2)
    bottomCollision2 = intersectSegSeg (x1+w1/2, y1) (x1, y1+h1/2) (x2+w2/2, y2-h2/2) (x2-w2/2, y2-h2/2)

    -- Ucitavanje tacaka u kojima se desila kolizija
    (leftCollision1X, leftCollision1Y) = fromMaybe (0, 0) (leftCollision1)
    (leftCollision2X, leftCollision2Y) = fromMaybe (0, 0) (leftCollision2)

    (rightCollision1X, rightCollision1Y) = fromMaybe (0, 0) (rightCollision1)
    (rightCollision2X, rightCollision2Y) = fromMaybe (0, 0) (rightCollision2)

    (topCollision1X, topCollision1Y) = fromMaybe (0, 0) (topCollision1)
    (topCollision2X, topCollision2Y) = fromMaybe (0, 0) (topCollision2)

    (bottomCollision1X, bottomCollision1Y) = fromMaybe (0, 0) (bottomCollision1)
    (bottomCollision2X, bottomCollision2Y) = fromMaybe (0, 0) (bottomCollision2)


    leftCollisionCoefficient        = if (leftCollision1 /= Nothing || leftCollision2 /= Nothing) then 
                                        ((leftCollision2X+leftCollision1X)/2)-x2
                                      else 0

    rightCollisionAngleCoefficient  = if rightCollision1 /= Nothing || rightCollision2 /= Nothing then 
                                        ((rightCollision2X+rightCollision1X)/2)-x2
                                      else 0

    topCollisionAngleCoefficient    = if (topCollision1 /= Nothing || topCollision2 /= Nothing) then 
                                        ((topCollision2X+topCollision1X)/2)-x2
                                      else 0

    bottomCollisionAngleCoefficient = if bottomCollision1 /= Nothing || bottomCollision2 /= Nothing then 
                                        ((bottomCollision2X+bottomCollision1X)/2)-x2
                                      else 0

    collisionType = 
      if topCollision1 /= Nothing || topCollision2 /= Nothing then 
        TopCollision 
      else if bottomCollision1 /= Nothing || bottomCollision2 /= Nothing then 
        BottomCollision 
      else if leftCollision1 /= Nothing || leftCollision2 /= Nothing then 
        LeftCollision 
      else if rightCollision1 /= Nothing || rightCollision2 /= Nothing then
        RightCollision
      else
        NoCollision

getCollisionType :: GameObject -> CollisionType
getCollisionType (collisionType, _, _, _, _) = collisionType

getLeftCollisionCoefficient :: GameObject -> Float
getLeftCollisionCoefficient (_, leftCollisionCoefficient, _, _, _) = leftCollisionCoefficient

getRightCollisionAngleCoefficient :: GameObject -> Float
getRightCollisionAngleCoefficient (_, _, rightCollisionAngleCoefficient, _, _) = rightCollisionAngleCoefficient

getTopCollisionAngleCoefficient :: GameObject -> Float
getTopCollisionAngleCoefficient (_, _, _, topCollisionAngleCoefficient, _) = topCollisionAngleCoefficient

getBottomCollisionAngleCoefficient :: GameObject -> Float
getBottomCollisionAngleCoefficient (_, _, _, _, bottomCollisionAngleCoefficient) = bottomCollisionAngleCoefficient
