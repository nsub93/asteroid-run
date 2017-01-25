module Main (main) where

import GameObject
import Graphics.Gloss
import Graphics.Gloss.Game as Game
import Data.List

window :: Display
window = InWindow "Asteroid run" (1000,700) (10,10)

background :: Color
background = black

fps :: Int
fps = 200

render :: GameWorld -> Picture
render gameWorld = 
    pictures ([ backgroundImage]++
              ( map (drawGameObject) (lasers gameWorld))++
              [ drawGameObject $ player1 gameWorld
             , drawGameObject $ player2 gameWorld
              ]++ ( map (drawGameObject) (asteroids gameWorld)++ [translate 300 200 $ text ( show (pointCounter gameWorld))]))

processEvent :: Event -> GameWorld -> GameWorld

-- prvi igrac
processEvent (EventKey (SpecialKey KeyLeft) Down _ _) world = world { player2Left = True }
processEvent (EventKey (SpecialKey KeyRight) Down _ _) world = world { player2Right = True }
processEvent (EventKey (SpecialKey KeyLeft) Up _ _) world = world { player2Left = False }
processEvent (EventKey (SpecialKey KeyRight) Up _ _) world = world { player2Right = False }

processEvent (EventKey (SpecialKey KeyUp) Down _ _) world = world { player2Up = True }
processEvent (EventKey (SpecialKey KeyDown) Down _ _) world = world { player2Down = True }
processEvent (EventKey (SpecialKey KeyUp) Up _ _) world = world { player2Up = False }
processEvent (EventKey (SpecialKey KeyDown) Up _ _) world = world { player2Down = False }

processEvent (EventKey (SpecialKey KeySpace) Down _ _) world = world { lasers = (addLaser (lasers world) (getGameObjectCoordinates (player1 world)) laserGreenImage) }
processEvent (EventKey (SpecialKey KeyEnter) Down _ _) world = world { lasers = (addLaser (lasers world) (getGameObjectCoordinates (player2 world)) laserRedImage) }


-- drugi igrac
processEvent (EventKey (Char 'a') Down _ _) world = world { player1Left = True }
processEvent (EventKey (Char 'd') Down _ _) world = world { player1Right = True }
processEvent (EventKey (Char 'a') Up _ _) world = world { player1Left = False }
processEvent (EventKey (Char 'd') Up _ _) world = world { player1Right = False }

processEvent (EventKey (Char 'w') Down _ _) world = world { player1Up = True }
processEvent (EventKey (Char 's') Down _ _) world = world { player1Down = True }
processEvent (EventKey (Char 'w') Up _ _) world = world { player1Up = False }
processEvent (EventKey (Char 's') Up _ _) world = world { player1Down = False }

processEvent _ world = world

addLaser :: [GameObject]->(Float,Float)->Picture->[GameObject]
addLaser laserArray (x,y) laserImage = laserArray ++ [(createGameObjectImgObject (x, y+40) (14, 37) laserImage)]

updateCounter :: GameWorld->GameWorld
updateCounter world = world {frameCounter = ((frameCounter world)+1)}


moveObjects :: GameWorld -> GameWorld
moveObjects world = world { player1 = newPlayer1 , player2 = newPlayer2, lasers= newLasers, asteroids=newAsteroids }
            where
                step = 1.5
            
                (player1X,player1Y) = getGameObjectCoordinates (player1 world)
                (player2X,player2Y) = getGameObjectCoordinates (player2 world)

                dx1 = if ((player1Left world) && (not $ player1X - 40 < -500)) then -step 
                      else if ((player1Right world) && (not $ player1X + 40 > 500)) then step              
                      else 0.0

                dx2 = if ((player2Left world) && (not $ player2X - 40 < -500)) then  - step
                      else if ((player2Right world) && (not $ player2X + 40 > 500)) then step  
                      else 0.0             
                      
                dy1 = if ((player1Up world) && (not $ player1Y + 40 > 350)) then step
                      else if ((player1Down world) && (not $ player1Y - 40 < -350)) then -step              
                      else 0.0

                dy2 = if ((player2Up world) && (not $ player2Y + 40 > 350)) then step
                      else if ((player2Down world) && (not $ player2Y - 40 < -350)) then -step              
                      else 0.0

                p1 =  moveGameObject (player1 world) dx1 dy1                                
                p2 =  moveGameObject (player2 world) dx2 dy2

                newPlayer1 = if (dx1 > 0 || (player1Right world)) then changeGameObjectImage p1 player1RightImg
                             else if (dx1 < 0 || (player1Left world)) then changeGameObjectImage p1 player1LeftImg
                             else changeGameObjectImage p1 player1BasicImg

                newPlayer2 = if (dx2 > 0 || (player2Right world)) then changeGameObjectImage p2 player2RightImg
                             else if (dx2 < 0 || (player2Left world)) then changeGameObjectImage p2 player2LeftImg
                             else changeGameObjectImage p2 player2BasicImg
                
                astep = 3.0
                (newLasers,newAsteroids) = laserAsteroidOutOfBound $ laserAsteroidCollision ((map (\x -> moveGameObject x 0 1.6) (lasers world)),(map (\x -> moveGameObject x (-0.7 * step) (-1 * step)) (asteroids world)))


laserAsteroidOutOfBound :: ([GameObject],[GameObject])->([GameObject],[GameObject])
laserAsteroidOutOfBound (lasers,asteroids) =  (filter (\x ->not $ objectOutOfBound x) lasers,
                                              filter (\x ->not $ objectOutOfBound x) asteroids)
objectOutOfBound :: GameObject -> Bool
objectOutOfBound obj = let
                              (objX,objY)=getGameObjectCoordinates obj
                       in
                              (objX>600)||(objX< -600)||(objY>450)||(objY< -450)

laserAsteroidCollision :: ([GameObject],[GameObject])->([GameObject],[GameObject])
laserAsteroidCollision (lasers,asteroids) = let
                                                lasersId = zip [1..] lasers
                                                asteroidsId = zip [1..] asteroids
                                                cartesianList = [(x,y) | x<-lasersId,y<-asteroidsId]
                                                collisions = filter (\(x,y) ->  collisionExists (snd x) (snd y)) cartesianList
                                                (lasersRemove,_) = unzip $ map (fst) collisions
                                                (asteroidsRemove,_) = unzip $ map (snd) collisions
                                          in
                                                (map snd (filter (\(x,y) -> notElem x lasersRemove) lasersId ),map snd (filter (\(x,y) -> notElem x asteroidsRemove) asteroidsId ))

collisionExists obj1 obj2 = let
                                    (colType,_,_,_,_) = detectCollision  obj1  obj2
                            in
                                    colType /= NoCollision


createAsteroid::Float->GameWorld->GameWorld
createAsteroid sec world = world { asteroids=newAsteroids }
            where
                seed = (frameCounter world) * (ceiling (sec + 1))    
                randX = nextRand seed
                randY = nextRand randX
                x' = randX `mod` 1000 - 400
                y' = randY `mod` 700 - 250 
                randStep = nextRand randY
                step = randStep `mod` 250 
                step1 = nextRand step `mod` 300 

                newAsteroids =  if (0 == mod (frameCounter world) step) then (asteroids world) ++ [createGameObjectImgObject ( fromInteger $ toInteger x' , 450)  (80, 80) asteroidSmallImage]
                                else if (0 == mod (frameCounter world) step1) then (asteroids world) ++ [createGameObjectImgObject (600 , fromInteger $ toInteger x' ) (80, 80) asteroidSmallImage]
                                else asteroids world
 
update :: Float -> GameWorld -> GameWorld
update sec world = moveObjects $ updateCounter $ createAsteroid sec world 

-- ucitavanje svih slika
player1BasicImg = png "images/greenBasic.png"
player1RightImg = png "images/greenRight.png"
player1LeftImg = png "images/greenLeft.png"

player2BasicImg = png "images/orangeBasic.png"
player2RightImg = png "images/orangeRight.png"
player2LeftImg = png "images/orangeLeft.png"

backgroundImage = png "images/sky.png"
laserRedImage = png "images/laserRed.png"
laserGreenImage = png "images/laserGreen.png"
asteroidBigImage = png "images/asteroidBig.png"
asteroidSmallImage = png "images/asteroidSmall.png"


-- kod preuzet sa vezbi prethodnih godina
randMultiplier = 25173
randIncrement = 13849
randModulus = 65536

nextRand :: Int -> Int
nextRand  n = (randMultiplier * n + randIncrement) `mod` randModulus

data GameWorld = GameWorld  { player1 :: GameObject
                            , player1Down :: Bool
                            , player1Up :: Bool
                            , player1Left :: Bool
                            , player1Right :: Bool
                            , player2 :: GameObject
                            , player2Down :: Bool
                            , player2Up :: Bool
                            , player2Left :: Bool
                            , player2Right :: Bool
                            , asteroids :: [GameObject]
                            , lasers :: [GameObject]
                            , frameCounter :: Int
                            , pointCounter :: Int
                            }

initialWorld = GameWorld { player1 = createGameObjectImgObject (0, 0) (80, 80) player1BasicImg
                         , player2 = createGameObjectImgObject (0, -310) (80, 80) player2BasicImg
                         , asteroids = []
                         , lasers = []
                         , player1Down = False
                         , player1Up = False
                         , player1Left = False
                         , player1Right = False
                         , player2Down = False
                         , player2Up = False
                         , player2Left = False
                         , player2Right = False
                         , frameCounter = 0
                         , pointCounter = 0
                         }

main :: IO ()
main = Game.play
           window
           background
           fps
           initialWorld
           render
           processEvent
           [update]