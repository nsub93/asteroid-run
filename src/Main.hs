module Main (main) where

import GameObject
import Graphics.Gloss
import Graphics.Gloss.Game as Game

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
              ])

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

processEvent (EventKey (SpecialKey KeySpace) Down _ _) world = world { lasers = (addLaser (lasers world) (getGameObjectCoordinates (player1 world))) }
processEvent (EventKey (SpecialKey KeyEnter) Down _ _) world = world { lasers = (addLaser (lasers world) (getGameObjectCoordinates (player2 world))) }


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

addLaser :: [GameObject]->(Float,Float)->[GameObject]
addLaser laserArray (x,y) = laserArray ++ [(createGameObjectImgObject (x, y+10) (14, 37) laserImage)]

movePlayers :: GameWorld -> GameWorld
movePlayers world = world { player1 = newPlayer1 , player2 = newPlayer2, lasers= newLasers }
            where
                step = 1.5
  
                dx1 = if (player1Left world) then - step 
                      else if (player1Right world) then step              
                      else 0.0

                dx2 = if (player2Left world) then  - step
                      else if (player2Right world) then step    
                      else 0.0             
                      
                dy1 = if (player1Up world) then step
                      else if (player1Down world) then -step              
                      else 0.0

                dy2 = if (player2Up world) then step
                      else if (player2Down world) then -step              
                      else 0.0

                p1 =  moveGameObject (player1 world) dx1 dy1                                
                p2 =  moveGameObject (player2 world) dx2 dy2

                newPlayer1 = if dx1 > 0 then changeGameObjectImage p1 player1RightImg
                             else if dx1 < 0 then changeGameObjectImage p1 player1LeftImg
                             else changeGameObjectImage p1 player1BasicImg

                newPlayer2 = if dx2 > 0 then changeGameObjectImage p2 player2RightImg
                             else if dx2 < 0 then changeGameObjectImage p2 player2LeftImg
                             else changeGameObjectImage p2 player2BasicImg
                
                newLasers = map (\x -> moveGameObject x 0 1.6) (lasers world)



update :: Float -> GameWorld -> GameWorld
update sec world = movePlayers world

-- ucitavanje svih slika
player1BasicImg = png "images/greenBasic.png"
player1RightImg = png "images/greenRight.png"
player1LeftImg = png "images/greenLeft.png"

player2BasicImg = png "images/orangeBasic.png"
player2RightImg = png "images/orangeRight.png"
player2LeftImg = png "images/orangeLeft.png"

backgroundImage = png "images/sky.png"
laserImage = png "images/laser.png"

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
                            }

initialWorld = GameWorld { player1 = createGameObjectImgObject (-200, 0) (80, 80) player1BasicImg
                         , player2 = createGameObjectImgObject (200, 0) (80, 80) player2BasicImg
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