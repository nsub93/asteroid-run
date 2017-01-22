module Main (main) where

import GameObject
import Graphics.Gloss
import Graphics.Gloss.Game as Game

window :: Display
window = InWindow "Asteroid run" (1000,700) (10,10)

background :: Color
background = black

fps :: Int
fps = 400

render :: GameWorld -> Picture
render gameWorld = 
    pictures [drawGameObject $ player1 gameWorld, drawGameObject $ player2 gameWorld]

processEvent :: Event -> GameWorld -> GameWorld
processEvent event world = world

update :: Float -> GameWorld -> GameWorld
update sec world = world

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
                            , player2 :: GameObject
                            , asteroids :: [GameObject]
                            , lasers :: [GameObject]
                            }

initialWorld = GameWorld { player1 = createGameObjectImgObject (-200, 0) (80, 80) player1BasicImg
                         , player2 = createGameObjectImgObject (200, 0) (80, 80) player2BasicImg
                         , asteroids = []
                         , lasers = []
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