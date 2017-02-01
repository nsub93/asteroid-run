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
              ( map (drawGameObject) (lasersPlayer1 gameWorld))++
              ( map (drawGameObject) (lasersPlayer2 gameWorld))++
              (if (length (player1Lives gameWorld))>0 then [ drawGameObject $ player1 gameWorld] else [])++
              (if (length (player2Lives gameWorld))>0 then [ drawGameObject $ player2 gameWorld] else [])
              ++ ( map (drawGameObject) (asteroids gameWorld))++ 
              [translate (-476.5) 140 $ color white $ scale 0.3 0.3 $ text ( show (pointsPlayer1 gameWorld))]++
              [translate 438 140 $ color white $ scale 0.3 0.3 $ text ( show (pointsPlayer2 gameWorld))]++
              ( map (drawGameObject) (player1Lives gameWorld))++
              ( map (drawGameObject) (player2Lives gameWorld))++
              (if (length (player1Lives gameWorld))>0 && (player1Cooldown gameWorld)>0 then [drawAura (player1 gameWorld) (player1Aura gameWorld)] else [])++
              (if (length (player2Lives gameWorld))>0 && (player2Cooldown gameWorld)>0 then [drawAura (player2 gameWorld) (player2Aura gameWorld)] else [])++
              (if ((length (player1Lives gameWorld))==0) && ((length (player2Lives gameWorld))==0) then [gameOverImg] else [])++
              (if (gamePaused gameWorld) then [gamePausedImg] else []))

drawAura :: GameObject->GameObject->Picture
drawAura player playerAura = translate pX pY ( drawGameObject playerAura)
                              where
                                    (pX,pY) = getGameObjectCoordinates player

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

processEvent (EventKey (SpecialKey KeySpace) Down _ _) world = world { lasersPlayer1 = if  (length (player1Lives world))>0 then addLaser (lasersPlayer1 world) (getGameObjectCoordinates (player1 world)) laserGreenImage else (lasersPlayer1 world) }
processEvent (EventKey (SpecialKey KeyEnter) Down _ _) world = world { lasersPlayer2 = if  (length (player2Lives world))>0 then addLaser (lasersPlayer2 world) (getGameObjectCoordinates (player2 world)) laserRedImage else (lasersPlayer2 world)  }


-- drugi igrac
processEvent (EventKey (Char 'a') Down _ _) world = world { player1Left = True }
processEvent (EventKey (Char 'd') Down _ _) world = world { player1Right = True }
processEvent (EventKey (Char 'a') Up _ _) world = world { player1Left = False }
processEvent (EventKey (Char 'd') Up _ _) world = world { player1Right = False }

processEvent (EventKey (Char 'w') Down _ _) world = world { player1Up = True }
processEvent (EventKey (Char 's') Down _ _) world = world { player1Down = True }
processEvent (EventKey (Char 'w') Up _ _) world = world { player1Up = False }
processEvent (EventKey (Char 's') Up _ _) world = world { player1Down = False }

processEvent (EventKey (Char 'r') Down _ _) world = initialWorld
processEvent (EventKey (Char 'p') Down _ _) world = world { gamePaused = newGamePaused }
                                                where
                                                      newGamePaused = not (gamePaused world)

processEvent _ world = world

addLaser :: [GameObject]->(Float,Float)->Picture->[GameObject]
addLaser laserArray (x,y) laserImage = laserArray ++ [(createGameObjectImgObject (x, y+40) (14, 37) laserImage)]

updateCounter :: GameWorld->GameWorld
updateCounter world = world {frameCounter = ((frameCounter world)+1)}


moveObjects :: GameWorld -> GameWorld
moveObjects world = world { player1 = newPlayer1 , player2 = newPlayer2
                          , lasersPlayer1 = newLasersPlayer1, lasersPlayer2 = newLasersPlayer2 , asteroids=newAsteroids
                          , player1Lives=newPlayer1Lives, player2Lives=newPlayer2Lives
                          , pointsPlayer1 = newPointsPlayer1, pointsPlayer2 = newPointsPlayer2
                          , player1Cooldown=newPlayer1Cooldown, player2Cooldown=newPlayer2Cooldown }
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


                p1Cooldown=(player1Cooldown world)
                p1Lives = (player1Lives world)
                (newPlayer1Lives,newPlayer1Cooldown) = if((length p1Lives)>0 && (playerAsteroidCollision (player1 world) (asteroids world)) && (p1Cooldown==0)) then ((init p1Lives),600) else (p1Lives,updatePlayerCooldown p1Cooldown)
                
                p2Cooldown=(player2Cooldown world)
                p2Lives = (player2Lives world)
                (newPlayer2Lives,newPlayer2Cooldown) = if((length p2Lives)>0 && (playerAsteroidCollision (player2 world) (asteroids world)) && (p2Cooldown==0)) then ((init p2Lives),600) else (p2Lives,updatePlayerCooldown p2Cooldown)
   
                p1 =  moveGameObject (player1 world) dx1 dy1                                
                p2 =  moveGameObject (player2 world) dx2 dy2

                newPlayer1 = if (dx1 > 0 || (player1Right world)) then changeGameObjectImage p1 player1RightImg
                             else if (dx1 < 0 || (player1Left world)) then changeGameObjectImage p1 player1LeftImg
                             else changeGameObjectImage p1 player1BasicImg

                newPlayer2 = if (dx2 > 0 || (player2Right world)) then changeGameObjectImage p2 player2RightImg
                             else if (dx2 < 0 || (player2Left world)) then changeGameObjectImage p2 player2LeftImg
                             else changeGameObjectImage p2 player2BasicImg
                
                astep = 2.0

                movedLasersPlayer1 = laserOrAsteroidOutOfBound $ map (\x -> moveGameObject x 0 1.6) (lasersPlayer1 world)
                movedLasersPlayer2 = laserOrAsteroidOutOfBound $ map (\x -> moveGameObject x 0 1.6) (lasersPlayer2 world)
                movedAsteroids = laserOrAsteroidOutOfBound $ map (\x -> moveGameObject x (-0.7 * astep) (-1 * astep)) (asteroids world)

                (newLasersPlayer1, survivedAsteroids) =   laserAsteroidCollision (movedLasersPlayer1, movedAsteroids)
                newPointsPlayer1 = (pointsPlayer1 world) + if (asteroiDifference) < 0 then asteroiDifference + 4
                                                           else asteroiDifference
                                                           where asteroiDifference = length movedAsteroids - length survivedAsteroids


                (newLasersPlayer2, newAsteroids) = laserAsteroidCollision (movedLasersPlayer2, survivedAsteroids)
                newPointsPlayer2 = (pointsPlayer2 world) + if (asteroiDifference) < 0 then asteroiDifference + 4
                                                           else asteroiDifference
                                                           where asteroiDifference = length survivedAsteroids - length newAsteroids
                

createLives (_,_) _ 3 = []
createLives (x,y) playerPicture i = let
                        lives1 = createLives (x,y) playerPicture (i+1)
                in
                        (createGameObjectImgObject (x,y-i*50) (30,50) playerPicture): lives1


--createBothLives :: Float->([GameObject],[GameObject])
--createBothLives 3 = ([],[])
--createBothLives i = 


laserOrAsteroidOutOfBound :: [GameObject] -> [GameObject] 
laserOrAsteroidOutOfBound objects =  filter (\x ->not $ objectOutOfBound x) objects

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
                                                (lasersRemoveId,_) = unzip $ map (fst) collisions
                                                (asteroidsRemoveId, asteroidsRemove) = unzip $ map (snd) collisions
                                                replacedBigAsteroids =  foldr (\asteroid asteroids -> (separateAsteroid asteroid) ++ asteroids ) [] (filter (\x -> fst(getGameObjectSize x)  == 200.0) asteroidsRemove)
                                                                         
                                          in
                                                (map snd (filter (\(x,y) -> notElem x lasersRemoveId) lasersId ),map snd (filter (\(x,y) -> notElem x asteroidsRemoveId) asteroidsId ) ++ replacedBigAsteroids)

updatePlayerCooldown p1Cooldown = if p1Cooldown>0 then p1Cooldown-1 else p1Cooldown

playerAsteroidCollision :: GameObject->[GameObject]->Bool
playerAsteroidCollision player asteroids = length (filter  (\x-> collisionExists player x) asteroids) >0

collisionExists obj1 obj2 = let
                                    (colType,_,_,_,_) = detectCollision  obj1  obj2
                            in
                                    colType /= NoCollision

separateAsteroid :: GameObject -> [GameObject]
separateAsteroid asteroid = [ createGameObjectImgObject (x' + 50 , y' + 50) (80,80) (rotate 15 asteroidSmallImage)
                            , createGameObjectImgObject (x' + 50 , y' - 50) (80,80) (rotate 345 asteroidSmallImage)
                            , createGameObjectImgObject (x' - 50 , y' + 50) (80,80) (rotate 345 asteroidSmallImage)
                            , createGameObjectImgObject (x' - 50 , y' - 50) (80,80) (rotate 15 asteroidSmallImage)
                            ] where (x', y') =  getGameObjectCoordinates asteroid

createAsteroid::Float->GameWorld->GameWorld
createAsteroid sec world = world { asteroids=newAsteroids }
            where
                seed = (frameCounter world) * (ceiling (sec + 1))    
                randX = nextRand seed
                randY = nextRand randX
                x' = randX `mod` 1000 - 400
                y' = randY `mod` 700 - 200 
                randStep = nextRand randY
                step = randStep `mod` 250
                step1 = nextRand step `mod` 300 

                (newAsteroidImg, w', h') = if mod randY 100 > 95 then  (asteroidBigImage, 200, 180)
                                 else (asteroidSmallImage, 80, 80)

                newAsteroids =  if (0 == mod (frameCounter world) step) then  (createGameObjectImgObject ( fromInteger $ toInteger x' , 450)  (w', h')  newAsteroidImg) : (asteroids world)
                                else if (0 == mod (frameCounter world) step1) then (createGameObjectImgObject (600 , fromInteger $ toInteger y' ) (w', h')  newAsteroidImg) : (asteroids world) 
                                else asteroids world

update :: Float -> GameWorld -> GameWorld
update sec world =  if( not (gamePaused world)) then (moveObjects $ updateCounter $ createAsteroid sec world) else world

-- ucitavanje svih slika
player1BasicImg = png "images/greenBasic.png"
player1RightImg = png "images/greenRight.png"
player1LeftImg = png "images/greenLeft.png"

player2BasicImg = png "images/orangeBasic.png"
player2RightImg = png "images/orangeRight.png"
player2LeftImg = png "images/orangeLeft.png"

orangeLife = png "images/orangeLife.png"
greenLife = png "images/greenLife.png"

backgroundImage = png "images/sky.png"
laserRedImage = png "images/laserRed.png"
laserGreenImage = png "images/laserGreen.png"
asteroidBigImage = png "images/asteroidBig.png"
asteroidSmallImage = png "images/asteroidSmall.png"

playerTimeoutImg = png "images/playerTimeout.png"
gameOverImg = png "images/gameOver.png"
gamePausedImg = png "images/gamePaused.png"


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
                            , lasersPlayer1 :: [GameObject]
                            , lasersPlayer2 :: [GameObject]
                            , frameCounter :: Int
                            , pointsPlayer1 :: Int
                            , pointsPlayer2 :: Int
                            , player2Lives :: [GameObject]
                            , player1Lives :: [GameObject]
                            , player1Cooldown :: Int
                            , player2Cooldown :: Int
                            , player1Aura :: GameObject
                            , player2Aura :: GameObject
                            , gamePaused :: Bool
                            }

initialWorld = GameWorld { player1 = createGameObjectImgObject (0, 0) (80, 80) player1BasicImg
                         , player2 = createGameObjectImgObject (0, -310) (80, 80) player2BasicImg
                         , asteroids = []
                         , lasersPlayer1 = []
                         , lasersPlayer2 = []
                         , player1Down = False
                         , player1Up = False
                         , player1Left = False
                         , player1Right = False
                         , player2Down = False
                         , player2Up = False
                         , player2Left = False
                         , player2Right = False
                         , frameCounter = 0
                         , pointsPlayer1 = 0
                         , pointsPlayer2 = 0
                         , player2Lives = createLives (475,310) orangeLife 0
                         , player1Lives = createLives (-475,310) greenLife 0
                         , player1Cooldown = 1000
                         , player2Cooldown = 1000            
                         , player1Aura = createGameObjectImgObject (0, 0) (100, 100) playerTimeoutImg
                         , player2Aura = createGameObjectImgObject (0, 0) (100, 100) playerTimeoutImg
                         , gamePaused = False
                         }
                  --(((createGameObjectImgObject (-475,330-i*40) (30,40) orangeLife): lives1),((createGameObjectImgObject (475,330-i*40) (30,40) greenLife): lives2))

main :: IO ()
main = Game.play
           window
           background
           fps
           initialWorld
           render
           processEvent
           [update]