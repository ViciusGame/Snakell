{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main (main) where

import Apecs.Physics
--import Apecs
import Apecs.Physics.Gloss
--import Control.Concurrent
--import Control.Monad
--import Linear
import System.Random

data Direction = U | D | L | R deriving (Show, Eq)

data Kinematics = Kinematics (V2 Float) Direction deriving Show

-- Cobra
data Snake = Head Kinematics | Body Kinematics Snake deriving Show
instance Component Snake where type Storage Snake = Map Snake

-- Fruta
newtype Food = Food (V2 Float) deriving Show
instance Component Food where type Storage Food = Map Food

-- Score
newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

-- Funções Auxiliares
opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

reflected :: Direction -> Direction -> Bool
reflected U D = True
reflected D U = True
reflected L R = True
reflected R L = True
reflected _ _ = False

-- Constantes
worldSize :: Float
worldSize = 500

snakeBodySize :: Integer
snakeBodySize = 20

makeWorld "World" [''Snake, ''Camera, ''Food, ''Score]

window :: Display
window = InWindow "SNAKE CLASSIC" (600, 600) (10000, 10)

initKinematics :: Kinematics
initKinematics = Kinematics (V2 (-20) 0) U

--Arte
snakePicture :: Picture
snakePicture = pictures [(color $ dark green) . Polygon $ rectanglePath (fromInteger snakeBodySize) (fromInteger snakeBodySize),
                         translate 0 5 $ color green . Polygon $ rectanglePath (fromInteger snakeBodySize) (fromInteger snakeBodySize/2),
                         color (withAlpha 0.05 black) . lineLoop $ rectanglePath (fromInteger snakeBodySize) (fromInteger snakeBodySize)]

drawSnake :: Snake -> Picture
drawSnake (Head (Kinematics (V2 x y) _)) = pictures [translate x y snakePicture]
drawSnake (Body (Kinematics (V2 x y) _) rest) = translate x y snakePicture <> drawSnake rest

foodPicture :: Picture
foodPicture = pictures [translate 0 10 $ color black . Polygon $ rectanglePath 2 5,
                        color red . circleSolid $ 7,
                        color black . circle $ 7]

drawFood :: Food -> Picture
drawFood (Food (V2 x y)) = translate x y foodPicture

render :: System World Picture
render = do
  let boundary = color black . lineLoop $ rectanglePath worldSize worldSize
  snake <- foldDraw drawSnake
  f1 <- foldDraw drawFood

  Score s <- get global
  let score = pictures [(color black . translate (-160) (260) . Polygon $ rectanglePath 100 20),
                        (color white . translate (-200) (255) . Scale 0.1 0.1 . Text $ "Score: " ++ show s)]
  return $ boundary <> snake <> f1 <> score

-- Controles de Jogo
changeSnakesDirection :: Direction -> Snake -> Snake
changeSnakesDirection dir = snd . changeSnakesDirectionInner dir
  where
    changeSnakesDirectionInner d (Head (Kinematics pos oldDirection)) =
      let newDirection = if reflected oldDirection d then oldDirection else d in (oldDirection, Head (Kinematics pos newDirection))
    changeSnakesDirectionInner d (Body (Kinematics pos oldDirection) rest) =
      let movedSnake = changeSnakesDirectionInner d rest in (oldDirection, Body (Kinematics pos $ fst movedSnake) $ snd movedSnake)

eventHandler :: Event -> System World ()
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) = cmap $ changeSnakesDirection L
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) = cmap $ changeSnakesDirection R
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) = cmap $ changeSnakesDirection D
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) = cmap $ changeSnakesDirection U
eventHandler _ = pure ()

addPoints :: (Food, Snake) -> System World ()
addPoints (Food (V2 x y), snake) =
    case snake of
      Head (Kinematics (V2 x' y') _) -> if abs(x - x') <= 20 && abs(y - y') <= 20
            then modify global $ \(Score s) -> Score (s + 100)
            else modify global $ \(Score s) -> Score (s + 1)
      Body _ _ -> modify global $ \(Score s) -> Score (s + 1)

dead :: Snake -> Snake
dead s = deadInner s
  where
    deadInner (Head (Kinematics pos _)) = if ateSelf pos s then Head initKinematics else s
    deadInner (Body _ rest) = deadInner rest

    ateSelf _ (Head _) = False
    ateSelf pos@(V2 x y) (Body (Kinematics (V2 x' y') _) rest)
      | x' == x && y' == y = True
      | otherwise = ateSelf pos rest

displaceCoordinate :: Direction -> Float -> Float
displaceCoordinate d c = if factor * newC >= worldSize/2 then newC - factor * worldSize else newC
  where
    newC = c + factor * fromInteger snakeBodySize
    factor = case d of
                  U -> 1
                  R -> 1
                  L -> -1
                  D -> -1

displace :: Direction -> V2 Float -> V2 Float
displace d (V2 x y)
  | d == U || d == D = V2 x $ displaceCoordinate d y
  | otherwise = V2 (displaceCoordinate d x) y

displaceSnake :: Snake -> Snake
displaceSnake = snd . displaceSnakeInner
  where
    displaceSnakeInner (Head (Kinematics pos d)) = (pos, Head (Kinematics (displace d pos) d))
    displaceSnakeInner (Body (Kinematics pos d) rest) =
      let movedSnake = displaceSnakeInner rest in (pos, Body (Kinematics (fst movedSnake) d) $ snd movedSnake)

tailKinematics :: Snake -> Kinematics
tailKinematics (Head (Kinematics pos d)) = Kinematics (displace (opposite d) pos) d
tailKinematics (Body (Kinematics pos d) _) = Kinematics pos d

addBody :: Food -> Food -> Snake -> (Food, Snake)
addBody food newF snake = addBodyInner food newF snake
  where
    addBodyInner :: Food -> Food -> Snake -> (Food, Snake)
    addBodyInner f@(Food (V2 x y)) newFood partialSnake =
      case partialSnake of
        (Head (Kinematics (V2 x' y') _)) -> if x == x' && y == y'
          then (newFood, Body (tailKinematics snake) snake)
          else (f, snake)
        (Body _ rest) -> addBodyInner f newFood rest

-- Simula cada tick que ocorre
timeStep :: Float -> System World ()
timeStep _ = do
  pos <- randomV2
  cmapM $ \(f, s) -> addPoints (f, s)
  cmap $ \(f, s) -> (addBody f (Food pos) . displaceSnake . dead $ s)

-- Gerador de Coordenadas
randomV2 :: System World (V2 Float)
randomV2 = do
  x <- randomInt
  y <- randomInt
  pure $ V2 (constrainFloatToWorld x) (constrainFloatToWorld y)
    where
      constrainFloatToWorld = fromInteger . (*snakeBodySize) . flip div snakeBodySize
      bounds = (-240, 240)
      randomInt = liftIO . randomRIO $ bounds

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    pos <- randomV2
    _ <- newEntity (Food pos, Head initKinematics)
    play window (light blue) 10 render eventHandler timeStep
