module Main (main) where

import Control.Monad (guard)
import Data.Functor (($>))
import FRP.Yampa
import FRP.Yampa.Vty (Command (..), runVty)
import Graphics.Vty qualified as Vty
import System.Random (randomRIO)
import Control.Applicative ((<|>))

main :: IO ()
main = do
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  x <- randomRIO (0.2, 0.8)
  y <- randomRIO (0.2, 0.8)
  theta <- randomRIO (4 * pi / 3, 5 * pi / 3)
  runVty vty 60 (paddleBall (x, y) theta 0.3)

data Board = Board
  { ball :: !(Float, Float)
  , paddle :: !(Float, Float)
  }

paddleBall ::
  -- | Initial ball position
  (Float, Float) ->
  -- | Initial ball angle
  Float ->
  -- | Initial ball velocity
  Float ->
  -- | Paddleball game
  SF (Event Vty.Event) (Event Vty.Picture, Event Command)
paddleBall (x, y) theta v = proc inp -> do
  dims <- hold (200, 200) -< inp >>= \case
    Vty.EvResize w h -> Event (h, w)
    _ -> NoEvent

  let move = inp >>= \case
        Vty.EvKey Vty.KLeft _ -> Event (-0.05)
        Vty.EvKey Vty.KRight _ -> Event 0.05
        _ -> NoEvent

  rec let board = Board { ball = (ballX, ballY), paddle = (paddleL, paddleR) }
          paddleImpactTheta = paddleImpactAngle $ (ballX - paddleL)  / (paddleR - paddleL)

      ballX <- (+ x) ^<< integral -< xVel
      ballY <- (+ y) ^<< integral -< yVel

      xVel <- accumHold (v * cos theta) -< wallCollision $> negate
                                       <|> paddleCollision $> const (v * cos paddleImpactTheta)
      yVel <- accumHold (v * sin theta) -< ceilingCollision $> negate
                                       <|> paddleCollision $> const (v * sin paddleImpactTheta)

      paddleL <- accumHoldBy (+) 0.4 -< move
      paddleR <- accumHoldBy (+) 0.6 -< move

      hit <- edge -< ballX >= paddleL && ballX <= paddleR && ballY >= 0.95

      wallCollision <- edge -< ballX >= 1 || ballX <= 0
      ceilingCollision <- edge -< ballY <= 0
      paddleCollision <- edge -< isEvent hit

  let miss = ballY >= 1
      terminate = guard miss $> Terminate
      display = guard (not miss) $> image
      image = render dims board

  returnA -< (display, terminate)

ballAttr :: Vty.Attr
ballAttr = Vty.withForeColor Vty.defAttr Vty.red

paddleAttr :: Vty.Attr
paddleAttr = Vty.defAttr

paddleImpactAngle :: Float -> Float
paddleImpactAngle x = (0.8 * (x - 0.5) + 1.5) * pi

render :: (Int, Int) -> Board -> Vty.Picture
render (height, width) Board {ball = (x, y), paddle = (l, r)} = Vty.picForLayers [ballImage, paddleImage]
  where
    ballX = round (fromIntegral width * x)
    ballY = round (fromIntegral height * y)
    ballImage = Vty.translate ballX ballY (Vty.char ballAttr '⬤')
    paddleL = round (fromIntegral width * l)
    paddleR = round (fromIntegral width * r)
    paddleHeight = ceiling @Double (fromIntegral height * 0.95)
    paddleImage = Vty.translate paddleL paddleHeight (Vty.charFill paddleAttr '█' (paddleR - paddleL) 1)
