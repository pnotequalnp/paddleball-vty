module Main (main) where

import Control.Monad (guard)
import Data.Functor (($>))
import FRP.Yampa
import FRP.Yampa.Vty (Command (..), runVty)
import Graphics.Vty qualified as Vty

main :: IO ()
main = do
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  runVty vty (paddleBall (5 * pi / 3) 0.3)

data Board = Board
  { ball :: !(Float, Float)
  , paddle :: !(Float, Float)
  }

paddleBall :: Float -> Float -> SF (Event Vty.Event) (Event Vty.Picture, Event Command)
paddleBall theta v = proc inp -> do
  dims <- hold (200, 200) -< inp >>= \case
    Vty.EvResize w h -> Event (h, w)
    _ -> NoEvent

  let move = inp >>= \case
        Vty.EvKey Vty.KLeft _ -> Event (-0.05)
        Vty.EvKey Vty.KRight _ -> Event 0.05
        _ -> NoEvent

  rec let board = Board { ball = (ballX, ballY), paddle = (paddleL, paddleR) }
          (paddleHit, image) = render dims board

      ballX <- (+ 0.2) ^<< integral -< xVel
      ballY <- (+ 0.2) ^<< integral -< yVel

      xVel <- accumHold (v * cos theta) -< xCollision $> negate
      yVel <- accumHold (v * sin theta) -< yCollision $> negate

      paddleL <- accumHoldBy (+) 0.4 -< move
      paddleR <- accumHoldBy (+) 0.6 -< move

      hit <- edge -< paddleHit

      xCollision <- edge -< ballX >= 1 || ballX <= 0
      yCollision <- edge -< ballY <= 0 || isEvent hit

  let miss = ballY >= 1
      terminate = guard miss $> Terminate
      display = guard (not miss) $> image

  returnA -< (display, terminate)

ballAttr :: Vty.Attr
ballAttr = Vty.withForeColor Vty.defAttr Vty.red

paddleAttr :: Vty.Attr
paddleAttr = Vty.defAttr

render :: (Int, Int) -> Board -> (Bool, Vty.Picture)
render (height, width) Board {ball = (x, y), paddle = (l, r)} =
  (hit, Vty.picForLayers [ballImage, paddleImage])
  where
    ballX = round (fromIntegral width * x)
    ballY = round (fromIntegral height * y)
    ballImage = Vty.translate ballX ballY (Vty.char ballAttr '⬤')
    paddleL = round (fromIntegral width * l)
    paddleR = round (fromIntegral width * r)
    paddleHeight = floor @Double (fromIntegral height * 0.95)
    paddleImage = Vty.translate paddleL paddleHeight (Vty.charFill paddleAttr '█' (paddleR - paddleL) 1)
    hit = ballY >= paddleHeight && ballX >= paddleL && ballX <= paddleR
