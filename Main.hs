{-# LANGUAGE TemplateHaskell #-}
-- This is the view of the program.
module Main where
import Helm
import qualified Helm.Mouse as Mouse
import qualified Helm.Keyboard as Keyboard
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Cmd as Cmd
import Helm.Graphics2D
import Helm.Color
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time)
import Linear.V2
-- mod' is rational mod
import Control.Lens as Lens
import Debug.Trace

windowDims :: V2 Int
windowDims = V2 800 600

data Action
   = Animate Double
   | Input (Keyboard.Key) deriving (Show)

-- Model

data Direction = West | East deriving (Show)

data Model = Model { _pos :: V2 Double, _vel :: V2 Double, _dir :: Direction } deriving (Show)
makeLenses ''Model

initialMario :: (Model, Cmd SDLEngine a)
initialMario = (Model { _pos = V2 0 0, _vel = V2 0 0, _dir = East }, Cmd.none)

-- Update

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update mario (Animate dt) = (step (dt, Keyboard.UpKey) mario, Cmd.none)
update mario action = ((trace ("action = " ++ show action) mario), Cmd.none)

step :: (Double, Keyboard.Key) -> Model -> Model
step (dt, key) mario = mario
    & gravity dt
    & jump key
    & walk key
    & physics dt


jump :: Keyboard.Key -> Model -> Model
jump key mario = if key == Keyboard.UpKey && mario ^. vel . _y == 0.0
    then mario & vel . _y .~ 6
    else mario

gravity :: Double -> Model -> Model
gravity dt mario = if mario ^. pos . _y > 0
    then mario & vel . _y -~ dt/4
    else mario & vel . _y .~ 0

physics :: Double -> Model -> Model
physics dt mario = mario
    & pos . _x +~ dt * (mario ^. vel ^. _x)
    & pos . _y .~ max 0 (mario ^. pos ^. _y + dt * mario ^. vel ._y)

walk :: Keyboard.Key -> Model -> Model
walk key mario = mario

-- Display

drawMario :: Model -> Form SDLEngine
drawMario mario = move (mario ^. pos + fmap fromIntegral windowDims / 2) $ filled (rgb 1 0 0) $ square 100

display :: Model -> Graphics SDLEngine
display mario = Graphics2D $ collage $ [drawMario mario]

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.fps 60 $ Animate, Keyboard.presses Input ]

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine defaultConfig GameLifecycle
    { initialFn       = initialMario
    , updateFn        = Main.update
    , subscriptionsFn = subscriptions
    , viewFn          = display
    }
