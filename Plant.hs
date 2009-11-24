module Plant where

import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators (Vec2)
import qualified Graphics.UI.SDL as SDL
import Data.Monoid (Monoid(..))
import Control.Monad (when)

type Drawing = Draw.Draw ()

data DNA
    = Branch DNA
    | Junction DNA DNA
    | Shorten DNA
    | Leaf

sJunction x = Junction x x

render :: DNA -> Drawing
render (Branch a) = Draw.line (0,0) (0,1) `mappend` Draw.translate (0,1) (render a)
render (Junction l r) = 
    mconcat [
        Draw.rotate (-deg2rad 20) $ render r,
        Draw.rotate (deg2rad 20) $ render l
    ]
render (Shorten d) = Draw.scale 0.8 0.8 (render d)
render Leaf = Draw.color (0,1,0,1) . Draw.scale 0.2 0.2 $ Draw.circle

(x,y) ^+^ (x',y') = (x + x', y + y')

deg2rad :: Double -> Double
deg2rad x = x * pi / 180

viewport :: Drawing -> Drawing
viewport = Draw.translate (0,-1) . Draw.scale 0.5 0.5

treen :: Int -> DNA
treen 0 = Leaf
treen n = let b = treen (n-1) in Branch (Junction b b)

initialize :: IO ()
initialize = do
    SDL.init [SDL.InitVideo]
    SDL.setVideoMode 640 480 32 [SDL.OpenGL]
    return ()

drawDNA :: DNA -> IO ()
drawDNA tree = do
    wasInit <- SDL.wasInit []
    when (null wasInit) initialize
    Draw.draw . viewport $ render tree
    SDL.glSwapBuffers
