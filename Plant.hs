module Plant where

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Data.Monoid (Monoid(..))

type Drawing = Draw.Draw ()

data Plant
    = PBranch Plant
    | PJunction Plant Plant
    | PLeaf
    | PStub (Pattern Plant)

data DPlant
    = DPBranch
    | DPJunctionL Plant
    | DPJunctionR Plant

type Pattern a = [DPlant] -> Maybe a

step :: Plant -> Plant
step = go []
    where
    go cx (PBranch p) = PBranch (go (DPBranch : cx) p)
    go cx (PJunction p q) = PJunction (go (DPJunctionL q : cx) p) (go (DPJunctionR p : cx) q)
    go _ PLeaf = PLeaf
    go cx (PStub pat) | Just x <- pat cx = x
                      | otherwise        = PStub pat

render :: Plant -> Drawing
render (PBranch p)     = Draw.line (0,0) (0,1) `mappend` Draw.translate (0,1) (render p)
render (PJunction p q) = Draw.rotate (-deg2rad 20) (render p) `mappend` Draw.rotate (deg2rad 20) (render q)
render PLeaf           = Draw.color (0,1,0,1) . Draw.scale 0.2 0.2 $ Draw.circle
render (PStub _)       = Draw.color (1,0.5,0,1) . Draw.scale 0.2 0.2 $ Draw.circle

deg2rad deg = pi / 180 * deg


viewport :: Drawing -> Drawing
viewport = Draw.translate (0,-1) . Draw.scale 0.5 0.5

drawDNA :: Plant -> IO ()
drawDNA tree = do
    go tree
  where
    go t = do
        SDL.init [SDL.InitVideo]
        SDL.setVideoMode 640 480 32 [SDL.OpenGL]
        Draw.draw . viewport $ render t
        SDL.glSwapBuffers
        cmd <- getCmd
        SDL.quit
        case cmd of
            False  -> return ()
            True   -> go (step t)

-- true = continue
-- false = quit
getCmd :: IO Bool
getCmd = do
    ev <- SDL.waitEvent
    case ev of
        SDL.KeyDown (SDL.Keysym { SDL.symKey = SDL.SDLK_SPACE }) -> return True
        SDL.KeyDown (SDL.Keysym { SDL.symKey = SDL.SDLK_ESCAPE }) -> return False
        _ -> getCmd
