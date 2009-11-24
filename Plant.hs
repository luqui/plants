module Plant where

import Prelude hiding (or)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Data.Monoid (Monoid(..), First(..))
import Control.Monad.Instances ()

type Drawing = Draw.Draw ()

data Plant
    = PBranch Plant
    | PJunction Plant Plant
    | PLeaf
    | PStub (PlantZ -> Maybe Plant)

data DPlant
    = DPBranch
    | DPJunctionL Plant
    | DPJunctionR Plant

type PlantZ = (Plant, [DPlant])
type Motion = PlantZ -> Maybe PlantZ

or :: Maybe a -> Maybe a -> Maybe a
or (Just x) _ = Just x
or Nothing  y = y

choice = foldr or Nothing

zUp :: Motion
zUp (p, DPBranch:cs) = Just (PBranch p, cs)
zUp (p, DPJunctionL p':cs) = Just (PJunction p p', cs)
zUp (p, DPJunctionR p':cs) = Just (PJunction p' p, cs)
zUp (p, []) = Nothing

zUpBranch :: Motion
zUpBranch (p, DPBranch:cs) = Just (PBranch p, cs)
zUpBranch _ = Nothing

zUpJunctionL :: Motion
zUpJunctionL (p, DPJunctionL p':cs) = Just (PJunction p p', cs)
zUpJunctionL _ = Nothing

zUpJunctionR :: Motion
zUpJunctionR (p, DPJunctionR p':cs) = Just (PJunction p' p, cs)
zUpJunctionR _ = Nothing

zDownBranch :: Motion
zDownBranch (PBranch p, cs) = Just (p, DPBranch : cs)
zDownBranch _ = Nothing

zDownJunctionL :: Motion
zDownJunctionL (PJunction a b, cs) = Just (a, DPJunctionL b : cs)
zDownJunctionL _ = Nothing

zDownJunctionR :: Motion
zDownJunctionR (PJunction a b, cs) = Just (b, DPJunctionR a : cs)
zDownJunctionR _ = Nothing

zStub :: PlantZ -> Maybe ()
zStub (PStub _, cs) = Just ()
zStub _ = Nothing

zLeaf :: PlantZ -> Maybe ()
zLeaf (PLeaf, cs) = Just ()
zLeaf _ = Nothing

zRoot :: PlantZ -> Maybe ()
zRoot (_, []) = Just ()
zRoot _ = Nothing

infixl 0 $>
($>) :: (Functor f) => f b -> a -> f a
f $> x = fmap (const x) f


step :: Plant -> Plant
step = go []
    where
    go cx (PBranch p) = PBranch (go (DPBranch : cx) p)
    go cx (PJunction p q) = PJunction (go (DPJunctionL q : cx) p) (go (DPJunctionR p : cx) q)
    go _ PLeaf = PLeaf
    go cx self@(PStub pat) | Just x <- pat (self,cx) = x
                           | otherwise               = self

render :: Plant -> Drawing
render (PBranch p)     = Draw.line (0,0) (0,1) `mappend` Draw.translate (0,1) (render p)
render (PJunction p q) = Draw.rotate (-deg2rad 20) (render p) `mappend` Draw.rotate (deg2rad 20) (render q)
render PLeaf           = Draw.color (0,1,0,1) . Draw.scale 0.2 0.2 $ Draw.circle
render (PStub _)       = Draw.color (1,0.5,0,1) . Draw.scale 0.2 0.2 $ Draw.circle

deg2rad deg = pi / 180 * deg


viewport :: Drawing -> Drawing
viewport = Draw.translate (0,-1) . Draw.scale 0.25 0.25

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
