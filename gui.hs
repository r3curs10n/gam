import qualified Graphics.UI.GLUT as G
import Game
import Data.IORef

testTree = HackNode{ hid="1", color=Red, children=[HackNode{hid="11", color=Green, children=[HackNode{hid="11", color=Blue, children=[]}, HackNode{hid="12", color=Red, children=[]}]}, HackNode{hid="12", color=Red, children=[HackNode{hid="11", color=Blue, children=[]}, HackNode{hid="12", color=Red, children=[]}]}] }
testTree2 = HackNode{ hid="1", color=Red, children=[HackNode{hid="11", color=Green, children=[HackNode{hid="11", color=Green, children=[]}, HackNode{hid="12", color=Red, children=[]}]}, HackNode{hid="12", color=Red, children=[HackNode{hid="11", color=Blue, children=[]}, HackNode{hid="12", color=Red, children=[]}]}] }

data InputState = InputState{input :: [Char], xoff :: G.GLfloat}

drawTree :: HackTree -> G.GLfloat -> G.DisplayCallback
drawTree htree offset = drawTree' 5 (-0.8-offset) (-0.8) (-0.8-offset) (-0.8) htree where
	drawTree' height px py ox oy HackNode{hid=rid, color=col, children=chil} = do
		--drawBox ox oy 0.05 0.05
		drawLine col px py ox oy
		drawText rid ox oy
		drawTrees (height-1) ox oy ox (oy+0.15) chil
	drawTrees height px py ox oy (x:xs) = do
		drawTree' height px py ox oy x
		drawTrees height px py nx oy xs where
			nx = ox + ((2**height)/20)
	drawTrees height px py ox oy [] = return ()

drawText str x y = do
	G.currentRasterPosition G.$= G.Vertex4 x y 0 1
	G.renderString G.Fixed8By13 str

initialInpState = InputState{input=[], xoff=0}

main :: IO ()
main = do
	(_progName, _args) <- G.getArgsAndInitialize
	_window <- G.createWindow "Hello World"
	igs <- initialGameState
	gs <- newIORef igs
	inp <- newIORef initialInpState
	G.displayCallback G.$= (display gs inp)
	G.keyboardCallback G.$= Just (keyPress gs inp)
	G.specialCallback G.$= Just (keySpPress gs inp)
	G.reshapeCallback G.$= Just reshape
	G.mainLoop

drawBox :: G.GLfloat -> G.GLfloat -> G.GLfloat -> G.GLfloat -> G.DisplayCallback
drawBox cx cy w h = G.renderPrimitive G.LineLoop $ mapM_ (\(x,y) -> G.vertex $ G.Vertex3 x y 0) corners where
	corners = [(cx-w, cy-h), (cx+w, cy-h), (cx+w, cy+h), (cx-w, cy+h)]

getRGB Red = (1,0,0)
getRGB Blue = (0,0,1)
getRGB Green = (0,1,0)
getRGB Black = (0,0,0)

color3f r g b = G.color $ G.Color3 r g (b :: G.GLfloat)
vertex3f x y z = G.vertex $ G.Vertex3 x y (z :: G.GLfloat)

drawLine :: Color -> G.GLfloat -> G.GLfloat -> G.GLfloat -> G.GLfloat -> G.DisplayCallback
drawLine c x0 y0 x1 y1 = G.renderPrimitive G.Lines $ do
	color3f r g b
	vertex3f x0 y0 0
	vertex3f x1 y1 0
	color3f 1 1 1 where (r,g,b) = getRGB c

reshape :: G.ReshapeCallback
reshape size = do
	G.viewport G.$= (G.Position 0 0, size)
	G.postRedisplay Nothing

display gs inp = do
	cgs@GameState{tree=htree, turn=player} <- G.get gs
	InputState{xoff=x} <- G.get inp
	G.clear [G.ColorBuffer]
	if not (isMovePossible cgs) then drawText ("Game Over. "++ (msg player) ) 0 0 else return ()
	drawTree htree x
	G.flush where
		msg pl = if pl==Red then "You Lose!" else "You Win!"

keyPress gs inp 'n' _ = do
	inp G.$= initialInpState
	newgame <- initialGameState
	gs G.$= newgame
	G.postRedisplay Nothing

keyPress gs inp 'a' _ = do
	InputState{input=uinp, xoff=x} <- G.get inp
	inp G.$= InputState{input=uinp, xoff=x-0.4}
	G.postRedisplay Nothing

keyPress gs inp 'd' _ = do
	InputState{input=uinp, xoff=x} <- G.get inp
	inp G.$= InputState{input=uinp, xoff=x+0.4}
	G.postRedisplay Nothing

keyPress _ inp key _ = do
	InputState{input=uinp, xoff=x} <- G.get inp
	inp G.$= InputState{input=uinp++[key], xoff=x}
	drawText [key] 0 0
	G.flush
	G.postRedisplay Nothing

keySpPress gs inp key pos = do
	cgs@GameState{tree=htree, turn=player} <- G.get gs
	InputState{input=uinp, xoff=x} <- G.get inp
	igs <- return $ GameState{tree=remEdge uinp htree, turn=Blue}
	fgs@GameState{tree=ht} <- return $ if isMovePossible igs then aiMove igs else igs
	gs G.$= fgs
	inp G.$= InputState{input=[], xoff=x}
	G.postRedisplay Nothing
