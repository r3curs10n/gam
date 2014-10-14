import qualified Graphics.UI.GLUT as G
 
data Color = Red | Blue | Green | Black
	deriving (Show, Eq)

data HackTree = EmptyNode
	| HackNode { hid :: String, color ::Color, children :: [HackTree]} deriving (Show, Eq)

data GameState = GameState { tree :: HackTree, turn :: Color} deriving (Show)

testTree = HackNode{ hid="1", color=Red, children=[HackNode{hid="11", color=Blue, children=[HackNode{hid="11", color=Blue, children=[]}, HackNode{hid="12", color=Red, children=[]}]}, HackNode{hid="12", color=Red, children=[HackNode{hid="11", color=Blue, children=[]}, HackNode{hid="12", color=Red, children=[]}]}] }

drawTree :: HackTree -> G.DisplayCallback
drawTree htree = drawTree' 4 (-0.8) (-0.8) (-0.8) (-0.8) htree where
	drawTree' height px py ox oy HackNode{children=chil} = do
		drawBox ox oy 0.05 0.05
		drawLine px py ox oy
		drawTrees (height-1) ox oy ox (oy+0.15) chil
	drawTrees height px py ox oy (x:xs) = do
		drawTree' height px py ox oy x
		drawTrees height px py nx oy xs where
			nx = ox + ((2**height)/20)
	drawTrees height px py ox oy [] = drawBox 0 0 0 0

main :: IO ()
main = do
	(_progName, _args) <- G.getArgsAndInitialize
	_window <- G.createWindow "Hello World"
	G.displayCallback G.$= display
	G.reshapeCallback G.$= Just reshape
	G.mainLoop

drawBox :: G.GLfloat -> G.GLfloat -> G.GLfloat -> G.GLfloat -> G.DisplayCallback
drawBox cx cy w h = G.renderPrimitive G.LineLoop $ mapM_ (\(x,y) -> G.vertex $ G.Vertex3 x y 0) corners where
	corners = [(cx-w, cy-h), (cx+w, cy-h), (cx+w, cy+h), (cx-w, cy+h)]

drawLine :: G.GLfloat -> G.GLfloat -> G.GLfloat -> G.GLfloat -> G.DisplayCallback
drawLine x0 y0 x1 y1 = G.renderPrimitive G.Lines $ mapM_ (\(x,y) -> G.vertex $ G.Vertex3 x y 0) [(x0,y0),(x1,y1)]

reshape :: G.ReshapeCallback
reshape size = do
	G.viewport G.$= (G.Position 0 0, size)
	G.postRedisplay Nothing

display :: G.DisplayCallback
display = do 
	G.clear [G.ColorBuffer]
	drawTree testTree
	G.flush