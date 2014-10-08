import System.Random
import Control.Monad

data Color = Red | Blue | Green | Black
	deriving (Show, Eq)

data HackTree = EmptyNode
	| HackNode { hid :: String, color ::Color, children :: [HackTree]} deriving (Show, Eq)

data GameState = GameState { tree :: HackTree, turn :: Color} deriving (Show)

mor :: [Bool] -> IO Bool
mor lst = return $ or lst

mnor :: [Bool] -> IO Bool
mnor lst = return $ not (or lst)

isMovePossible :: Color -> HackTree -> IO Bool
isMovePossible player EmptyNode = return False
isMovePossible player (HackNode {color = c, children=chil})
	| c == player || c == Green = return True
	| otherwise = mor =<< (sequence (map (isMovePossible player) chil))

rand n = getStdRandom (randomR (0,n))

makeRandomTrees :: String -> Int -> Int -> Int -> (IO [HackTree])
makeRandomTrees label n level maxChildren = sequence [makeRandomTree label i level maxChildren | i <- [1..n]]

makeRandomTree :: String -> Int -> Int -> Int -> (IO HackTree)
makeRandomTree label tid level maxChildren = do
	n <- rand maxChildren
	x <- return $ label ++ (show tid)
	cid <- rand 2
	c <- return $ [Red, Blue, Green] !! cid
	chils <- if level==0 then (return []) else (makeRandomTrees x n (level-1) maxChildren)
	return HackNode {hid=x, color=c, children=chils }

makeRootBlack :: HackTree -> IO HackTree
makeRootBlack EmptyNode = return EmptyNode
makeRootBlack HackNode {color=_, children=chil, hid=h} = return HackNode{color=Black, children=chil, hid=h}

initialGameState = do
	HackNode{children=subtrees} <- makeRandomTree "" 1 3 3
	subtrees' <- sequence (map makeRootBlack subtrees)
	htree <- return HackNode {children=subtrees', hid="1", color=Black}
	return GameState{tree=htree, turn=Red}

rem1edge :: Color -> HackTree -> [HackTree]
rem1edge player root@HackNode{children=chil, color=rc, hid=rid} = do
	x@HackNode{color=c} <- chil
	rest <- return $ filter (\y -> y /= x) chil
	tnr <- return $ [HackNode{color=rc, children=y:rest, hid=rid} | y <- (rem1edge player x)]
	if c==player || c==Green then HackNode{children=rest, color=rc, hid=rid}:tnr else tnr

genStatesFrom :: GameState -> [GameState]
genStatesFrom GameState{tree=htree, turn=player} = [ GameState{tree=t, turn=nextPlayer} | t <- (rem1edge player htree)] where
	nextPlayer = if player == Red then Blue else Red

isWinningState :: GameState -> IO Bool
isWinningState gs@GameState{tree=htree, turn=player} = do
	pos <- isMovePossible player htree
	if not pos then return False else mnor =<< (sequence (map isWinningState (genStatesFrom gs)))

test = do
	state <- initialGameState
	putStrLn $ show state
	isW <- isWinningState state
	putStrLn $ show isW

