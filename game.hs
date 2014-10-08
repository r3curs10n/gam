import System.Random
import Control.Monad

data Color = Red | Blue | Green | Black
	deriving (Show, Eq)

data HackTree = EmptyNode
	| HackNode { hid :: String, color ::Color, children :: [HackTree]} deriving (Show, Eq)

data GameState = GameState { tree :: HackTree, turn :: Color} deriving (Show)

isMovePossible :: GameState-> Bool
isMovePossible GameState{turn=player, tree=HackNode{color=c, children=chil}}
	| c == player || c == Green = True
	| otherwise = or [ isMovePossible GameState{turn=player, tree=c} | c <- chil]

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

rem1edge :: Color -> HackTree -> [HackTree]
rem1edge player root@HackNode{children=chil, color=rc, hid=rid} = do
	x@HackNode{color=c} <- chil
	rest <- return $ filter (\y -> y /= x) chil
	tnr <- return $ [HackNode{color=rc, children=y:rest, hid=rid} | y <- (rem1edge player x)]
	if c==player || c==Green then HackNode{children=rest, color=rc, hid=rid}:tnr else tnr

genStatesFrom :: GameState -> [GameState]
genStatesFrom GameState{tree=htree, turn=player} = [ GameState{tree=t, turn=nextPlayer} | t <- (rem1edge player htree)] where
	nextPlayer = if player == Red then Blue else Red

isWinningState :: GameState -> Bool
isWinningState gs@GameState{tree=htree, turn=player}
	| not $ isMovePossible gs = False
	| otherwise = not $ or (map isWinningState (genStatesFrom gs))

remEdge :: String -> HackTree -> HackTree
remEdge label HackNode{children=chil, color=rc, hid=rid} = HackNode{children=nc, color=rc, hid=rid} where
	nc = [remEdge label c | c@HackNode{hid=x} <- chil, x /= label]

aiMove :: GameState -> GameState
aiMove gs@GameState{tree=htree, turn=player} = if length winningStates == 0 then head allStates else head winningStates where
	winningStates = filter (\x -> not (isWinningState x)) allStates
	allStates = genStatesFrom gs

test = do
	state <- initialGameState
	putStrLn $ show state
	isW <- return $ isWinningState state
	putStrLn $ show isW

play :: GameState -> IO ()
play gs@GameState{turn=player, tree=htree} = do
	putStrLn $ show gs
	if not (isMovePossible gs) then gameOver else
		if player == Red then userPlays else aiPlays where
			userPlays = do
				putStrLn "Your move:"
				e <- getLine
				play GameState{turn=nextPlayer, tree=remEdge e htree}
			aiPlays = do
				play $ aiMove gs
			nextPlayer = if player == Red then Blue else Red
			gameOver = do
				putStrLn "Game Over!"

main = do
	state <- initialGameState
	play state

initialGameState = do
	HackNode{children=subtrees} <- makeRandomTree "" 1 2 2
	subtrees' <- sequence (map makeRootBlack subtrees)
	htree <- return HackNode {children=subtrees', hid="1", color=Black}
	return GameState{tree=htree, turn=Red}