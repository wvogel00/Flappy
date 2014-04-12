{-# LANGUAGE ImplicitParams #-}

import FreeGame
import Control.Applicative

winWIDTH = 480
winHEIGHT = 520
birdX = 80

type Poll = (Bool,Vec2)

data Lane = Z|X|C|V deriving Eq
data GameState = Opening | Playing | GameOver | Ranking deriving (Show,Eq)
data Game = Game{
    state :: GameState,
    points :: Int,
    pos :: Vec2,
    jumpPower :: Double,
    gravitiy :: Double,
    polls :: [Poll],
    font :: Font,
    startBtn :: Bitmap,
    rankBtn :: Bitmap,
    background :: Bitmap,
    bird :: Bitmap,
    upPoll :: Bitmap,
    underPoll :: Bitmap
    }

initialize font start rank back bird uppoll underpoll = return $ Game {
    state = Opening,
    points = 0,
    pos = V2 birdX 260,
    jumpPower = 0,
    gravitiy = 3,
    polls = [(False,V2 480 200)],
    font = font,
    startBtn = start,
    rankBtn = rank,
    background = back,
    bird = bird,
    upPoll = uppoll,
    underPoll = underpoll
    }

main = do
    font <- loadFont "JNRfont_s.ttf"
    uppoll <- readBitmap "poll.png"
    underpoll <- readBitmap "underpoll.png"
    startBtn <- readBitmap "start.png"
    rankBtn <- readBitmap "rank.png"
    bird <- readBitmap "bird.png"
    background <- readBitmap "background.png"
    game <- initialize font startBtn rankBtn background bird uppoll underpoll
    runGame Windowed (BoundingBox 0 0 winWIDTH winHEIGHT) $ do
        setTitle "Flappy Bird"
        clearColor black
        mainloop game
    
mainloop game = do
    translate (V2 240 260) $ bitmap (background game)
    case state game of
        Opening -> do
            drawButtons game
            translate (pos game) $ bitmap (bird game)
            next <- seeInput Opening game
            debug (font game).show $ state next
            tick
            mainloop next
        Playing -> do
            translate (pos game) $ bitmap (bird game)
            drawPolls game
            next' <- seeInput Playing game
            let y = nextPos (pos game) (jumpPower next') (gravitiy game)
            debug (font game) $ show (gravitiy game)
            showPoints (font game) $ points game
            next <- update $ game {pos = y, jumpPower = nextJump (jumpPower next')}
            tick
            mainloop $ next --game {pos = y, jumpPower = nextJump (jumpPower next')}
        GameOver -> do
            --drawFrame
            translate (V2 240 80) $ bitmap (bird game)
            tick
            mainloop game
        

drawButtons game = do
    translate (V2 180 400) $ bitmap (startBtn game)
    translate (V2 300 400) $ bitmap (rankBtn game)
    
drawPolls game = mapM_ (drawPoll (upPoll game) (underPoll game)) $ map snd $ polls game

drawPoll up under pos = do
    translate (moveY pos (-220)) $ bitmap up
    translate (moveY pos (220)) $ bitmap under
    
    
seeInput Opening game = do
    pos <- mousePosition
    t <- mouseUpL
    return $ if t && inStartBtn pos then game {state = Playing} else game
    
seeInput Playing game = do
    mUp <- mouseUpL
    key <- keyUp KeySpace
    return $ if mUp || key then game {jumpPower = jumpPower game - 17,gravitiy = 0} else game
    
inStartBtn (V2 x y) = 135 <= x && x <= 225 && 376 <= y && y <= 424

nextJump power = if power < 0 then power + 2 else 0


--update functions
update game = do
    r <- randomness (100,350)
    let (cnt,polls') = passPoll (polls game)
    return $ game{
        points = points game + cnt, 
        polls = deletePoll.movePolls $ nextPolls r polls',
        gravitiy = nextGravity (gravitiy game) (jumpPower game)}        

nextPos :: Vec2 -> Double -> Double -> Vec2
nextPos pos pwr g
    | pwr < 0 = moveY pos pwr
    | 0 == pwr = moveY pos g
    | otherwise = pos

nextGravity g pwr
    | pwr == 0 && g == 0 = 1
    | pwr == 0 = min 5 $ (g+0.06)
    | pwr < 0 = 0
    | 0 < pwr = g
    
deletePoll = filter ((negate 50<).posX.snd)
movePolls = map (\(flag,pos) -> (flag,moveX pos (-1.8)))
nextPolls r ps = ps ++ if posX (snd $ last ps) < 240 then [(False,V2 520 r)] else []    
passPoll = foldr isPassed (0,[]) where
    isPassed p (n,ps)
        | fst p = (n,p:ps)
        | posX (snd p) <= birdX = (n+1,(True,snd p):ps)
        | otherwise = (n,p:ps)

--operate the position of objects
posX (V2 x _) = x
moveX (V2 x y) v = V2 (x+v) y
moveY (V2 x y) v = V2 x (y+v)
(V2 x y) .+. (V2 x' y') = V2 (x+x') (y+y')
--show data with text
debug font str = translate (V2 40 40)$ color black $ text font 25 str
showPoints font pnt = translate (V2 (winWIDTH/2) 40)$ color white $ text font 40 $ show pnt