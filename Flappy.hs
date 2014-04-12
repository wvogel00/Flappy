{-# LANGUAGE ImplicitParams #-}

import FreeGame
import Control.Applicative
import System.Directory

winWIDTH = 480
winHEIGHT = 520
birdX = 80

type Poll = (Bool,Vec2)

data Lane = Z|X|C|V deriving Eq
data GameState = Opening | Playing | GameOver | Ranking deriving (Show,Eq)
data Game = Game{
    state :: GameState,
    points :: Int,
    highscore :: Int,
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
    underPoll :: Bitmap,
    scoreBoard :: Bitmap,
    medal :: Bitmap
    }

initialize font start rank back bird uppoll underpoll scoreboard medal highscore = return $ Game {
    state = Opening,
    points = 0,
    highscore = highscore,
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
    underPoll = underpoll,
    scoreBoard = scoreboard,
    medal = medal
    }

main = do
    font <- loadFont "JNRfont_s.ttf"
    uppoll <- readBitmap "poll.png"
    underpoll <- readBitmap "underpoll.png"
    startBtn <- readBitmap "start.png"
    rankBtn <- readBitmap "rank.png"
    bird <- readBitmap "bird.png"
    background <- readBitmap "background.png"
    scoreboard <- readBitmap "scoreboard.png"
    medal <- readBitmap "medal.png"
    highscore <- read <$> readFile "score"
    game <- initialize font startBtn rankBtn background bird uppoll underpoll scoreboard medal highscore
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
            tick
            mainloop next
        Playing -> do
            drawPolls game
            translate (pos game) $ bitmap (bird game)
            next' <- seeInput Playing game
            let y = nextPos (pos game) (jumpPower next') (gravitiy game)
            showPoints (font game) $ points game
            next <- update Playing $ game {pos = y, jumpPower = nextJump (jumpPower next')}
            tick
            mainloop $ next
        GameOver -> do
            drawPolls game
            translate (pos game) $ bitmap (bird game)
            translate (V2 (winWIDTH/2-100) (winHEIGHT/2-80)) $ color cyan $ text (font game) 40 "Game Over"
            drawScore game
            next' <- update GameOver game
            next <- seeInput GameOver next'
            tick
            mainloop next
        
--draw objects
drawButtons game = do
    translate (V2 180 400) $ bitmap (startBtn game)
    translate (V2 300 400) $ bitmap (rankBtn game)
    
drawPolls game = mapM_ (drawPoll (upPoll game) (underPoll game)) $ map snd $ polls game

drawPoll up under pos = do
    translate (moveY pos (-230)) $ bitmap up
    translate (moveY pos (220)) $ bitmap under

drawScore game = do
    translate (V2 (winWIDTH/2) (winHEIGHT/2)) $ bitmap (scoreBoard game)
    let newScore = max (highscore game) (points game)
    if points game > 50
        then do
            translate (V2 (winWIDTH/2-87) (winHEIGHT/2+5)) $ bitmap (medal game)
        else
            return ()
    translate (V2 340 250)$ color white $ text (font game) 30 $ show $ points game
    translate (V2 340 310)$ color white $ text (font game) 30 $ show newScore
    
    
    
--get the inputs from mouse and keyboard
seeInput Opening game = do
    pos <- mousePosition
    t <- mouseUpL
    return $ if t && inStartBtn pos then game {state = Playing} else game
    
seeInput Playing game = do
    mUp <- mouseUpL
    key <- keyUp KeySpace
    return $ if mUp || key then game {jumpPower = jumpPower game - 17,gravitiy = 0} else game
    
seeInput GameOver game = do
    mUp <- mouseUpL
    key <- keyUp KeySpace
    if (key || mUp) && 460 <= posY (pos game)
        then do
            if highscore game < (points game)
                then do
                    liftIO $ removeFile "score"
                    liftIO $ writeFile "score" $ show $ points game    
                    return $ refresh game
                else return game
        else return game
    

refresh game = game{
    state = Opening,
    points = 0,
    pos = V2 birdX 260,
    jumpPower = 0,
    gravitiy = 3,
    polls = [(False,V2 480 200)]
    }

inStartBtn (V2 x y) = 135 <= x && x <= 225 && 376 <= y && y <= 424

--update functions
update Playing game = do
    r <- randomness (100,350)
    let (cnt,polls') = passPoll (polls game)
    return $ game{
        points = points game + cnt, 
        polls = deletePoll.movePolls $ nextPolls r polls',
        gravitiy = nextGravity (gravitiy game) (jumpPower game),
        state = nextState (pos game) polls'
        }

update GameOver game = do
    let g = nextGravity (gravitiy game) (jumpPower game)
    return $ game{
        gravitiy = g+1,
        pos = nextPos (pos game) 0 (gravitiy game)
        }
    

nextPos :: Vec2 -> Double -> Double -> Vec2
nextPos pos pwr g
    | 460 <= posY pos = V2 birdX 460
    | pwr < 0 = moveY pos pwr
    | 0 == pwr = moveY pos g
    | otherwise = pos

nextJump power = if power < 0 then power + 2 else 0

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

nextState :: Vec2 -> [Poll] -> GameState
nextState pos polls = if isHit pos polls then GameOver else Playing

isHit :: Vec2 -> [Poll] -> Bool
isHit pos = not.null.filter (hit pos.snd)

hit :: Vec2 -> Vec2 -> Bool
hit pos poll = 
    (posY poll+90)-posY pos < 35 && (posX poll-70) < posX pos && posX pos < (posX poll+75)
    || posY pos-(posY poll-87) < 35 && (posX poll-70) < posX pos && posX pos < (posX poll+75)
    || posY pos < (posY poll-87) && abs (posX poll - posX pos) < 25
    || (posY poll+90) < posY pos && abs (posX poll - posX pos) < 25
    || 460 <= posY pos

distance :: Vec2 -> Vec2 -> Double
distance (V2 x1 y1) (V2 x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

--operate the position of objects
posX (V2 x _) = x
moveX (V2 x y) v = V2 (x+v) y
posY (V2 _ y) = y
moveY (V2 x y) v = V2 x (y+v)
(V2 x y) .+. (V2 x' y') = V2 (x+x') (y+y')

--show data with text
debug font str = translate (V2 40 40)$ color black $ text font 25 str
showPoints font pnt = translate (V2 (winWIDTH/2) 40)$ color white $ text font 40 $ show pnt