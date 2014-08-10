import Data.Char
import System.Environment (getArgs)
-- 53 is Joker A
jA = 53
-- 54 is Joker B
jB = 54

toStream i = [fromIntegral (ord c - 13) | c <- (map toUpper i), isAsciiUpper c]
toKeyStream i = [ord c - 64 | c <- (map toUpper i), isAsciiUpper c]


moveJoker j deck
    | last deck == j = [head deck, j] ++ (tail (init deck)) -- Joker is bottom
    | otherwise      =
        let (ys,zs) = splitAt (length (takeWhile (/=j) deck) + 1) (filter (/=j) deck)
        in ys ++ [j] ++ zs

moveJokerA deck = moveJoker jA deck
moveJokerB deck = moveJoker jB (moveJoker jB deck)

moveJokers deck = moveJokerB (moveJokerA deck)

-- Swap the segments of the deck before/after the first/second joker
tripleCut deck =
  let front    = takeWhile (<53) deck
      middle   = takeWhile (<53) (drop ((length front) + 1) deck)
      back     = tail (dropWhile (<53) (tail (dropWhile (<53) deck)))
      jOrder   = filter (>=53) deck
  in back ++ [head jOrder] ++ middle ++ [last jOrder] ++ front

-- Cut deck at index of the value of the last card
-- leaving the last card in place
countCut deck
    | last deck >= 53 = deck
    | otherwise       = cutAt (fromIntegral (last deck)) deck

cutAt offset deck =
    let front  = take offset deck
        middle = drop offset deck
    in (init middle) ++ front ++ [last deck]

-- read the card at index N from the deck
readCardN deck card = let c = fromIntegral (deck !! card)
    in if c >= 53 then fromIntegral (53) else c

-- get the value of the next number in the stream
nextStreamChar deck = fromIntegral (deck !! (readCardN deck 0))

-- Do the steps that iterate the deck
deckStep deck = countCut (tripleCut (moveJokers deck))

-- A single step of the algo to generate the keystream from a deck
keyStreamStep deck =
    let stepped = deckStep deck
        c       = nextStreamChar stepped
    in if c >= 53 then keyStreamStep stepped else (c, stepped)

-- A single step of the algo to key the deck based on a string
keyDeck deck key
    | length key == 0 = deck
    | otherwise       = keyDeck (cutAt (head key) (deckStep deck)) (tail key)

iterStream prev = keyStreamStep (snd prev)

-- make an infinite keystream. Don't print to screen ;)
newKeystream deck = [fst s | s <- iterate iterStream (0, deck), (fst s) /= 0]

encryptStreams msg stream =
    let pairs = zip (toStream msg) stream
    in [((fst x + snd x) `mod` 26) + 65 | x <- pairs]

-- Pad message to be a multiple of 5 characters
pad msg
    | length msg `mod` 5 == 0 = msg
    | otherwise               = msg ++ (take (-(length msg) `mod` 5 + 5) (repeat 'X'))

intersperse' msg
    | length msg < 5 = msg
    | otherwise      = take 5 msg ++ " " ++ intersperse' (drop 5 msg)

encryptMessage key msg =
    let message = pad msg
        deck    = keyDeck [1..54] (toKeyStream key)
        stream  = newKeystream deck
        s = encryptStreams message stream
    in map chr (map fromIntegral s)

encryptContents key fname = do
    msg <- readFile fname
    putStrLn (intersperse' (encryptMessage key msg))

main = do
    args <- getArgs
    case args of
        [k,infile] -> encryptContents k infile
        [decrypt,k,infile] -> encryptContents k infile
        _ -> putStrLn "error: exactly two arguments needed"
