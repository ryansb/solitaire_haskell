import Data.Char
-- 53 is Joker A
jA = 53
-- 54 is Joker B
jB = 54

pack      = [1..54]
testPack  = [1..20] ++ [jA] ++ [21..40] ++ [jB] ++ [41..52]
testPack' = [1..20] ++ [jB] ++ [21..40] ++ [jA] ++ [41..52]

-- TODO: Actually take input for key
rawkey = toStream (take 10 (repeat 'A'))

-- TODO: actually take input for message
message = "hello aunt matilda"

toStream i = [ord c - 13 | c <- (map toUpper i), isAsciiUpper c]

-- doesn't quite work when 1 is at the front
cutDeck deck =
    let v = splitAt (fromIntegral (head deck) + 1) deck
    in tail (fst v) ++ [head deck] ++ snd v


moveJoker j deck
    | last deck == j = [head deck, j] ++ (tail (init deck)) -- Joker is bottom
    | otherwise      =
        let (ys,zs) = splitAt (length (takeWhile (/=j) deck) + 1) (filter (/=j) deck)
        in ys ++ [j] ++ zs

moveJokerA deck = moveJoker jA deck
moveJokerB deck = moveJoker jB (moveJoker jB deck)

moveJokers deck = moveJokerB (moveJokerA deck)

tripleCut deck =
  let front    = takeWhile (<53) deck
      middle   = takeWhile (<53) (drop ((length front) + 1) deck)
      back     = tail (dropWhile (<53) (tail (dropWhile (<53) deck)))
      jOrder   = filter (>=53) deck
  in back ++ [head jOrder] ++ middle ++ [last jOrder] ++ front

countCut deck
    | last deck >= 53 = deck
    | otherwise       =
        let offset  = fromIntegral (last deck)
            front   = take offset deck
            middle  = drop offset deck
        in (init middle) ++ front ++ [last deck]

cardN card deck = let c = fromIntegral (deck !! card)
    in if c >= 53 then 53 else c

streamCard deck = fromIntegral (deck !! (cardN 0 deck))

streamChar' deck = let sc = streamCard (deckStep deck)
    in if sc >= 53 then streamChar' (deckStep deck) else sc

deckStep deck = countCut (tripleCut (moveJokers deck))

cipherStep deck =
    let c       = streamChar' deck
        stepped = deckStep deck
    in if c >= 53 then cipherStep stepped else (c, stepped)

iterStream prev = cipherStep (snd prev)

newKeystream deck = [fst s | s <- iterate iterStream (0, deck), (fst s) /= 0]

testStream = newKeystream pack
