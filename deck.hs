import Data.Char
-- 53 is Joker A
jA = 53
-- 54 is Joker B
jB = 54

pack = [1..54]

-- TODO: Actually take input for key
rawkey = toStream (take 10 (repeat 'A'))

-- TODO: actually take input for message
message = "hello aunt matilda"

toStream i = [ord c - 13 | c <- (map toUpper i), isAsciiUpper c]

-- doesn't quite work when 1 is at the front
cutDeck deck =
    let v = splitAt (fromIntegral (head deck) + 1) deck
    in tail (fst v) ++ [head deck] ++ snd v


moveJokerA deck
    | (head deck) == jA = (init (tail deck) ++ [(head deck), (last deck)]) -- A Joker is top
    | otherwise       = let (ys,zs) = splitAt ((length (takeWhile (/=jA) deck)) - 1) (filter (/=jA) deck) in ys ++ [jA] ++ zs

moveJokerB deck
    | head deck == jB = let (ys, zs) = splitAt ((length deck) - 2) deck in (tail ys) ++ [ head deck ] ++ zs
    | deck !! 1 == jB = let (ys, zs) = splitAt ((length deck) - 1) deck in (filter (/=jB) ys) ++ [ jB ] ++ zs
    | otherwise       = let (ys,zs) = splitAt ((length (takeWhile (/=jB) deck)) - 1) (filter (/=jB) deck) in ys ++ [jB] ++ zs
