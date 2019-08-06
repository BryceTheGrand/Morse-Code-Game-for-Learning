import System.Random(randomRIO)
import Data.Char(toUpper)

newtype Morse = Morse { unMorse :: String  }
newtype Code = Code   { unCode  :: [Morse] }

instance Show Morse where
    show = unMorse

instance Show Code where
    show = unwords . map unMorse . unCode

morse :: Char -> Morse
morse ' ' = Morse " "
morse x   = Morse $ case x of 'A' -> ".-"   ; 'B' -> "-..." ; 'C' -> "-.-."
                              'D' -> "-.."  ; 'E' -> "."    ; 'F' -> "..-."
                              'G' -> "--."  ; 'H' -> "...." ; 'I' -> ".."
                              'J' -> ".---" ; 'K' -> "-.-"  ; 'L' -> ".-.."
                              'M' -> "--"   ; 'N' -> "-."   ; 'O' -> "---"
                              'P' -> ".--." ; 'Q' -> "--.-" ; 'R' -> ".-."
                              'S' -> "..."  ; 'T' -> "-"    ; 'U' -> "..-"
                              'V' -> "...-" ; 'W' -> ".--"  ; 'X' -> "-..-"
                              'Y' -> "-.--" ; 'Z' -> "--.." ; '1' -> ".----"
                              '2' -> "..---"; '3' -> "...--"; '4' -> "....-"
                              '5' -> "....."; '6' -> "-...."; '7' -> "--..."
                              '8' -> "---.."; '9' -> "----."; '0' -> "-----"

unmorse :: Morse -> Char
unmorse (Morse " ") = ' '
unmorse (Morse x)   = case x of ".-"    -> 'A'; "-..."  -> 'B'; "-.-."  -> 'C'
                                "-.."   -> 'D'; "."     -> 'E'; "..-."  -> 'F'
                                "--."   -> 'G'; "...."  -> 'H'; ".."    -> 'I'
                                ".---"  -> 'J'; "-.-"   -> 'K'; ".-.."  -> 'L'
                                "--"    -> 'M'; "-."    -> 'N'; "---"   -> 'O'
                                ".--."  -> 'P'; "--.-"  -> 'Q'; ".-."   -> 'R'
                                "..."   -> 'S'; "-"     -> 'T'; "..-"   -> 'U'
                                "...-"  -> 'V'; ".--"   -> 'W'; "-..-"  -> 'X'
                                "-.--"  -> 'Y'; "--.."  -> 'Z'; ".----" -> '1'
                                "..---" -> '2'; "...--" -> '3'; "....-" -> '4'
                                "....." -> '5'; "-...." -> '6'; "--..." -> '7'
                                "---.." -> '8'; "----." -> '9'; "-----" -> '0'

morseCode :: String -> Code
morseCode xs = Code (map morse xs)

english :: Code -> String
english = map unmorse . unCode

pick :: [a] -> IO a
pick xs = do
    n <- randomRIO (0, length xs)
    return $ xs !! n

findErrors :: String -> String -> String
findErrors [] [] = []
findErrors _ []  = "The word was not the correct length."
findErrors [] _  = "The word was not the correct length."
findErrors (x:xs) (y:ys)
    | x == y    = findErrors xs ys
    | otherwise = y : " should have been " ++ [x] ++ ", which is: " ++
                  show (morse x) ++ "\n" ++ findErrors xs ys

getWord = do
    wordsList <- fmap words (readFile "words.txt")
    pick wordsList

morseToWords = do
    answer <- fmap (filter (/= '-') . map toUpper) getWord
    putStrLn $ "The word is: " ++ show (morseCode answer) ++
               "\nWhat is this word decoded?"
    guess <- getLine
    if map toUpper guess == answer then do
        putStrLn "Correct!"
        morseToWords
    else do
        putStrLn $ "Incorrect! The word was: " ++ answer
        putStrLn $ findErrors answer (map toUpper guess)
        morseToWords

findErrorsMorse :: String -> String -> String
findErrorsMorse answer guess = findErrorsMorse' (words answer) (words guess)
  where
    findErrorsMorse' [] [] = []
    findErrorsMorse' _ [] = []
    findErrorsMorse' [] _ = []
    findErrorsMorse' (x:xs) (y:ys)
        | x == y    = "" ++ findErrorsMorse' xs ys
        | otherwise = y ++ " should have been " ++ x ++ " which is: " ++
                      [unmorse (Morse y)] ++ "\n" ++ findErrorsMorse' xs ys

wordsToMorse = do
    answer <- fmap (filter (/= '-') . map toUpper) getWord
    putStrLn $ "The word is: " ++ answer ++ "\nWhat is this in Morse Code?"
    guess <- getLine
    if guess == show (morseCode answer) then do
        putStrLn "Correct!"
        wordsToMorse
    else do
        putStrLn $ "Incorrect! The code was: " ++ show (morseCode answer)
        putStrLn $ findErrorsMorse (show $ morseCode answer) guess
        wordsToMorse

main = do
    putStrLn "Would you like to be given words, or morse code? (1, 2)"
    choice <- fmap head getLine
    if choice == '1' then morseToWords
    else wordsToMorse