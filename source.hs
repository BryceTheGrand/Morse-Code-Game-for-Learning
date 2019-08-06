import System.Random(randomRIO)
import Data.Char(toUpper)

data Morse = Morse { unMorse :: String  }
data Code  = Code  { unCode  :: [Morse] }

instance Show Morse where
    show = unMorse

instance Show Code where
    show = unwords . map unMorse . unCode

morse :: Char -> Morse
morse ' ' = Morse " "
morse x   = Morse $ case x of 'A' -> ".-"   ; 'B' -> "-..."
                              'C' -> "-.-." ; 'D' -> "-.."
                              'E' -> "."    ; 'F' -> "..-."
                              'G' -> "--."  ; 'H' -> "...."
                              'I' -> ".."   ; 'J' -> ".---"
                              'K' -> "-.-"  ; 'L' -> ".-.."
                              'M' -> "--"   ; 'N' -> "-."
                              'O' -> "---"  ; 'P' -> ".--."
                              'Q' -> "--.-" ; 'R' -> ".-."
                              'S' -> "..."  ; 'T' -> "-"
                              'U' -> "..-"  ; 'V' -> "...-"
                              'W' -> ".--"  ; 'X' -> "-..-"
                              'Y' -> "-.--" ; 'Z' -> "--.."
                              '1' -> ".----"; '2' -> "..---"
                              '3' -> "...--"; '4' -> "....-"
                              '5' -> "....."; '6' -> "-...."
                              '7' -> "--..."; '8' -> "---.."
                              '9' -> "----."; '0' -> "-----"

unmorse :: Morse -> Char
unmorse (Morse " ") = ' '
unmorse (Morse x)   = case x of ".-"    -> 'A'; "-..."  -> 'B'
                                "-.-."  -> 'C'; "-.."   -> 'D'
                                "."     -> 'E'; "..-."  -> 'F'
                                "--."   -> 'G'; "...."  -> 'H'
                                ".."    -> 'I'; ".---"  -> 'J'
                                "-.-"   -> 'K'; ".-.."  -> 'L'
                                "--"    -> 'M'; "-."    -> 'N'
                                "---"   -> 'O'; ".--."  -> 'P'
                                "--.-"  -> 'Q'; ".-."   -> 'R'
                                "..."   -> 'S'; "-"     -> 'T'
                                "..-"   -> 'U'; "...-"  -> 'V'
                                ".--"   -> 'W'; "-..-"  -> 'X'
                                "-.--"  -> 'Y'; "--.."  -> 'Z'
                                ".----" -> '1'; "..---" -> '2'
                                "...--" -> '3'; "....-" -> '4'
                                "....." -> '5'; "-...." -> '6'
                                "--..." -> '7'; "---.." -> '8'
                                "----." -> '9'; "-----" -> '0'

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

main = do
    wordsList <- fmap words (readFile "words.txt")
    answer'   <- pick wordsList
    let answer = filter (/= '-') $ map toUpper answer'
    putStrLn $ "The word is: " ++ show (morseCode answer) ++
               "\nWhat is this word decoded?"
    guess <- getLine
    if map toUpper guess == answer then do
        putStrLn "Correct!"
        main
    else do
        putStrLn $ "Incorrect! The word was: " ++ answer
        putStrLn $ findErrors answer (map toUpper guess)
        main
