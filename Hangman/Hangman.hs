import System.IO

hangman :: IO ()
hangman = do    putStrLn "Think of a word:" -- prints message to console
                word <- sgetLine -- gets the word
                putStrLn "Try to guess it:" -- prints message to a console
                play word

sgetLine :: IO String 
sgetLine = do   x <- getCh -- gets each character
                if x == '\n' then
                    do  putChar x
                        return []
                else 
                    do  putChar '-' --prints this to screen
                        xs <- sgetLine --recursion. Gets rest of line.
                        return (x : xs)
getCh :: IO Char 
getCh = do  hSetEcho stdin False --turns the echo status to false so won't print charcter to screen
            x <- getChar --gets character user typed
            hSetEcho stdin True --turns echo status back on.
            return x

play :: String -> IO ()
play word = do  putStr "? " --writes ? to output device.
                guess <- getLine --reads line
                if guess == word then
                    putStrLn "You got it!!"
                else
                    do  putStrLn (match word guess)
                        play word --recursion

match :: String -> String -> String 
match xs ys = [if elem x ys then x else '-' | x <- xs]
