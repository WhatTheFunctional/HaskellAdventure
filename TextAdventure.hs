--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO
import System.Exit
import qualified Data.Char
import qualified Data.Text

import NaturalLanguageLexer
import DummyAdventure

printTokens :: String -> [Token] -> IO ()
printTokens word [] = return ()
printTokens word ((TokenVerb synonyms) : tokens) = (putStrLn ("== Verb " ++ word)) >> printTokens word tokens
printTokens word ((TokenNoun name) : tokens) = (putStrLn ("== Noun " ++ word)) >> printTokens word tokens
printTokens word ((TokenPreposition synonyms) : tokens) = (putStrLn ("== Preposition " ++ word)) >> printTokens word tokens

--Print tokens for a word
printWordTokens :: [TokenMatch] -> IO ()
printWordTokens [] = return ()
printWordTokens ((TokenMatch word matchedTokens) : tokens) = printTokens word matchedTokens >> printWordTokens tokens

--Print help text
printHelp :: IO ()
printHelp = putStrLn "Commands:" >>
            putStrLn "Help - Print help text" >>
            putStrLn "Nouns - Print all available nouns" >>
            putStrLn "Verbs - Print all available verbs" >>
            putStrLn "Prepositions - Print all available prepositions" >>
            putStrLn "Quit - Exit the game"

--Print verbs
printVerbs :: [Token] -> IO ()
printVerbs [] = return ()
printVerbs ((TokenVerb synonyms) : tokens) = putStrLn (show synonyms) >> printVerbs tokens

--Print nouns
printNouns :: [Token] -> IO ()
printNouns [] = return ()
printNouns ((TokenNoun name) : tokens) = putStrLn name >> printNouns tokens

--Print prepositions
printPrepositions :: [Token] -> IO ()
printPrepositions [] = return ()
printPrepositions ((TokenPreposition synonyms) : tokens) = putStrLn (show synonyms) >> printPrepositions tokens

parseInput :: String -> IO ()
parseInput line
    | map Data.Char.toLower line == "exit" = putStrLn "Thanks for playing!" >> exitSuccess
    | map Data.Char.toLower line == "Exit" = putStrLn "Thanks for playing!" >> exitSuccess
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> exitSuccess
    | map Data.Char.toLower line == "Quit" = putStrLn "Thanks for playing!" >> exitSuccess
    | map Data.Char.toLower line == "help" = printHelp
    | map Data.Char.toLower line == "Help" = printHelp
    | map Data.Char.toLower line == "verbs" = printVerbs allVerbs
    | map Data.Char.toLower line == "Verbs" = printVerbs allVerbs
    | map Data.Char.toLower line == "nouns" = printNouns allNouns
    | map Data.Char.toLower line == "Nouns" = printNouns allNouns
    | map Data.Char.toLower line == "prepositions" = printPrepositions allPrepositions
    | map Data.Char.toLower line == "Prepositions" = printPrepositions allPrepositions
    | otherwise = return (lexInput allTokens (map Data.Text.unpack (Data.Text.splitOn (Data.Text.pack " ") (Data.Text.pack line)))) >>= printWordTokens >> return ()

adventure :: IO ()
adventure = getLine >>= parseInput >> hFlush stdout >> adventure

main = printHelp >> hFlush stdout >> adventure

