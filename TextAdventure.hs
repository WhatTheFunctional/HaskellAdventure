--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO
import System.Exit
import qualified Data.Char
import qualified Data.Text

import NaturalLanguageLexer
import NaturalLanguageParser
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

--Print sentence
printSentences :: [Sentence] -> IO ()
printSentences [] = putStrLn "I'm sorry, I don't understand what you said."
printSentences (sentence : []) = putStrLn (show sentence)
printSentences (sentence : sentences) = putStrLn (show sentence) >> printSentences sentences

--Print help text
printHelp :: IO ()
printHelp = putStrLn "Commands:" >>
            putStrLn "Help - Print help text" >>
            putStrLn "Grammar - Print available grammar" >>
            putStrLn "Nouns - Print all available nouns" >>
            putStrLn "Verbs - Print all available verbs" >>
            putStrLn "Prepositions - Print all available prepositions" >>
            putStrLn "Quit - Exit the game"

--Print grammar
printGrammar :: IO ()
printGrammar = putStrLn "Simple sentence: <Verb> <Noun>" >>
               putStrLn "Simple preposition sentence: <Verb> <Preposition> <Noun>" >>
               putStrLn "Complex sentence: <Verb> <Noun> <Preposition> <Noun>" >>
               putStrLn "Complex preposition sentence: <Verb> <Preposition> <Noun> <Preposition> <Noun>"

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
    | map Data.Char.toLower line == "help" = printHelp
    | map Data.Char.toLower line == "grammar" = printGrammar
    | map Data.Char.toLower line == "verbs" = printVerbs allVerbs
    | map Data.Char.toLower line == "nouns" = printNouns allNouns
    | map Data.Char.toLower line == "prepositions" = printPrepositions allPrepositions
    | map Data.Char.toLower line == "exit" = putStrLn "Thanks for playing!" >> exitSuccess
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> exitSuccess
    | otherwise = return sentenceTokenMatches >>= printWordTokens >> return sentences >>= printSentences >> return ()
        where inputWords = (map Data.Text.unpack (Data.Text.splitOn (Data.Text.pack " ") (Data.Text.pack line)))
              sentenceTokenMatches = lexInput allTokens inputWords
              sentences = parseSentence sentenceTokenMatches

adventure :: IO ()
adventure = getLine >>= parseInput >> hFlush stdout >> adventure

main = printHelp >> hFlush stdout >> adventure

