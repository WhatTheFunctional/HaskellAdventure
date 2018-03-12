--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO
import System.Exit
import qualified Data.Char
import qualified Data.Text

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph
import DummyAdventure

printTokens :: String -> [Token] -> IO ()
printTokens word [] = return () >> putStr "\n" >> hFlush stdout
printTokens word ((TokenVerb synonyms) : tokens) = (putStrLn ("== Verb " ++ word)) >> printTokens word tokens
printTokens word ((TokenNoun name) : tokens) = (putStrLn ("== Noun " ++ word)) >> printTokens word tokens
printTokens word ((TokenPreposition synonyms) : tokens) = (putStrLn ("== Preposition " ++ word)) >> printTokens word tokens

--Print tokens for a word
printWordTokens :: [TokenMatch] -> IO ()
printWordTokens [] = putStr "\n" >> hFlush stdout
printWordTokens ((TokenMatch word matchedTokens) : tokens) = printTokens word matchedTokens >> printWordTokens tokens

--Print sentence
printSentences :: [Sentence] -> IO ()
printSentences [] = putStrLn "I'm sorry, I don't understand what you said.\n" >> hFlush stdout
printSentences (sentence : []) = putStrLn (show sentence)
printSentences (sentence : sentences) = putStrLn (show sentence) >> printSentences sentences

--Print intro
printIntro :: IO ()
printIntro = putStrLn "Haskell Text Adventure Engine v1.0" >>
             putStrLn "Copyright Laurence Emms 2018" >>
             putStr   "\n" >>
             hFlush stdout

--Print help text
printHelp :: IO ()
printHelp = putStrLn "The following commands are available:" >>
            putStrLn "Help - Print help text" >>
            putStrLn "Grammar - Print available grammar" >>
            putStrLn "Nouns - Print all available nouns" >>
            putStrLn "Verbs - Print all available verbs" >>
            putStrLn "Prepositions - Print all available prepositions" >>
            putStrLn "Inventory - Print all current inventory items" >>
            putStrLn "Flags - Print all current flags" >>
            putStrLn "Quit - Exit the game" >>
            putStr   "\n" >>
            hFlush stdout

printGrammar :: IO ()
printGrammar = putStrLn "Simple sentence: <Verb> <Noun>" >>
               putStrLn "Simple preposition sentence: <Verb> <Preposition> <Noun>" >>
               putStrLn "Complex sentence: <Verb> <Noun> <Preposition> <Noun>" >>
               putStrLn "Complex preposition sentence: <Verb> <Preposition> <Noun> <Preposition> <Noun>" >>
               putStr   "\n" >>
               hFlush stdout

printVerbs :: [Token] -> IO ()
printVerbs [] = putStr "\n" >> hFlush stdout
printVerbs ((TokenVerb synonyms) : tokens) = putStrLn ("Synonyms for " ++ (head synonyms) ++ ": " ++ (show synonyms) ++ ".") >> printVerbs tokens

printNouns :: [Token] -> IO ()
printNouns [] = putStr "\n" >> hFlush stdout
printNouns ((TokenNoun name) : tokens) = putStrLn (name ++ ".") >> printNouns tokens

printPrepositions :: [Token] -> IO ()
printPrepositions [] = putStr "\n" >> hFlush stdout
printPrepositions ((TokenPreposition synonyms) : tokens) = putStrLn ("Synonyms for " ++ (head synonyms) ++ ": " ++ (show synonyms) ++ ".") >> printPrepositions tokens

printInventory :: Inventory -> IO ()
printInventory (Inventory []) = putStr "\n" >> hFlush stdout
printInventory (Inventory (object : remainingInventory)) = putStrLn (object ++ ".") >> printInventory (Inventory remainingInventory)

printFlags :: Flags -> IO ()
printFlags (Flags []) = putStr "\n" >> hFlush stdout
printFlags (Flags (flag : remainingFlags)) = putStrLn (flag ++ ".") >> printFlags (Flags remainingFlags)

splitInput :: [Char] -> String -> [String] -> [String]
splitInput [] [] allWords = [] --No characters remaining and no current word
splitInput [] currentWord allWords = (reverse currentWord) : allWords --No characters remaining and a current word exists
splitInput (' ' : cs) [] allWords = splitInput cs [] allWords --Whitespace detected and no current word exists
splitInput (' ' : cs) currentWord allWords = (reverse currentWord) : splitInput cs [] allWords --Whitespace detected and a current word exists
splitInput (c : cs) currentWord allWords = splitInput cs (c : currentWord) allWords --A non-whitespace character was detected

parseInput :: Inventory -> Flags -> String -> IO (Maybe [Sentence])
parseInput inventory flags line
    | map Data.Char.toLower line == "help" = printHelp >> return (Just [])
    | map Data.Char.toLower line == "grammar" = putStrLn "All grammar:" >> printGrammar >> return (Just [])
    | map Data.Char.toLower line == "verbs" = putStrLn "All verbs:" >> printVerbs allVerbs >> return (Just [])
    | map Data.Char.toLower line == "nouns" = putStrLn "All nouns:" >> printNouns allNouns >> return (Just [])
    | map Data.Char.toLower line == "prepositions" = putStrLn "All prepositions:" >> printPrepositions allPrepositions >> return (Just [])
    | map Data.Char.toLower line == "inventory" = putStrLn "Inventory:" >> printInventory inventory >> return (Just [])
    | map Data.Char.toLower line == "flags" = putStrLn "Flags:" >> printFlags flags >> return (Just [])
    | map Data.Char.toLower line == "exit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | sentences == [] = putStrLn "I'm sorry, I don't understand what you said.\n" >> return (Just sentences)
    | otherwise = --printWordTokens sentenceTokenMatches >>
                  --printSentences sentences >>
                  return (Just sentences)
        where inputWords = splitInput line [] []
              sentenceTokenMatches = lexInput allTokens inputWords
              sentences = parseSentence sentenceTokenMatches

doAdventureLoop :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> Maybe [Sentence] -> IO (Maybe (NarrativeGraph, SceneIndex, Inventory, Flags))
doAdventureLoop _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop narrativeGraph sceneIndex inventory flags (Just []) = adventure (Just (narrativeGraph, sceneIndex, inventory, flags)) --Failed to parse any sentences
doAdventureLoop narrativeGraph sceneIndex inventory flags (Just sentences) = performInteraction narrativeGraph sceneIndex inventory flags sentences >>=
                                                                             adventure --Perform the adventure loop

adventure :: Maybe (NarrativeGraph, SceneIndex, Inventory, Flags) -> IO (Maybe (NarrativeGraph, SceneIndex, Inventory, Flags))
adventure Nothing = putStrLn "Game over. Thanks for playing!" >> hFlush stdout >> return Nothing
adventure (Just (narrativeGraph, sceneIndex, inventory, flags)) = printSceneDescription narrativeGraph sceneIndex inventory flags >>
                                                                  getLine >>=
                                                                  parseInput inventory flags >>=
                                                                  doAdventureLoop narrativeGraph sceneIndex inventory flags
main = printIntro >>
       putStrLn gameIntro >>
       printHelp >>
       hFlush stdout >>
       adventure (Just (makeNarrativeGraph adventureScenes endScenes defaultScene, 0, startInventory, startFlags)) >>
       return ()
           where (adventureScenes, endScenes) = allScenes
