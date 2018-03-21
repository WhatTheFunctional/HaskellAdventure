--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO
import System.Exit
import qualified Data.Char
import qualified Data.Text

import TextReflow
import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph
import NightmareAdventure

allDelimiters :: [Char]
allDelimiters = [' ', '\t']

allColumnWidth :: Int
allColumnWidth = 80

printTokens :: String -> [Token] -> IO ()
printTokens word [] = return () >> putStr "\n" >> hFlush stdout
printTokens word ((TokenVerb _ _) : tokens) = (putStrLn ("== Verb " ++ word)) >> printTokens word tokens
printTokens word ((TokenNoun _ _) : tokens) = (putStrLn ("== Noun " ++ word)) >> printTokens word tokens
printTokens word ((TokenPreposition _ _) : tokens) = (putStrLn ("== Preposition " ++ word)) >> printTokens word tokens

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
printIntro = reflowPutStrs allDelimiters 
                           allColumnWidth
                           ["Haskell Text Adventure Engine v1.0\n",
                            "Copyright Laurence Emms 2018\n\n"] >>
             hFlush stdout

--Print help text
printHelp :: IO ()
printHelp = reflowPutStrs allDelimiters
                          allColumnWidth
                          ["The following commands are available:\n",
                           "Help - Print help text.\n",
                           "Grammar - Print available grammar.\n",
                           "Nouns - Print all available nouns. Warning, this contains spoilers!\n",
                           "Verbs - Print all available verbs.\n",
                           "Prepositions - Print all available prepositions.\n",
                           "Inventory - Print all current inventory items.\n",
                           "Flags - Print all current flags. Warning, this contains spoilers!\n",
                           "Quit - Exit the  game.\n",
                           "--------------------\n\n"] >>
            hFlush stdout

printGrammar :: IO ()
printGrammar = reflowPutStrs allDelimiters
                             allColumnWidth
                             ["Simple sentence: <Verb> <Noun>\n",
                              "Simple preposition sentence: <Verb> <Preposition> <Noun>\n",
                              "Complex sentence: <Verb> <Noun> <Preposition> <Noun>\n",
                              "Complex preposition sentence: <Verb> <Preposition> <Noun> <Preposition> <Noun>\n"] >>
               hFlush stdout

printVerbs :: [Token] -> IO ()
printVerbs [] = putStr "\n" >> hFlush stdout
printVerbs ((TokenVerb name synonyms) : tokens) = reflowPutStr allDelimiters
                                                               allColumnWidth
                                                               ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ ".\n") >>
                                             printVerbs tokens

printNouns :: [Token] -> IO ()
printNouns [] = putStr "\n" >> hFlush stdout
printNouns ((TokenNoun name synonyms) : tokens) = reflowPutStr allDelimiters
                                                               allColumnWidth
                                                               ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ ".\n") >>
                                             printNouns tokens

printPrepositions :: [Token] -> IO ()
printPrepositions [] = putStr "\n" >> hFlush stdout
printPrepositions ((TokenPreposition name synonyms) : tokens) = reflowPutStr allDelimiters
                                                                             allColumnWidth
                                                                             ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ ".\n") >>
                                                           printPrepositions tokens

printInventory :: Inventory -> IO ()
printInventory (Inventory []) = putStr "\n" >> hFlush stdout
printInventory (Inventory (object : remainingInventory)) = reflowPutStr allDelimiters
                                                                        allColumnWidth
                                                                        (object ++ ".\n") >>
                                                           printInventory (Inventory remainingInventory)

printFlags :: Flags -> IO ()
printFlags (Flags []) = putStr "\n" >> hFlush stdout
printFlags (Flags (flag : remainingFlags)) = reflowPutStr allDelimiters
                                                          allColumnWidth
                                                          (flag ++ ".\n") >>
                                             printFlags (Flags remainingFlags)

parseInput :: Inventory -> Flags -> String -> IO (Maybe [Sentence])
parseInput inventory flags line
    | map Data.Char.toLower line == "help" = printHelp >> return (Just [])
    | map Data.Char.toLower line == "grammar" = putStrLn "All grammar:" >> printGrammar >> return (Just [])
    | map Data.Char.toLower line == "verbs" = putStrLn "All verbs:" >> printVerbs allVerbs >> return (Just [])
    | map Data.Char.toLower line == "nouns" = putStrLn "All nouns:" >> printNouns allNouns >> return (Just [])
    | map Data.Char.toLower line == "prepositions" = putStrLn "All prepositions:" >> printPrepositions allPrepositions >> return (Just [])
    | map Data.Char.toLower line == "inventory" = putStrLn "All items in inventory:" >> printInventory inventory >> return (Just [])
    | map Data.Char.toLower line == "flags" = putStrLn "All currently set flags:" >> printFlags flags >> return (Just [])
    | map Data.Char.toLower line == "exit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | sentences == [] = reflowPutStr allDelimiters allColumnWidth "I'm sorry, I don't understand what you said." >> putStr "\n" >> return (Just sentences)
    | otherwise = --printWordTokens sentenceTokenMatches >>
                  --printSentences sentences >>
                  return (Just sentences)
        where inputWords = splitString allDelimiters line []
              sentenceTokenMatches = lexInput allTokens inputWords
              sentences = parseSentence sentenceTokenMatches

doAdventureLoop :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> Maybe [Sentence] -> IO (Maybe (SceneIndex, Inventory, Flags))
doAdventureLoop _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop narrativeGraph sceneIndex inventory flags (Just []) = adventure narrativeGraph (Just (sceneIndex, inventory, flags)) --Failed to parse any sentences
doAdventureLoop narrativeGraph sceneIndex inventory flags (Just sentences) = performInteraction allDelimiters allColumnWidth narrativeGraph sceneIndex inventory flags sentences >>=
                                                                             adventure narrativeGraph --Perform the adventure loop

updateAdventure :: NarrativeGraph -> Maybe (SceneIndex, Inventory, Flags) -> IO (Maybe (SceneIndex, Inventory, Flags))
updateAdventure _ Nothing = return Nothing
updateAdventure narrativeGraph (Just (sceneIndex, inventory, flags))
    = putStr "\n" >>
      printInvalidInteractions narrativeGraph sceneIndex >>
      getLine >>=
      parseInput inventory flags >>=
      doAdventureLoop narrativeGraph sceneIndex inventory flags

adventure :: NarrativeGraph -> Maybe (SceneIndex, Inventory, Flags) -> IO (Maybe (SceneIndex, Inventory, Flags))
adventure _ Nothing = reflowPutStr allDelimiters allColumnWidth "Game over. Thanks for playing!" >> hFlush stdout >> return Nothing
adventure narrativeGraph (Just (sceneIndex, inventory, flags)) = printSceneDescription allDelimiters allColumnWidth narrativeGraph (Just (sceneIndex, inventory, flags)) >>=
                                                                 updateAdventure narrativeGraph

main = printIntro >>
       reflowPutStr allDelimiters allColumnWidth gameIntro >>
       putStr "\n" >>
       printHelp >>
       hFlush stdout >>
       adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (0, startInventory, startFlags)) >>
       return ()
           where (adventureScenes, endScenes) = allScenes
