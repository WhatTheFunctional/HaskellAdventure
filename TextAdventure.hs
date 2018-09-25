--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable

import System.IO
import System.Exit
import qualified Data.Char
import qualified Data.Text
import qualified Data.List.Split

import TextReflow
import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph
import NightmareAdventure

allDelimiters :: [Char]
allDelimiters = [' ', '\t']

allColumnWidth :: Int
allColumnWidth = 75

printTokens :: String -> [Token] -> IO ()
printTokens word [] = return () >> putStr "\n" >> hFlush stdout
printTokens word ((TokenVerb _ _) : tokens) = (putStrLn ("== Verb " ++ word)) >> printTokens word tokens >> hFlush stdout
printTokens word ((TokenNoun _ _) : tokens) = (putStrLn ("== Noun " ++ word)) >> printTokens word tokens >> hFlush stdout
printTokens word ((TokenPreposition _ _) : tokens) = (putStrLn ("== Preposition " ++ word)) >> printTokens word tokens >> hFlush stdout

--Print tokens for a word
printWordTokens :: [TokenMatch] -> IO ()
printWordTokens [] = putStr "\n" >> hFlush stdout
printWordTokens ((TokenMatch word matchedTokens) : tokens) = printTokens word matchedTokens >> printWordTokens tokens >> hFlush stdout

--Print sentence
printSentences :: [Sentence] -> IO ()
printSentences [] = putStr "I'm sorry, I don't understand what you said." >> hFlush stdout
printSentences (sentence : []) = putStrLn (show sentence) >> hFlush stdout
printSentences (sentence : sentences) = putStrLn (show sentence) >> printSentences sentences >> hFlush stdout

--Print intro
printIntro :: IO ()
printIntro
    = reflowPutStrs allDelimiters 
                    allColumnWidth
                    ["Haskell Text Adventure Engine v1.0\n",
                     "Copyright Laurence Emms 2018\n\n\n"] >>
      hFlush stdout

--Print help text
printHelp :: IO ()
printHelp
    = reflowPutStrs allDelimiters
                    allColumnWidth
                    ["The following commands are available:\n",
                     "Inventory - Print all current inventory items.\n",
                     "Help - Print help text.\n",
                     "Grammar - Print available grammar.\n",
                     "Verbs - Print all available verbs.\n",
                     "Prepositions - Print all available prepositions.\n",
                     -- "Nouns - Print all available nouns. Warning, this contains spoilers!\n",
                     -- "Flags - Print all current flags. Warning, this contains spoilers!\n",
                     "Quit - Exit the  game.\n",
                     "--------------------"] >>
      hFlush stdout

printGrammar :: IO ()
printGrammar
    = reflowPutStrs allDelimiters
                    allColumnWidth
                    ["Simple sentence: <Verb> <Noun>\n",
                     "Simple preposition sentence: <Verb> <Preposition> <Noun>\n",
                     "Complex sentence: <Verb> <Noun> <Preposition> <Noun>\n",
                     "Complex preposition sentence: <Verb> <Preposition> <Noun> <Preposition> <Noun>\n"] >>
      hFlush stdout

printVerbs :: [Token] -> IO ()
printVerbs [] = putStr "\n" >> hFlush stdout
printVerbs ((TokenVerb name synonyms) : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ "\n") >>
      printVerbs tokens >>
      hFlush stdout
printVerbs (_ : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Invalid token\n") >>
      printVerbs tokens >>
      hFlush stdout

printNouns :: [Token] -> IO ()
printNouns [] = putStr "\n" >> hFlush stdout
printNouns ((TokenNoun name synonyms) : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ "\n") >>
      printNouns tokens >> hFlush stdout
printNouns (_ : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Invalid token\n") >>
      printNouns tokens >> hFlush stdout

printPrepositions :: [Token] -> IO ()
printPrepositions [] = putStr "\n" >> hFlush stdout
printPrepositions ((TokenPreposition name synonyms) : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Synonyms for " ++ name ++ ": " ++ (show synonyms) ++ "\n") >>
      printPrepositions tokens >> hFlush stdout
printPrepositions (_ : tokens)
    = reflowPutStr allDelimiters
                   allColumnWidth
                   ("Invalid token\n") >>
      printPrepositions tokens >> hFlush stdout

printInventory :: Inventory -> IO ()
printInventory (Inventory []) = putStr "\n" >> hFlush stdout
printInventory (Inventory (object : remainingInventory))
    = reflowPutStr allDelimiters
                   allColumnWidth
                   (object ++ "\n") >>
      printInventory (Inventory remainingInventory) >> hFlush stdout

printFlags :: Flags -> IO ()
printFlags (Flags []) = putStr "\n" >> hFlush stdout
printFlags (Flags (flag : remainingFlags))
    = reflowPutStr allDelimiters
                   allColumnWidth
                   (flag ++ ".\n") >>
      printFlags (Flags remainingFlags) >> hFlush stdout

parseInput :: Inventory -> Flags -> String -> IO (Maybe [Sentence])
parseInput inventory flags line
    | map Data.Char.toLower line == "help" = printHelp >> return (Just [])
    | map Data.Char.toLower line == "grammar" = putStrLn "All grammar:" >> printGrammar >> return (Just [])
    | map Data.Char.toLower line == "verbs" = putStrLn "All verbs:" >> printVerbs allVerbs >> return (Just [])
    -- | map Data.Char.toLower line == "nouns" = putStrLn "All nouns:" >> printNouns allNouns >> return (Just [])
    | map Data.Char.toLower line == "prepositions" = putStrLn "All prepositions:" >> printPrepositions allPrepositions >> return (Just [])
    | map Data.Char.toLower line == "inventory" = putStrLn "All items in inventory:" >> printInventory inventory >> return (Just [])
    -- | map Data.Char.toLower line == "flags" = putStrLn "All currently set flags:" >> printFlags flags >> return (Just [])
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | sentences == [] = putStr "I'm sorry, I don't understand what you said." >> hFlush stdout >> return (Just sentences)
    | otherwise = --printWordTokens sentenceTokenMatches >>
                  --printSentences sentences >>
                  hFlush stdout >>
                  return (Just sentences)
        where inputWords = Data.List.Split.splitOneOf allDelimiters line
              sentenceTokenMatches = lexInput allTokens inputWords
              sentences = parseSentence sentenceTokenMatches

doAdventureLoop :: NarrativeGraph -> SceneKey -> Inventory -> Flags -> Maybe [Sentence] -> IO (Maybe (SceneKey, Inventory, Flags))
doAdventureLoop _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop narrativeGraph sceneKey inventory flags (Just []) = adventure narrativeGraph (Just (sceneKey, inventory, flags)) --Failed to parse any sentences
doAdventureLoop narrativeGraph sceneKey inventory flags (Just sentences) = performInteraction allDelimiters allColumnWidth narrativeGraph sceneKey inventory flags sentences >>=
                                                                           adventure narrativeGraph --Perform the adventure loop

updateAdventure :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
updateAdventure _ Nothing = return Nothing
updateAdventure narrativeGraph (Just (sceneKey, inventory, flags))
    = putStr "\n> " >>
      hFlush stdout >>
      printInvalidInteractions narrativeGraph sceneKey >>
      getLine >>=
      parseInput inventory flags >>=
      (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
      doAdventureLoop narrativeGraph sceneKey inventory flags

adventure :: NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
adventure _ Nothing = reflowPutStr allDelimiters allColumnWidth "Game over. Thanks for playing!" >> hFlush stdout >> return Nothing
adventure narrativeGraph (Just (sceneKey, inventory, flags))
    = printSceneDescription allDelimiters allColumnWidth narrativeGraph (Just (sceneKey, inventory, flags)) >>=
      updateAdventure narrativeGraph

main = printIntro >>
       reflowPutStr allDelimiters allColumnWidth gameIntro >>
       putStr "\n" >>
       printHelp >>
       putStr "\n" >>
       putStr "\n" >>
       putStr "\n" >>
       hFlush stdout >>
       adventure (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
       return ()
           where (adventureScenes, endScenes) = allScenes
