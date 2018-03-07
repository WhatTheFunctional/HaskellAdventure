--text_adventure.hs
--Copyright Laurence Emms 2018
--Module for text adventures

import System.IO
import System.Exit
import qualified Control.Monad
import qualified Data.Char
import qualified Data.Text

data Token = TokenVerb [String] |
             TokenNoun String |
             TokenPreposition [String] deriving (Show, Eq)

--Stores the result when a string matches one or more tokens
data TokenMatch = NoTokenMatches |
                  TokenMatches String [Token] 

join :: TokenMatch -> TokenMatch -> Maybe TokenMatch
join NoTokenMatches NoTokenMatches = Just NoTokenMatches
join a NoTokenMatches = Just a
join NoTokenMatches b = Just b
join (TokenMatches wordA tokensA) (TokenMatches wordB tokensB)
    | wordA == wordB = Just (TokenMatches wordA (tokensA ++ tokensB))
    | otherwise = Nothing

allVerbs :: [Token]
allVerbs = [TokenVerb ["get", "take", "pick up"],
            TokenVerb ["put", "place", "put down"],
            TokenVerb ["give"],
            TokenVerb ["use"],
            TokenVerb ["move", "walk", "go", "run"],
            TokenVerb ["climb", "scale"],
            TokenVerb ["open"],
            TokenVerb ["close", "shut"],
            TokenVerb ["enter"],
            TokenVerb ["leave", "exit"],
            TokenVerb ["eat", "consume"],
            TokenVerb ["drink"]]

allNouns :: [Token]
allNouns = [TokenNoun "north",
            TokenNoun "south",
            TokenNoun "west",
            TokenNoun "east",
            TokenNoun "up",
            TokenNoun "down",
            TokenNoun "inside",
            TokenNoun "outside",
            TokenNoun "door",
            TokenNoun "window",
            TokenNoun "chest",
            TokenNoun "entrance",
            TokenNoun "exit",
            TokenNoun "me",
            TokenNoun "myself",
            TokenNoun "Steve",
            TokenNoun "cake"]

allPrepositions :: [Token]
allPrepositions = [TokenPreposition ["in", "inside"],
                   TokenPreposition ["out", "outside"],
                   TokenPreposition ["on", "on top"],
                   TokenPreposition ["under", "below"],
                   TokenPreposition ["with"]]

allTokens :: [Token]
allTokens = allNouns ++ allVerbs ++ allPrepositions

--Match a single token
--Takes the word to match and the token to match as inputs
--Evaluates to the token which was matched
matchToken :: String -> Token -> Maybe Token
matchToken "" _  = Nothing --Empty string can't match tokens
matchToken word token@(TokenVerb synonyms)
    | lowerCaseWord `elem` synonyms = Just token
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)
matchToken word token@(TokenNoun name)
    | word == name = Just token
    | lowerCaseWord == name = Just token
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)
matchToken word token@(TokenPreposition synonyms)
    | lowerCaseWord `elem` synonyms = Just token
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)

--Match a list of tokens against a single word
tokenize :: String -> Token -> TokenMatch
tokenize word token =
    case matchToken word token of
        Nothing -> NoTokenMatches --Failed to match token
        Just matchedToken -> (TokenMatches word [matchedToken]) --Token matched

lexTokens :: [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens words [] = lexInput words
lexTokens words ((Nothing, _) : tokens) = lexTokens words tokens
lexTokens words ((Just token, tokenWords) : tokens) =
    case token of
        NoTokenMatches -> lexTokens words tokens
        token -> token : lexInput tokenWords

lexInput :: [String] -> [TokenMatch]
lexInput [] = []
lexInput (word1 : word2 : words) =
    lexTokens (word2 : words) [(Control.Monad.foldM (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) NoTokenMatches allTokens, words), --Prioritize look-ahead by putting the look-ahead option first
                               (Control.Monad.foldM (\acc token -> (tokenize word1 token) `join` acc) NoTokenMatches allTokens, word2 : words)]
lexInput (word : words) =
    lexTokens words [(Control.Monad.foldM (\acc token -> (tokenize word token) `join` acc) NoTokenMatches allTokens, words)]

printTokens :: String -> [Token] -> IO ()
printTokens word [] = return ()
printTokens word ((TokenVerb synonyms) : tokens) = (putStrLn ("== Verb " ++ word)) >> printTokens word tokens
printTokens word ((TokenNoun name) : tokens) = (putStrLn ("== Noun " ++ word)) >> printTokens word tokens
printTokens word ((TokenPreposition synonyms) : tokens) = (putStrLn ("== Preposition " ++ word)) >> printTokens word tokens

--Print tokens for a word
printWordTokens :: [TokenMatch] -> IO ()
printWordTokens [] = return ()
printWordTokens (NoTokenMatches : tokens) = (putStrLn ("Error: NoTokenMatches found in token list")) >> printWordTokens tokens
printWordTokens ((TokenMatches word matchedTokens) : tokens) = printTokens word matchedTokens >> printWordTokens tokens

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
    | otherwise = return (lexInput (map Data.Text.unpack (Data.Text.splitOn (Data.Text.pack " ") (Data.Text.pack line)))) >>= printWordTokens >> return ()

adventure :: IO ()
adventure = getLine >>= parseInput >> hFlush stdout >> adventure

main = printHelp >> hFlush stdout >> adventure

