--NaturalLanguageLexer.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language lexer

module NaturalLanguageLexer (Token(..),
                             TokenMatch(..),
                             join,
                             lexInput) where

import qualified Data.Char

data Token = TokenVerb [String] |
             TokenNoun [String] |
             TokenPreposition [String] deriving (Show, Eq)

--Stores the result when a string matches one or more tokens
data TokenMatch = TokenMatch String [Token] deriving (Show, Eq)

join :: Maybe TokenMatch -> Maybe TokenMatch -> Maybe TokenMatch
join Nothing Nothing = Nothing
join (Just a) Nothing = Just a
join Nothing (Just b) = Just b
join (Just (TokenMatch wordA tokensA)) (Just (TokenMatch wordB tokensB))
    | wordA == wordB = Just (TokenMatch wordA (tokensA ++ tokensB))
    | otherwise = Nothing

--Match a single word against a single token
tokenize :: String -> Token -> Maybe TokenMatch
tokenize "" _  = Nothing --Empty string can't match tokens
tokenize word token@(TokenVerb synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)
tokenize word token@(TokenNoun synonyms)
    | word `elem` synonyms = Just (TokenMatch word [token])
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)
tokenize word token@(TokenPreposition synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = (Data.Char.toLower (head word)) : (tail word)

lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens words [] = lexInput potentialTokens words
lexTokens potentialTokens words ((Nothing, _) : tokens) = lexTokens potentialTokens words tokens
lexTokens potentialTokens words ((Just token, tokenWords) : tokens) = token : lexInput potentialTokens tokenWords

lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput potentialTokens [] = []
lexInput potentialTokens (word1 : word2 : words) =
    lexTokens potentialTokens (word2 : words) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, words), --Prioritize look-ahead by putting the look-ahead option first
                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : words)]
lexInput potentialTokens (word : words) =
    lexTokens potentialTokens words [(foldl (\acc token -> (tokenize word token) `join` acc) Nothing potentialTokens, words)]
