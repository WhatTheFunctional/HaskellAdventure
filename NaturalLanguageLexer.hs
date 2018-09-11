--NaturalLanguageLexer.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language lexer

module NaturalLanguageLexer (Token(..),
                             TokenMatch(..),
                             join,
                             lexInput) where

import qualified Data.Char

data Token = TokenVerb String [String] |
             TokenNoun  String [String] |
             TokenPreposition  String [String] deriving (Show, Eq)

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
tokenize word token@(TokenVerb _ synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = map Data.Char.toLower word
tokenize word token@(TokenNoun _ synonyms)
    | word `elem` synonyms = Just (TokenMatch word [token])
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = map Data.Char.toLower word
tokenize word token@(TokenPreposition _ synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = map Data.Char.toLower word

lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens words [] = lexInput potentialTokens words
lexTokens potentialTokens words ((Nothing, _) : tokens) = lexTokens potentialTokens words tokens
lexTokens potentialTokens words ((Just token, tokenWords) : tokens) = token : lexInput potentialTokens tokenWords

lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput potentialTokens [] = []
--Prioritize look-ahead by putting the look-ahead option first
lexInput potentialTokens (word1 : word2 : word3 : word4 : word5 : words) =
    lexTokens potentialTokens (word5 : words) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3 ++ (' ' : word4 ++ (' ' : word5)))) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3 ++ (' ' : word4))) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3)) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : words)]
lexInput potentialTokens (word1 : word2 : word3 : word4 : words) =
    lexTokens potentialTokens (word4 : words) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3 ++ (' ' : word4))) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3)) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : words)]
lexInput potentialTokens (word1 : word2 : word3 : words) =
    lexTokens potentialTokens (word3 : words) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ (' ' : word3)) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : words)]
lexInput potentialTokens (word1 : word2 : words) =
    lexTokens potentialTokens (word2 : words) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, words),
                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : words)]
lexInput potentialTokens (word : words) =
    lexTokens potentialTokens words [(foldl (\acc token -> (tokenize word token) `join` acc) Nothing potentialTokens, words)]
