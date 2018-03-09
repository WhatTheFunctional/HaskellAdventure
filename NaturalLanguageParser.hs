--NaturalLanguageParser.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language parser

module NaturalLanguageParser (Sentence(..),
                              parseSentence) where

import NaturalLanguageLexer

data Sentence = SimpleSentence Token Token |
                SimplePrepositionSentence Token Token Token |
                ComplexSentence Token Token Token Token |
                ComplexPrepositionSentence Token Token Token Token Token deriving (Show, Eq)

verbIsInTokenList :: [Token] -> Maybe Token
verbIsInTokenList [] = Nothing
verbIsInTokenList ((TokenVerb synonyms) : ts) = Just (TokenVerb synonyms)
verbIsInTokenList (_ : ts) = verbIsInTokenList ts

nounIsInTokenList :: [Token] -> Maybe Token
nounIsInTokenList [] = Nothing
nounIsInTokenList ((TokenNoun name) : ts) = Just (TokenNoun name)
nounIsInTokenList (_ : ts) = nounIsInTokenList ts

prepositionIsInTokenList :: [Token] -> Maybe Token
prepositionIsInTokenList [] = Nothing
prepositionIsInTokenList ((TokenPreposition synonyms) : ts) = Just (TokenPreposition synonyms)
prepositionIsInTokenList (_ : ts) = prepositionIsInTokenList ts

makeSimpleSentence :: Maybe Token -> Maybe Token -> Maybe Sentence
makeSimpleSentence (Just verb@(TokenVerb _)) (Just noun@(TokenNoun _))
    = Just (SimpleSentence verb noun)
makeSimpleSentence _ _ = Nothing

makeSimplePrepositionSentence :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe Sentence
makeSimplePrepositionSentence (Just verb@(TokenVerb _)) (Just preposition@(TokenPreposition _)) (Just noun@(TokenNoun _))
    = Just (SimplePrepositionSentence verb preposition noun)
makeSimplePrepositionSentence _ _ _ = Nothing

makeComplexSentence :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe Token -> Maybe Sentence
makeComplexSentence (Just verb@(TokenVerb _)) (Just noun0@(TokenNoun _)) (Just preposition@(TokenPreposition _)) (Just noun1@(TokenNoun _))
    = Just (ComplexSentence verb noun0 preposition noun1)
makeComplexSentence _ _ _ _ = Nothing

makeComplexPrepositionSentence :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe Token -> Maybe Token -> Maybe Sentence
makeComplexPrepositionSentence (Just verb@(TokenVerb _)) (Just preposition0@(TokenPreposition _)) (Just noun0@(TokenNoun _)) (Just preposition1@(TokenPreposition _)) (Just noun1@(TokenNoun _))
    = Just (ComplexPrepositionSentence verb preposition0 noun0 preposition1 noun1)
makeComplexPrepositionSentence _ _ _ _ _ = Nothing

parseSentence :: [TokenMatch] -> Maybe Sentence
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2), (TokenMatch _ t3), (TokenMatch _ t4)]
    = makeComplexPrepositionSentence (verbIsInTokenList t0) (prepositionIsInTokenList t1) (nounIsInTokenList t2) (prepositionIsInTokenList t3) (nounIsInTokenList t4)
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2), (TokenMatch _ t3)]
    = makeComplexSentence (verbIsInTokenList t0) (nounIsInTokenList t1) (prepositionIsInTokenList t2) (nounIsInTokenList t3)
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2)]
    = makeSimplePrepositionSentence (verbIsInTokenList t0) (prepositionIsInTokenList t1) (nounIsInTokenList t2)
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1)]
    = makeSimpleSentence (verbIsInTokenList t0) (nounIsInTokenList t1)
parseSentence _ = Nothing
