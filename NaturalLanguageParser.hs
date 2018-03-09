--NaturalLanguageParser.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language parser

module NaturalLanguageParser (Sentence(..),
                              parseSentence) where

import NaturalLanguageLexer

data Sentence = SimpleSentence Token Token |
                ComplexSentence Token Token Token Token deriving (Show, Eq)

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
makeSimpleSentence (Just verb@(TokenVerb _)) (Just noun0@(TokenNoun _))
    = Just (SimpleSentence verb noun0)
makeSimpleSentence _ _ = Nothing

makeComplexSentence :: Maybe Token -> Maybe Token -> Maybe Token -> Maybe Token -> Maybe Sentence
makeComplexSentence (Just verb@(TokenVerb _)) (Just noun0@(TokenNoun _)) (Just preposition@(TokenPreposition _)) (Just noun1@(TokenNoun _))
    = Just (ComplexSentence verb noun0 preposition noun1)
makeComplexSentence _ _ _ _ = Nothing

parseSentence :: [TokenMatch] -> Maybe Sentence
parseSentence [(TokenMatch s0 t0), (TokenMatch s1 t1), (TokenMatch s2 t2), (TokenMatch s3 t3)]
    = makeComplexSentence (verbIsInTokenList t0) (nounIsInTokenList t1) (prepositionIsInTokenList t2) (nounIsInTokenList t3)
parseSentence [(TokenMatch s0 t0), (TokenMatch s1 t1)]
    = makeSimpleSentence (verbIsInTokenList t0) (nounIsInTokenList t1)
parseSentence _ = Nothing
