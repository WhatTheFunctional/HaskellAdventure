--NaturalLanguageParser.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language parser

module NaturalLanguageParser (Sentence(..),
                              parseSentence) where

import Data.List
import NaturalLanguageLexer

data Sentence = NullSentence | --Exists for completeness
                Phrase Token |
                SimpleSentence Token Token |
                SimplePrepositionSentence Token Token Token |
                ComplexSentence Token Token Token Token |
                ComplexPrepositionSentence Token Token Token Token Token deriving (Show, Eq)

verbsInTokenList :: [Token] -> [Token]
verbsInTokenList [] = []
verbsInTokenList (verb@(TokenVerb _ _) : ts) = verb : verbsInTokenList ts
verbsInTokenList (_ : ts) = verbsInTokenList ts

nounsInTokenList :: [Token] -> [Token]
nounsInTokenList [] = []
nounsInTokenList (noun@(TokenNoun _ _) : ts) = noun : nounsInTokenList ts
nounsInTokenList (_ : ts) = nounsInTokenList ts

prepositionsInTokenList :: [Token] -> [Token]
prepositionsInTokenList [] = []
prepositionsInTokenList (preposition@(TokenPreposition _ _) : ts) = preposition : prepositionsInTokenList ts
prepositionsInTokenList (_ : ts) = prepositionsInTokenList ts

makeSentence :: [[Token]] -> [Sentence]
makeSentence []
    = []
makeSentence [verbs]
    = fmap (\verb -> Phrase verb) verbs
makeSentence [verbs, nouns]
    = fmap (\[verb, noun] -> SimpleSentence verb noun) 
      ((:) <$> verbs <*>
           ((:) <$> nouns <*> [[]]))
makeSentence [verbs, prepositions, nouns]
    = fmap (\[verb, preposition, noun] -> SimplePrepositionSentence verb preposition noun)
      ((:) <$> verbs <*>
          ((:) <$> prepositions <*>
              ((:) <$> nouns <*> [[]])))
makeSentence [verbs, nouns0, prepositions, nouns1]
    = fmap (\[verb, noun0, preposition, noun1] -> ComplexSentence verb noun0 preposition noun1)
      ((:) <$> verbs <*>
          ((:) <$> nouns0 <*>
              ((:) <$> prepositions <*>
                  ((:) <$> nouns1 <*> [[]]))))
makeSentence [verbs, prepositions0, nouns0, prepositions1, nouns1]
    = fmap (\[verb, preposition0, noun0, preposition1, noun1] -> ComplexPrepositionSentence verb preposition0 noun0 preposition1 noun1)
      ((:) <$> verbs <*>
          ((:) <$> prepositions0 <*>
              ((:) <$> nouns0 <*>
                  ((:) <$> prepositions1 <*>
                      ((:) <$> nouns1 <*> [[]])))))

parseSentence :: [TokenMatch] -> [Sentence]
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2), (TokenMatch _ t3), (TokenMatch _ t4)]
    = makeSentence [(verbsInTokenList t0),
                    (prepositionsInTokenList t1),
                    (nounsInTokenList t2),
                    (prepositionsInTokenList t3),
                    (nounsInTokenList t4)]
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2), (TokenMatch _ t3)]
    = makeSentence [(verbsInTokenList t0),
                    (nounsInTokenList t1),
                    (prepositionsInTokenList t2),
                    (nounsInTokenList t3)]
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1), (TokenMatch _ t2)]
    = makeSentence [(verbsInTokenList t0),
                    (prepositionsInTokenList t1),
                    (nounsInTokenList t2)]
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1)]
    = makeSentence [(verbsInTokenList t0),
                    (nounsInTokenList t1)]
parseSentence [(TokenMatch _ t0)]
    = makeSentence [(verbsInTokenList t0)]
parseSentence _ = []
