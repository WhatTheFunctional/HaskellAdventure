--NaturalLanguageParser.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language parser

module NaturalLanguageParser (Sentence(..),
                              unambiguousSentence,
                              parseSentence) where

import Data.List
import NaturalLanguageLexer

data Sentence = NullSentence | --Exists for completeness
                Phrase Token |
                SimpleSentence Token Token |
                SimplePrepositionSentence Token Token Token |
                ComplexSentence Token Token Token Token |
                ComplexPrepositionSentence Token Token Token Token Token deriving (Show, Eq)

--Helper functions to make sentences from the unambigous token names
--These are intended for use only in the scene definition, not for natural language parsing
findVerb :: [Token] -> String -> [Token]
findVerb verbsList verb
    = case Data.List.find (\(TokenVerb v _) -> v == verb) verbsList of
      Just tokenVerb@(TokenVerb _ _) -> [tokenVerb]
      Nothing -> []

findNoun :: [Token] -> String -> [Token]
findNoun nounsList noun
    = case Data.List.find (\(TokenNoun n _) -> n == noun) nounsList of
      Just tokenNoun@(TokenNoun _ _) -> [tokenNoun]
      Nothing -> []

findPreposition :: [Token] -> String -> [Token]
findPreposition prepositionsList preposition
    = case Data.List.find (\(TokenPreposition p _) -> p == preposition) prepositionsList of
      Just tokenPreposition@(TokenPreposition _ _) -> [tokenPreposition]
      Nothing -> []

makeUnambiguousSentence :: [Token] -> Sentence
makeUnambiguousSentence [verb]
    = Phrase verb
makeUnambiguousSentence [verb, noun]
    = SimpleSentence verb noun
makeUnambiguousSentence [verb, preposition, noun]
    = SimplePrepositionSentence verb preposition noun
makeUnambiguousSentence [verb, noun0, preposition, noun1]
    = ComplexSentence verb noun0 preposition noun1
makeUnambiguousSentence [verb, preposition0, noun0, preposition1, noun1]
    = ComplexPrepositionSentence verb preposition0 noun0 preposition1 noun1
makeUnambiguousSentence _
    = NullSentence

unambiguousSentence :: [Token] -> [Token] -> [Token] -> [String] -> Sentence
unambiguousSentence verbsList nounsList prepositionsList []
    = NullSentence
unambiguousSentence verbsList nounsList prepositionsList [verb]
    = makeUnambiguousSentence (findVerb verbsList verb)
unambiguousSentence verbsList nounsList prepositionsList [verb, noun]
    = makeUnambiguousSentence (concat [(findVerb verbsList verb),
                                       (findNoun nounsList noun)])
unambiguousSentence verbsList nounsList prepositionsList [verb, preposition, noun]
    | (length (findPreposition prepositionsList preposition)) /= 0
        = makeUnambiguousSentence (concat [(findVerb verbsList verb),
                                           (findPreposition prepositionsList preposition),
                                           (findNoun nounsList noun)])
    | otherwise
        = makeUnambiguousSentence (concat [(findVerb verbsList verb),
                                           (findNoun nounsList preposition),
                                           (findNoun nounsList noun)])
unambiguousSentence verbsList nounsList prepositionsList [verb, noun0, preposition, noun1]
    = makeUnambiguousSentence (concat [(findVerb verbsList verb),
                                       (findNoun nounsList noun0),
                                       (findPreposition prepositionsList preposition),
                                       (findNoun nounsList noun1)])
unambiguousSentence verbsList nounsList prepositionsList [verb, preposition0, noun0, preposition1, noun1]
    = makeUnambiguousSentence (concat [(findVerb verbsList verb),
                                       (findPreposition prepositionsList preposition0),
                                       (findNoun nounsList noun0),
                                       (findPreposition prepositionsList preposition1),
                                       (findNoun nounsList noun1)])
unambiguousSentence _ _ _ _
    = NullSentence

--Functions to parse natural language sentences
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
    = (makeSentence [(verbsInTokenList t0),
                     (prepositionsInTokenList t1),
                     (nounsInTokenList t2)]) ++
      (makeSentence [(verbsInTokenList t0),
                     (nounsInTokenList t1),
                     (nounsInTokenList t2)])
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1)]
    = makeSentence [(verbsInTokenList t0),
                    (nounsInTokenList t1)]
parseSentence [(TokenMatch _ t0)]
    = makeSentence [(verbsInTokenList t0)]
parseSentence _ = []
