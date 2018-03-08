--DummyAdventure.hs
--Copyright Laurence Emms 2018
--Module with a dummy vocabulary and adventure for testing

module DummyAdventure (allVerbs, allNouns, allPrepositions, allTokens) where

import NaturalLanguageLexer

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
