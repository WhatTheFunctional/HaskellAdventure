--DummyAdventure.hs
--Copyright Laurence Emms 2018
--Module with a dummy vocabulary and adventure for testing

module DummyAdventure (allVerbs, allNouns, allPrepositions, allTokens) where

import NaturalLanguageLexer

allVerbs :: [Token]
allVerbs = [TokenVerb ["get", "take", "pick up"],
            TokenVerb ["put", "place", "put down"],
            TokenVerb ["throw", "pitch"],
            TokenVerb ["give"],
            TokenVerb ["select", "pick"],
            TokenVerb ["use"],
            TokenVerb ["jump"],
            TokenVerb ["move", "walk", "go", "run"],
            TokenVerb ["move down", "walk down", "go down", "run down"],
            TokenVerb ["move up", "walk up", "go up", "run up"],
            TokenVerb ["climb", "scale"],
            TokenVerb ["climb down", "scale down"],
            TokenVerb ["climb up", "scale up"],
            TokenVerb ["open"],
            TokenVerb ["close", "shut"],
            TokenVerb ["enter"],
            TokenVerb ["leave", "exit"],
            TokenVerb ["eat", "consume"],
            TokenVerb ["drink", "consume"]]

allNouns :: [Token]
allNouns = [TokenNoun "north",
            TokenNoun "south",
            TokenNoun "west",
            TokenNoun "east",
            TokenNoun "staircase",
            TokenNoun "stairs",
            TokenNoun "up",
            TokenNoun "down",
            TokenNoun "inside",
            TokenNoun "outside",
            TokenNoun "door",
            TokenNoun "green door",
            TokenNoun "window",
            TokenNoun "chest",
            TokenNoun "chair",
            TokenNoun "table",
            TokenNoun "north entrance",
            TokenNoun "exit",
            TokenNoun "me",
            TokenNoun "myself",
            TokenNoun "my hand",
            TokenNoun "fork",
            TokenNoun "spoon",
            TokenNoun "Steve",
            TokenNoun "juice",
            TokenNoun "cake"]

allPrepositions :: [Token]
allPrepositions = [TokenPreposition ["in", "inside", "within"],
                   TokenPreposition ["into"],
                   TokenPreposition ["out", "outside"],
                   TokenPreposition ["on", "on top", "upon"],
                   TokenPreposition ["above", "over"],
                   TokenPreposition ["below", "under", "underneath", "beneath"],
                   TokenPreposition ["across"],
                   TokenPreposition ["before"],
                   TokenPreposition ["after"],
                   TokenPreposition ["against"],
                   TokenPreposition ["around"],
                   TokenPreposition ["among"],
                   TokenPreposition ["at"],
                   TokenPreposition ["in front"],
                   TokenPreposition ["from"],
                   TokenPreposition ["behind"],
                   TokenPreposition ["beside", "by", "next to"],
                   TokenPreposition ["beyond"],
                   TokenPreposition ["near"],
                   TokenPreposition ["down"],
                   TokenPreposition ["up"],
                   TokenPreposition ["past"],
                   TokenPreposition ["through"],
                   TokenPreposition ["to", "toward"],
                   TokenPreposition ["until"],
                   TokenPreposition ["with"],
                   TokenPreposition ["together with"]]

allTokens :: [Token]
allTokens = allNouns ++ allVerbs ++ allPrepositions
