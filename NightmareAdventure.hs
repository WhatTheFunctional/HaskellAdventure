--NightmareAdventure.hs
--Copyright Laurence Emms 2018
--Base module for the nightmare adventure game

module NightmareAdventure (gameIntro,
                           allVerbs,
                           allNouns,
                           allPrepositions,
                           allTokens,
                           startInventory,
                           startFlags,
                           defaultScene,
                           allScenes) where

import qualified Data.List

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph

gameIntro :: String
gameIntro = "Nightmare Adventure by Vibha Laljani, Raji Venkat, and Laurence Emms\n"

allVerbs :: [Token]
allVerbs =
    [
        TokenVerb "get" ["get", "take", "pick up"],
        TokenVerb "put" ["put", "place", "put down"],
        TokenVerb "throw" ["throw", "pitch"],
        TokenVerb "give" ["give"],
        TokenVerb "select" ["select", "pick"],
        TokenVerb "look" ["look"],
        TokenVerb "look around" ["look around"],
        TokenVerb "use" ["use"],
        TokenVerb "jump" ["jump"],
        TokenVerb "walk" ["walk", "move", "go"],
        TokenVerb "walk down" ["walk down", "move down", "go down"],
        TokenVerb "walk up" ["walk up", "move up", "go up"],
        TokenVerb "run" ["run", "jog", "sprint", "dash"],
        TokenVerb "run down" ["run down", "jog down", "sprint down", "dash down"],
        TokenVerb "run up" ["run up", "jog up", "sprint up", "dash up"],
        TokenVerb "climb" ["climb", "scale"],
        TokenVerb "climb down" ["climb down", "scale down"],
        TokenVerb "climb up" ["climb up", "scale up"],
        TokenVerb "unlock" ["unlock"],
        TokenVerb "lock" ["lock"],
        TokenVerb "open" ["open"],
        TokenVerb "close" ["close", "shut"],
        TokenVerb "enter" ["enter"],
        TokenVerb "insert" ["insert"],
        TokenVerb "remove" ["remove"],
        TokenVerb "leave" ["leave", "exit"],
        TokenVerb "eat" ["eat", "consume"],
        TokenVerb "drink" ["drink", "consume"]
    ]

allNouns :: [Token]
allNouns =
    [
        TokenNoun "north" ["north"],
        TokenNoun "south" ["south"],
        TokenNoun "west" ["west"],
        TokenNoun "east" ["east"],
        TokenNoun "up" ["up"],
        TokenNoun "down" ["down"],
        TokenNoun "inside" ["inside"],
        TokenNoun "outside" ["outside"],
        TokenNoun "me" ["me"],
        TokenNoun "myself" ["myself"],
        TokenNoun "chrome amulet" ["chrome amulet", "amulet"]
    ]

allPrepositions :: [Token]
allPrepositions =
    [
        TokenPreposition "in" ["in", "inside", "within"],
        TokenPreposition "into" ["into"],
        TokenPreposition "out" ["out", "outside"],
        TokenPreposition "on" ["on", "on top", "upon"],
        TokenPreposition "on" ["on", "with"],
        TokenPreposition "above" ["above", "over"],
        TokenPreposition "over" ["over"],
        TokenPreposition "below" ["below", "under", "underneath", "beneath"],
        TokenPreposition "across" ["across"],
        TokenPreposition "before" ["before"],
        TokenPreposition "after" ["after"],
        TokenPreposition "against" ["against"],
        TokenPreposition "around" ["around"],
        TokenPreposition "among" ["among"],
        TokenPreposition "at" ["at"],
        TokenPreposition "in front" ["in front"],
        TokenPreposition "from" ["from"],
        TokenPreposition "behind" ["behind"],
        TokenPreposition "beside" ["beside", "by", "next to"],
        TokenPreposition "beyond" ["beyond"],
        TokenPreposition "near" ["near"],
        TokenPreposition "down" ["down"],
        TokenPreposition "up" ["up"],
        TokenPreposition "past" ["past"],
        TokenPreposition "through" ["through"],
        TokenPreposition "to" ["to", "toward"],
        TokenPreposition "until" ["until"],
        TokenPreposition "with" ["with"],
        TokenPreposition "together with" ["together with"]
    ]

--Helper function to make unambiguous sentences
uSentence :: [String] -> Sentence
uSentence words = unambiguousSentence allVerbs allNouns allPrepositions words

allTokens :: [Token]
allTokens = allNouns ++ allVerbs ++ allPrepositions

startInventory :: Inventory
startInventory = Inventory ["chrome amulet"]

startFlags :: Flags
startFlags = Flags []

scene0 :: Scene
scene0 =
    Scene
    {
        sceneDescription =
            ConditionalDescription [(CNot (FlagSet "intro"), "After a long day working at the moisture farms outside of the village, you return home. You open the door to your mud-brick cottage to find your brother lying on the dirt floor just inside the <doorway>.\nAt first, you fear the worst; life on the edge of The Beyond can be dangerous.\nBut as you lean down to inspect your brother, <Jorryn>, you realize that he is not dead, but asleep.\nYou rush to check your parents, who are also on the floor by the stove, they are also asleep.\nNo amount of shaking or shouting seems to wake them.", [SetFlag "intro"])],
        interactions =
            [
            ]
    }

winScene :: Scene
winScene =
    Scene
    {
        sceneDescription = ConditionalDescription [],
        interactions = []
    }

defaultScene :: Scene
defaultScene =
    Scene
    {
        sceneDescription = ConditionalDescription [],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["jump"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You jump up and down in place.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You walk around a bit.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["run"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You jog in place.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "chrome amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "chrome amulet", --The key is in your inventory
                                conditionalDescription = ConditionalDescription [(CTrue, "You are wearing the Chrome Amulet your grandfather gave you as a child. It shimmers as you inspect it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "me"],
                                 uSentence ["look", "at", "myself"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You look fine.", [])],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

allScenes :: ([Scene], [SceneIndex])
allScenes = ([scene0, winScene], --List of scenes
             [1]) --End scenes
