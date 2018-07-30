--DummyAdventure.hs
--Copyright Laurence Emms 2018
--Module with a dummy vocabulary and adventure for testing

module DummyAdventure (gameIntro, allVerbs, allNouns, allPrepositions, allTokens, startScene, startInventory, startFlags, defaultScene, allScenes) where

import qualified Data.List

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph

gameIntro :: String
gameIntro = "Dummy Adventure by Laurence Emms\n"

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
        TokenNoun "staircase" ["staircase"],
        TokenNoun "stairs" ["stairs"],
        TokenNoun "up" ["up"],
        TokenNoun "down" ["down"],
        TokenNoun "inside" ["inside"],
        TokenNoun "outside" ["outside"],
        TokenNoun "door" ["door"],
        TokenNoun "green door" ["green door"],
        TokenNoun "white door" ["white door", "door"],
        TokenNoun "green room" ["green room", "room"],
        TokenNoun "window" ["window"],
        TokenNoun "chest" ["chest"],
        TokenNoun "chair" ["chair"],
        TokenNoun "table" ["table"],
        TokenNoun "pen" ["pen"],
        TokenNoun "north entrance" ["north entrance"],
        TokenNoun "exit" ["exit"],
        TokenNoun "key" ["key"],
        TokenNoun "me" ["me"],
        TokenNoun "myself" ["myself"],
        TokenNoun "my hand" ["my hand"],
        TokenNoun "fork" ["fork"],
        TokenNoun "spoon" ["spoon"],
        TokenNoun "Steve" ["Steve"],
        TokenNoun "juice" ["juice"],
        TokenNoun "cake" ["cake"]
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

startScene :: String
startScene = "scene0"

startInventory :: Inventory
startInventory = Inventory ["fork"]

startFlags :: Flags
startFlags = Flags ["started game"]

scene0 :: Scene
scene0 =
    Scene
    {
        sceneDescription =
            ConditionalDescription [(CTrue, "You're standing in a green room. The room has a <white door>.", []),
                                    (CNot (FlagSet "opened white door"), "The <white door> is closed.", []),
                                    (FlagSet "opened white door", "The <white door> is open.", []),
                                    (CNot (InInventory "key"), "There is a <key> on the floor.", [])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["get", "key"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --The player does not have the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You pick up the <key>.", [])],
                                stateChanges = [AddToInventory "key"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --The player has the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You already have the <key>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["open", "white door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already opened.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --The white door is locked and the player has a key
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.", [])],
                                stateChanges = [SetFlag "unlocked white door", SetFlag "opened white door"]
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "unlocked white door", --The white door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You open the <white door>.", [])],
                                stateChanges = [SetFlag "opened white door"]
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (CNot (InInventory "key")), --The white door is locked player and the player doesn't have a key
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is locked.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["close", "white door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is open
                                conditionalDescription = ConditionalDescription [(CTrue, "You close the <white door>.", [])],
                                stateChanges = [RemoveFlag "opened white door"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already closed.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "white door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is open
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is open.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is closed.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "key"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "There's a <key> on the floor.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look around"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You take a look around. You don't like the color scheme of this room.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["use", "key", "on", "white door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Player does not have a key
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to use with the <white door>.", [])],
                                stateChanges = [RemoveFlag "unlocked white door"]
                            },
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "unlocked white door"), --The player has a key and the door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.", [])],
                                stateChanges = [SetFlag "unlocked white door"]
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door"), --The player has a key and the door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You lock the <white door> with your <key>.", [])],
                                stateChanges = [RemoveFlag "unlocked white door"]
                            }
                        ]
                },
                Interaction
                {
                    sentences =
                        [
                            uSentence ["lock", "white door"],
                            uSentence ["lock", "white door", "with", "key"]
                        ],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door") `CAnd` (InInventory "key"), --Player has a key and the white door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You lock the <white door> with your <key>.", [])],
                                stateChanges = [RemoveFlag "unlocked white door"]
                            },
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "unlocked white door"), --The door is already locked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already locked.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to lock the <white door> with.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences =
                        [
                            uSentence ["unlock", "white door"],
                            uSentence ["unlock", "white door", "with", "key"]
                        ],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.", [])],
                                stateChanges = [SetFlag "unlocked white door"]
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door"), --The door is already unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already unlocked.", [])],
                                stateChanges = [SetFlag "unlocked white door"]
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to unlock the <white door> with.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["open", "white door", "with", "key"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with the <key> and open it.", [])],
                                stateChanges = [SetFlag "unlocked white door", SetFlag "opened white door"]
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "opened white door")) `CAnd` (FlagSet "unlocked white door"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You open the <white door>.", [])],
                                stateChanges = [SetFlag "opened white door"]
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is already opened
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already open.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --The player does not have the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to open the <white door> with.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "through", "white door"],
                                 uSentence ["leave", "through", "white door"],
                                 uSentence ["leave", "out", "white door"],
                                 uSentence ["walk", "out", "white door"],
                                 uSentence ["walk", "outside"],
                                 uSentence ["leave"],
                                 uSentence ["leave", "green room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is opened
                                conditionalDescription = ConditionalDescription [(CTrue, "You walk through the <white door>. Congratulations, you escaped the green room!", [])],
                                stateChanges = [SceneChange 1]
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You can't leave. The <white door> is closed.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["run", "through", "white door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is opened
                                conditionalDescription = ConditionalDescription [(CTrue, "You dash through the <white door>. Congratulations, you escaped the green room!", [])],
                                stateChanges = [SceneChange 1]
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You can't leave. The <white door> is closed.", [])],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

scene1 :: Scene
scene1 =
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
                    sentences = [uSentence ["look", "at", "key"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "key", --The key is in your inventory
                                conditionalDescription = ConditionalDescription [(CTrue, "The <key> is in your pocket where you left it.", [])],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

allScenes :: (Data.Map.Map SceneKey Scene, [SceneKey])
allScenes = (Data.Map.fromList [("scene0", scene0), ("scene1", scene1)], --List of scenes
             ["scene1"]) --End scenes
