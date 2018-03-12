--DummyAdventure.hs
--Copyright Laurence Emms 2018
--Module with a dummy vocabulary and adventure for testing

module DummyAdventure (gameIntro, allVerbs, allNouns, allPrepositions, allTokens, startInventory, startFlags, defaultScene, allScenes) where

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph

gameIntro :: String
gameIntro = "Dummy Adventure by Laurence Emms\n"

allVerbs :: [Token]
allVerbs =
    [
        TokenVerb ["get", "take", "pick up"],
        TokenVerb ["put", "place", "put down"],
        TokenVerb ["throw", "pitch"],
        TokenVerb ["give"],
        TokenVerb ["select", "pick"],
        TokenVerb ["look"],
        TokenVerb ["look around"],
        TokenVerb ["use"],
        TokenVerb ["jump"],
        TokenVerb ["move", "walk", "go"],
        TokenVerb ["move down", "walk down", "go down"],
        TokenVerb ["move up", "walk up", "go up"],
        TokenVerb ["run", "jog", "sprint", "dash"],
        TokenVerb ["run down", "jog down", "sprint down", "dash down"],
        TokenVerb ["run up", "jog up", "sprint up", "dash up"],
        TokenVerb ["climb", "scale"],
        TokenVerb ["climb down", "scale down"],
        TokenVerb ["climb up", "scale up"],
        TokenVerb ["unlock"],
        TokenVerb ["lock"],
        TokenVerb ["open"],
        TokenVerb ["close", "shut"],
        TokenVerb ["enter"],
        TokenVerb ["insert"],
        TokenVerb ["remove"],
        TokenVerb ["leave", "exit"],
        TokenVerb ["eat", "consume"],
        TokenVerb ["drink", "consume"]
    ]

allNouns :: [Token]
allNouns =
    [
        TokenNoun ["north"],
        TokenNoun ["south"],
        TokenNoun ["west"],
        TokenNoun ["east"],
        TokenNoun ["staircase"],
        TokenNoun ["stairs"],
        TokenNoun ["up"],
        TokenNoun ["down"],
        TokenNoun ["inside"],
        TokenNoun ["outside"],
        TokenNoun ["door"],
        TokenNoun ["green door"],
        TokenNoun ["white door", "door"],
        TokenNoun ["window"],
        TokenNoun ["chest"],
        TokenNoun ["chair"],
        TokenNoun ["table"],
        TokenNoun ["pen"],
        TokenNoun ["north entrance"],
        TokenNoun ["exit"],
        TokenNoun ["key"],
        TokenNoun ["me"],
        TokenNoun ["myself"],
        TokenNoun ["my hand"],
        TokenNoun ["fork"],
        TokenNoun ["spoon"],
        TokenNoun ["Steve"],
        TokenNoun ["juice"],
        TokenNoun ["cake"]
    ]

allPrepositions :: [Token]
allPrepositions =
    [
        TokenPreposition ["in", "inside", "within"],
        TokenPreposition ["into"],
        TokenPreposition ["out", "outside"],
        TokenPreposition ["on", "on top", "upon"],
        TokenPreposition ["on", "with"],
        TokenPreposition ["above", "over"],
        TokenPreposition ["over"],
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
        TokenPreposition ["together with"]
    ]

allTokens :: [Token]
allTokens = allNouns ++ allVerbs ++ allPrepositions

startInventory :: Inventory
startInventory = Inventory ["fork"]

startFlags :: Flags
startFlags = Flags ["started game"]

scene0 :: Scene
scene0 =
    Scene
    {
        sceneDescription =
            ConditionalDescription [(CTrue, "You're standing in a green room. The room has a <white door>.\n"),
                                    (CNot (InInventory "key"), "There is a <key> on the floor.\n"),
                                    (CNot (FlagSet "opened white door"), "The <white door> is closed.\n"),
                                    (FlagSet "opened white door", "The <white door> is open.\n")],
        interactions =
            [
                Interaction
                {
                    sentences = [SimpleSentence (TokenVerb ["get", "take", "pick up"]) (TokenNoun ["key"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --The player does not have the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You pick up the <key>.")],
                                addedObjects = ObjectChanges ["key"],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --The player has the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You already have the <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimpleSentence (TokenVerb ["open"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already opened.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --The white door is locked and the player has a key
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["unlocked white door", "opened white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "unlocked white door", --The white door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You open the <white door>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["opened white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (CNot (InInventory "key")), --The white door is locked player and the player doesn't have a key
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is locked.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimpleSentence (TokenVerb ["close", "shut"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is open
                                conditionalDescription = ConditionalDescription [(CTrue, "You close the <white door>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges ["opened white door"],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already closed.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimplePrepositionSentence (TokenVerb ["look"]) (TokenPreposition ["at"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is open
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is open.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is closed.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimplePrepositionSentence (TokenVerb ["look"]) (TokenPreposition ["at"]) (TokenNoun ["key"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "There's a <key> on the floor.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [Phrase (TokenVerb ["look around"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You take a look around. You don't like the color scheme of this room.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [ComplexSentence (TokenVerb ["use"]) (TokenNoun ["key"]) (TokenPreposition ["on", "with"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Player does not have a key
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to use with the <white door>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges ["unlocked white door"],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "unlocked white door"), --The player has a key and the door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["unlocked white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door"), --The player has a key and the door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You lock the <white door> with your <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges ["unlocked white door"],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences =
                        [
                            SimpleSentence (TokenVerb ["lock"]) (TokenNoun ["white door", "door"]),
                            ComplexSentence (TokenVerb ["lock"]) (TokenNoun ["white door", "door"]) (TokenPreposition ["with"]) (TokenNoun ["key"])
                        ],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door") `CAnd` (InInventory "key"), --Player has a key and the white door is unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "You lock the <white door> with your <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges ["unlocked white door"],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "unlocked white door"), --The door is already locked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already locked.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to lock the <white door> with.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences =
                        [
                            SimpleSentence (TokenVerb ["unlock"]) (TokenNoun ["white door", "door"]),
                            ComplexSentence (TokenVerb ["unlock"]) (TokenNoun ["white door", "door"]) (TokenPreposition ["with"]) (TokenNoun ["key"])
                        ],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with your <key>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["unlocked white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "unlocked white door"), --The door is already unlocked
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already unlocked.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["unlocked white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to unlock the <white door> with.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [ComplexSentence (TokenVerb ["open"]) (TokenNoun ["white door", "door"]) (TokenPreposition ["with"]) (TokenNoun ["key"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "unlocked white door")) `CAnd` (InInventory "key"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You unlock the <white door> with the <key> and open it.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["unlocked white door", "opened white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "opened white door")) `CAnd` (FlagSet "unlocked white door"), --Player has a key and the white door is locked
                                conditionalDescription = ConditionalDescription [(CTrue, "You open the <white door>.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges ["opened white door"],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is already opened
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is already open.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "key"), --The player does not have the key
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <key> to open the <white door> with.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimplePrepositionSentence (TokenVerb ["move", "walk", "go"]) (TokenPreposition ["through"]) (TokenNoun ["white door", "door"]),
                                 SimplePrepositionSentence (TokenVerb ["leave", "exit"]) (TokenPreposition ["through"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is opened
                                conditionalDescription = ConditionalDescription [(CTrue, "You walk through the <white door>. Congratulations, you escaped the green room!")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Just 1
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is closed.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimplePrepositionSentence (TokenVerb ["run", "jog", "sprint", "dash"]) (TokenPreposition ["through"]) (TokenNoun ["white door", "door"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "opened white door", --The white door is opened
                                conditionalDescription = ConditionalDescription [(CTrue, "You dash through the <white door>. Congratulations, you escaped the green room!")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Just 1
                            },
                            ConditionalAction
                            {
                                condition = CTrue, --Otherwise
                                conditionalDescription = ConditionalDescription [(CTrue, "The <white door> is closed.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
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
                    sentences = [Phrase (TokenVerb ["jump"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You jump up and down in place.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [Phrase (TokenVerb ["move", "walk", "go"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You walk around a bit.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [Phrase (TokenVerb ["run", "jog", "sprint", "dash"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You jog in place.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                },
                Interaction
                {
                    sentences = [SimplePrepositionSentence (TokenVerb ["look"]) (TokenPreposition ["at"]) (TokenNoun ["key"])],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "key", --The key is in your inventory
                                conditionalDescription = ConditionalDescription [(CTrue, "The <key> is in your pocket where you left it.")],
                                addedObjects = ObjectChanges [],
                                removedObjects = ObjectChanges [],
                                setFlags = FlagChanges [],
                                removedFlags = FlagChanges [],
                                nextScene = Nothing
                            }
                        ]
                }
            ]
    }

allScenes :: ([Scene], [SceneIndex])
allScenes = ([scene0, scene1], --List of scenes
             [1]) --End scenes
