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
        TokenVerb "inspect" ["inspect"],
        TokenVerb "look around" ["look around"],
        TokenVerb "use" ["use"],
        TokenVerb "jump" ["jump"],
        TokenVerb "walk" ["walk", "move", "go"],
        TokenVerb "walk down" ["walk down", "move down", "go down"],
        TokenVerb "walk up" ["walk up", "move up", "go up"],
        TokenVerb "run" ["run", "jog", "sprint", "dash"],
        TokenVerb "run down" ["run down", "jog down", "sprint down", "dash down"],
        TokenVerb "run up" ["run up", "jog up", "sprint up", "dash up"],
        TokenVerb "dance" ["dance"],
        TokenVerb "smile" ["smile", "grin"],
        TokenVerb "frown" ["frown"],
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
        TokenVerb "drink" ["drink", "consume"],
        TokenVerb "do something" ["do something"]
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
        TokenNoun "chrome amulet" ["chrome amulet", "amulet"],
        TokenNoun "door" ["door", "front door", "doorway"],
        TokenNoun "home" ["home", "my house", "cottage", "mud-brick cottage"],
        TokenNoun "tower" ["tower", "aeon tower", "crystal tower"],
        TokenNoun "square" ["square", "aldeia square"]
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

introString :: String
introString = "It's been a long day working at the moisture farms outside of your village, your aldeia. You return home just as the sun is setting. You open the door to your mud-brick cottage to find your brother, <Jorryn>, lying on the dirt floor just inside the <front door>.\nAt first, you fear the worst; life on the edge of The Beyond can be dangerous.\nBut as you lean down to inspect your brother you realize that he is not dead, but asleep.\nYou rush to check your parents, who are also on the floor by the stove, they are also asleep.\nNo amount of shaking or shouting seems to wake them."

cottageDescriptionString :: String
cottageDescriptionString = "You're standing in your home. <Jorryn> is lying by the <front door> to the <east>. Your parents are on the floor by the <stove>. Your room is to the <west>."

cottageScene :: Scene
cottageScene =
    Scene
    {
        sceneDescription =
            ConditionalDescription
            [
                (CNot (FlagSet "intro"), introString, [SetFlag "intro"]),
                (CNot (FlagSet "cottage described"), cottageDescriptionString, [SetFlag "cottage described"])
            ],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["look", "at", "home"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, cottageDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "east"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You peer out of your <front door>. You see the path to the <aldeia square>.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "through", "door"],
                                 uSentence ["leave", "through", "door"],
                                 uSentence ["leave", "out", "door"],
                                 uSentence ["walk", "out", "door"],
                                 uSentence ["walk", "outside"],
                                 uSentence ["walk", "east"],
                                 uSentence ["leave", "east"],
                                 uSentence ["leave"],
                                 uSentence ["leave", "home"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (FlagSet "square visited", "You walk through the front door of your home out into the aldeia. Everyone you pass is still asleep. Your friend, <Evanna>, is still asleep at the base of the <Ancient Clock>.", []),
                                            (CNot (FlagSet "square visited"), "You walk through the front door of your home out into your aldeia. As you walk through the aldeia, you come across several people asleep on the ground. You arrive at the aldeia square and you notice that, for the first time in your life, the <Ancient Clock> has stopped. You see your friend, <Evanna>, walking slowly through the square in a dazed stupor. As you approach, she collapses to the ground. The <Ancient Clock> chimes and her body starts to glow a deep green color, the glowing aura shoots quickly into the <Ancient Clock> and it falls silent.", [])
                                        ],
                                stateChanges = [SceneChange 2, SetFlag "square visited", RemoveFlag "cottage described"]
                            }
                        ]
                }
            ]
    }

winScene :: Scene
winScene =
    Scene
    {
        sceneDescription = ConditionalDescription [],
        interactions = []
    }

aldeiaDescriptionString :: String
aldeiaDescriptionString = "You are standing in the <aldeia square>. It is a dusty open area surrounded by mud-brick buildings. The <Ancient Clock> stands silent in the center of the square. Your friend, <Evanna>, is lying at the base of the clock. Your <home> is to the <west>. You see the <Aeon Tower> to the <south> at the edge of the Kethar desert."

aldeiaScene :: Scene
aldeiaScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "aldeia described"), aldeiaDescriptionString, [SetFlag "aldeia described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["look", "at", "square"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, aldeiaDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "west"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see the path back <home>", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "south"],
                                 uSentence ["look", "at", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see the <Aeon Tower>, a single massive crystal growing out of the Kethar desert. It serves as Isvald's home.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "west"],
                                 uSentence ["walk", "home"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk through the deserted streets of your aldeia, passing several sleeping bodies on your way home.", [RemoveFlag "aldeia described"])
                                        ],
                                stateChanges = [SceneChange 0]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "south"],
                                 uSentence ["walk", "to", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk towards the looming <Aeon Tower>.", [RemoveFlag "aldeia described"])
                                        ],
                                stateChanges = [SceneChange 3]
                            }
                        ]
                }
            ]
    }

towerDescriptionString :: String
towerDescriptionString = "You are standing at the base of the <Aeon Tower>, inhabited by Isvald, the aldeia's resident Aeon Priest. The tower is a giant spiral of blue crystal, maybe 100 feet tall, with no visible entrances. Beyond the tower is Kethar desert, which stretches to the horizon. The <aledeia square> is to the <north>."

towerScene :: Scene
towerScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "tower described"), towerDescriptionString, [SetFlag "tower described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["look", "at", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, towerDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "north"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see your aldeia. A dirt path leads from the <Aeon Tower> back to the <aldeia square>", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "north"],
                                 uSentence ["walk", "to", "square"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk back to the <aldeia square>.", [RemoveFlag "tower described"])
                                        ],
                                stateChanges = [SceneChange 2]
                            }
                        ]
                }
            ]
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
                    sentences = [uSentence ["dance"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You dance a happy dance.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["smile"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You smile.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["frown"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You frown.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["do something"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "That does something.", [])],
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
allScenes = ([cottageScene, winScene, aldeiaScene, towerScene], --List of scenes
             [1]) --End scenes
