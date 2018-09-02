--NightmareAdventure.hs
--Copyright Laurence Emms 2018
--Base module for the nightmare adventure game

module NightmareAdventure (gameIntro,
                           allVerbs,
                           allNouns,
                           allPrepositions,
                           allTokens,
                           startScene,
                           startInventory,
                           startFlags,
                           defaultScene,
                           allScenes) where

import qualified Data.List
import qualified Data.Map

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph

gameIntro :: String
gameIntro = "Nightmare Adventure by Vibha Laljani, Raji Venkat, and Laurence Emms\n\n"

allVerbs :: [Token]
allVerbs =
    [
        TokenVerb "get" ["get", "take", "pick up", "pluck"],
        TokenVerb "put" ["put", "place", "put down"],
        TokenVerb "throw" ["throw", "pitch"],
        TokenVerb "give" ["give"],
        TokenVerb "select" ["select", "pick"],
        TokenVerb "look" ["look", "see", "view", "scan", "spy", "observe"],
        TokenVerb "inspect" ["inspect", "check out", "observe", "scan"],
        TokenVerb "look around" ["look around"],
        TokenVerb "use" ["use"],
        TokenVerb "jump" ["jump"],
        TokenVerb "sit" ["sit"],
        TokenVerb "lie down" ["lie", "lie down"],
        TokenVerb "go" ["move", "proceed", "leave", "exit"],
        TokenVerb "walk" ["walk", "stride", "strut", "step", "hike", "trot", "stroll", "march", "amble", "saunter", "trek", "wander", "trudge", "perambulate", "plod", "traverse", "prance", "promenade", "perambulate", "tread", "traipse", "hoof it", "move", "go", "leave", "exit"],
        TokenVerb "walk down" ["walk down", "move down", "go down"],
        TokenVerb "walk up" ["walk up", "move up", "go up"],
        TokenVerb "approach" ["approach"],
        TokenVerb "fly" ["fly", "float", "drift", "glide", "move", "go"],
        TokenVerb "run" ["run", "race", "jog", "sprint", "dash"],
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
        TokenVerb "enter" ["enter", "get into"],
        TokenVerb "insert" ["insert"],
        TokenVerb "remove" ["remove"],
        TokenVerb "leave" ["leave", "exit", "get out"],
        TokenVerb "eat" ["eat", "consume"],
        TokenVerb "drink" ["drink", "consume"],
        TokenVerb "do something" ["do something"],
        TokenVerb "do nothing" ["do nothing"],
        TokenVerb "press" ["press", "hit", "push", "depress", "poke"],
        TokenVerb "lift" ["lift", "remove"],
        TokenVerb "touch" ["touch", "stroke", "grab", "feel", "handle", "pat", "brush", "tap"],
        TokenVerb "ask" ["ask", "question", "query", "inquire", "quiz", "interrogate"],
        TokenVerb "talk" ["talk", "chat", "converse", "communicate", "speak", "parley"],
        TokenVerb "call" ["call", "summon"],
        TokenVerb "buy" ["buy", "purchase"],
        TokenVerb "fall asleep" ["fall asleep"],
        TokenVerb "sleep" ["sleep"],
        TokenVerb "fix" ["fix", "repair"]
    ]

allNouns :: [Token]
allNouns =
    [
        TokenNoun "north" ["north"], --General nouns
        TokenNoun "south" ["south"],
        TokenNoun "west" ["west"],
        TokenNoun "east" ["east"],
        TokenNoun "up" ["up"],
        TokenNoun "down" ["down"],
        TokenNoun "inside" ["inside"],
        TokenNoun "outside" ["outside"],
        TokenNoun "me" ["me"],
        TokenNoun "myself" ["myself"],
        TokenNoun "chrome amulet" ["chrome amulet"],
        TokenNoun "front door" ["front door"],
        TokenNoun "door" ["door", "doorway"],
        TokenNoun "home" ["home", "my house", "cottage", "mud-brick cottage"], --Cottage nouns
        TokenNoun "Jorryn" ["Jorryn"],
        TokenNoun "parents" ["parents"],
        TokenNoun "square" ["square", "village square"], --Village square nouns
        TokenNoun "clock" ["ancient clock", "clock"],
        TokenNoun "Evanna" ["Evanna"],
        TokenNoun "jade amulet" ["jade amulet"],
        TokenNoun "tower" ["tower", "wizard's tower", "crystal tower"], --Tower nouns
        TokenNoun "pedestal" ["pedestal"],
        TokenNoun "indentation" ["indentation"],
        TokenNoun "chrome indentation" ["chrome indentation"],
        TokenNoun "jade indentation" ["jade indentation"],
        TokenNoun "ruby indentation" ["ruby indentation"],
        TokenNoun "ruby amulet" ["ruby amulet"],
        TokenNoun "gateway" ["gateway", "gate", "cloaked gateway"],
        TokenNoun "elevator" ["elevator"],
        TokenNoun "ground floor" ["ground floor"],
        TokenNoun "wizard" ["wizard", "Isvald"],
        TokenNoun "bedroom" ["bedroom"],
        TokenNoun "ground floor" ["ground floor"],
        TokenNoun "music room" ["music room"],
        TokenNoun "buest room" ["guest room"],
        TokenNoun "button panel" ["panel", "button panel"],
        TokenNoun "button" ["button"],
        TokenNoun "gear" ["gear"],
        TokenNoun "chest of drawers" ["chest of drawers", "drawers"],
        TokenNoun "bed" ["bed"],
        TokenNoun "gramophone" ["gramophone", "phonograph"],
        TokenNoun "needle" ["needle", "stylus"],
        TokenNoun "star" ["star", "stars"], -- Star field nouns
        TokenNoun "clock constellation" ["clock", "clock constellation"],
        TokenNoun "hypnotism constellation" ["hypnotism", "hypnotism constellation"],
        TokenNoun "hypnosis" ["hypnosis", "hypnotism"],
        TokenNoun "cupcake constellation" ["cupcake", "cup cake", "cupcake constellation", "cup cake constellation"],
        TokenNoun "rabbit" ["rabbit", "white rabbit"],
        TokenNoun "couch" ["couch", "red couch"],
        TokenNoun "star field" ["star field"],
        TokenNoun "pendulum" ["pendulum"],
        TokenNoun "carrot cake" ["carrot cake"],
        TokenNoun "chocolate cake" ["chocolate cake"],
        TokenNoun "lemon cake" ["lemon cake"],
        TokenNoun "left cake" ["left cake"],
        TokenNoun "middle cake" ["middle cake"],
        TokenNoun "right cake" ["right cake"],
        TokenNoun "red card" ["red card"],
        TokenNoun "green card" ["green card"],
        TokenNoun "blue card" ["blue card"],
        TokenNoun "table" ["table"],
        TokenNoun "cupcake" ["cupcake"],
        TokenNoun "elderly woman" ["elderly woman", "old woman", "elderly lady", "old lady"]
    ]

allPrepositions :: [Token]
allPrepositions =
    [
        TokenPreposition "in" ["in", "inside", "within"],
        TokenPreposition "into" ["into", "in", "inside"],
        TokenPreposition "out" ["out", "outside"],
        TokenPreposition "upon" ["on", "on top", "upon"],
        TokenPreposition "above" ["above", "over"],
        TokenPreposition "over" ["over"],
        TokenPreposition "below" ["below", "under", "underneath", "beneath"],
        TokenPreposition "across" ["across"],
        TokenPreposition "before" ["before"],
        TokenPreposition "after" ["after"],
        TokenPreposition "against" ["against"],
        TokenPreposition "around" ["around"],
        TokenPreposition "among" ["among"],
        TokenPreposition "about" ["about"],
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
        TokenPreposition "using" ["using"],
        TokenPreposition "by" ["by"],
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
startScene = "cottage"

startInventory :: Inventory
startInventory = Inventory ["chrome amulet"]

startFlags :: Flags
startFlags = Flags []

introString :: String
introString = "It's been a long day working at the moisture farms outside of your <village>. You return home just as the sun is setting. You open the door to your mud-brick cottage to find your brother, <Jorryn>, lying on the dirt floor just inside the <front door>.\nAt first, you fear the worst; life on the edge of The Beyond can be dangerous.\nBut as you lean down to inspect your brother you realize that he is not dead, but asleep.\nYou rush to check your <parents>, who are also on the floor by the stove, they are also asleep.\nNo amount of shaking or shouting seems to wake them."

cottageDescriptionString :: String
cottageDescriptionString = "You're standing in your home. <Jorryn> is lying by the <front door> to the <east>. Your <parents> are on the floor by the stove."

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
                    sentences = [uSentence ["look", "at", "parents"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "Your <parents> are unhurt but you can't wake them from their slumber.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "Jorryn lies asleep on the floor, you can't seem to wake him.", [])
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
                                        (CNot (FlagSet "front door closed"), "You peer out of your <front door>. You see the path to the <village square>.", []),
                                        ((FlagSet "front door closed"), "You look at your <front door>. It's closed.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "door"],
                                 uSentence ["look", "at", "front door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (FlagSet "front door closed"), "It's your <front door>. The door is open.", []),
                                        ((FlagSet "front door closed"), "It's your <front door>. The door is closed.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["close", "door"],
                                 uSentence ["close", "front door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (FlagSet "front door closed"), "You close your front door.", []),
                                        ((FlagSet "front door closed"), "Your front door is already closed.", [])
                                    ],
                                stateChanges = [SetFlag "front door closed"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["open", "door"],
                                 uSentence ["open", "front door"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (FlagSet "front door closed"), "Your front door is already open.", []),
                                        ((FlagSet "front door closed"), "You open your front door.", [])
                                    ],
                                stateChanges = [RemoveFlag "front door closed"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "through", "door"],
                                 uSentence ["walk", "out", "door"],
                                 uSentence ["walk", "through", "front door"],
                                 uSentence ["walk", "out", "front door"],
                                 uSentence ["walk", "outside"],
                                 uSentence ["walk", "east"],
                                 uSentence ["leave"],
                                 uSentence ["leave", "front door"],
                                 uSentence ["leave", "home"],
                                 uSentence ["walk", "to", "square"],
                                 uSentence ["approach", "square"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "front door closed"),
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (FlagSet "square visited", "You walk through the front door of your home out into the <village>. Everyone you pass is still asleep. Your friend, <Evanna>, is still asleep at the base of the <Ancient Clock>.", []),
                                            (CNot (FlagSet "square visited"), "You walk through the front door of your home out into your <village>. As you walk through the <village>, you come across several people asleep on the ground. You arrive at the <village square> and you notice that, for the first time in your life, the <Ancient Clock> has stopped. You see your friend, <Evanna>, walking slowly through the square in a dazed stupor. As you approach, she collapses to the ground. The <Ancient Clock> chimes and her body starts to glow a deep green color, the glowing aura shoots quickly into the <Ancient Clock> and it falls silent.", [])
                                        ],
                                stateChanges = [SceneChange "village", SetFlag "square visited", RemoveFlag "cottage described"]
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "front door closed",
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "Your front door is closed, blocking your way.", [])
                                        ],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

winScene :: Scene
winScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CTrue, "Everything swirls around, and you find yourself back in your cottage with your parents, and Jorryn. Jorryn is stretching out of his sleep. \"I feel so refreshed! Get ready, we have to go tend to the crops!\", he says.", [SceneChange "end"])],
        interactions = []
    }

loseScene :: Scene
loseScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CTrue, "Game over.", [SceneChange "end"])],
        interactions = []
    }

endScene :: Scene
endScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CTrue, "", [])],
        interactions = []
    }

villageDescriptionString :: String
villageDescriptionString = "You are standing in the <village square>. It is a dusty open area surrounded by mud-brick buildings. The <Ancient Clock> stands silent in the center of the square. Your friend, <Evanna>, is lying at the base of the clock. Your <home> is to the <west>. You see the <wizard's tower> to the <south> at the edge of the Kethar desert."

villageScene :: Scene
villageScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "village described"), villageDescriptionString, [SetFlag "village described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look", "at", "clock"],
                                 uSentence ["inspect", "clock"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You inspect the <Ancient Clock>. It appears to be carved out of a block of marble and has stood on this spot, steadily ticking since long before your tribe came to settle in The Beyond. The clock is not ticking, and the hands have stopped. You notice that something appears to have bored a perfectly round hole in the marble. Inside, you see that the clock operates with a complicated set of gears. One of the gears appears to be missing.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "Evanna"],
                                 uSentence ["inspect", "Evanna"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "jade amulet"), "Your friend <Evanna>, lies in a deep slumber at the base of the Ancient clock. You call her name and shake her, but nothing you do can wake her. You notice that her <jade amulet> is gently glowing.", []),
                                        (InInventory "jade amulet", "Your friend <Evanna>, lies in a deep slumber at the base of the Ancient clock. You call her name and shake her, but nothing you do can wake her.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "jade amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "jade amulet"), "You reach down and remove <Evanna>'s <jade amulet>.", []),
                                        (InInventory "jade amulet", "You have already taken <Evanna>'s <jade amulet>.", [])
                                    ],
                                stateChanges = [AddToInventory "jade amulet"]
                            }
                        ]
                },
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
                                        (CTrue, villageDescriptionString, [])
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
                                        (CTrue, "You see the <wizard's tower>, a single massive crystal growing out of the Kethar desert. It serves as <Isvald>'s home.", [])
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
                                            (CTrue, "You walk through the deserted streets of your <village>, passing several sleeping bodies on your way home.", [RemoveFlag "village described"])
                                        ],
                                stateChanges = [SceneChange "cottage"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "south"],
                                 uSentence ["walk", "to", "tower"],
                                 uSentence ["approach", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk towards the looming <wizard's tower>.", [RemoveFlag "village described"])
                                        ],
                                stateChanges = [SceneChange "tower"]
                            }
                        ]
                }
            ]
    }

towerExteriorDescriptionString :: String
towerExteriorDescriptionString = "You are standing at the base of the <wizard's tower>, inhabited by <Isvald>, the <village>'s resident <wizard>. The tower is a giant spiral of blue crystal, maybe 100 feet tall, with no visible entrances. Beyond the tower is Kethar desert, which stretches to the horizon. The <aledeia square> is to the <north>. In front of you is a <pedestal> with some strange <indentations>."

gatewayDescriptionString :: String
gatewayDescriptionString = " A cloaked <gateway> has been opened in the side of the tower."

towerExteriorScene :: Scene
towerExteriorScene =
    Scene
    {
        sceneDescription =
            ConditionalDescription [(CAnd (CNot (FlagSet "tower described")) (CNot (FlagSet "gateway opened")), towerExteriorDescriptionString, [SetFlag "tower described"]),
                                    (CAnd (CNot (FlagSet "tower described")) (FlagSet "gateway opened"), towerExteriorDescriptionString ++ gatewayDescriptionString, [SetFlag "tower described"])],
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
                                        (CTrue, towerExteriorDescriptionString, []),
                                        (FlagSet "gateway opened", gatewayDescriptionString, [])
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
                                        (CTrue, "You see your <village>. A dirt path leads from the <wizard's tower> back to the <village square>", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "north"],
                                 uSentence ["walk", "to", "square"],
                                 uSentence ["approach", "square"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk back to the <village square>.", [RemoveFlag "tower described"])
                                        ],
                                stateChanges = [SceneChange "village"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "pedestal"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see a <pedestal> made out of marble. It rises up to chest height and has three <indentations>: a <chrome indentation>, a <jade indentation>, and a <ruby indentation>. ", []),
                                        (FlagSet "chrome amulet installed", "The <chrome amulet> is inserted into the <chrome indentation>.", []),
                                        (FlagSet "jade amulet installed", "The <jade amulet> is inserted into the <jade indentation>.", []),
                                        (FlagSet "ruby amulet installed", "The <ruby amulet> is inserted into the <ruby indentation>.", []),
                                        (CNot (FlagSet "got ruby amulet"), "You notice that there is a <ruby amulet> at the base of the <pedestal>.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "ruby amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "got ruby amulet",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You already got the <ruby amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You pick up the <ruby amulet>.", [])
                                    ],
                                stateChanges = [AddToInventory "ruby amulet", SetFlag "got ruby amulet"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["insert", "chrome amulet", "into", "pedestal"],
                                 uSentence ["put", "chrome amulet", "into", "pedestal"],
                                 uSentence ["use", "chrome amulet", "on", "pedestal"],
                                 uSentence ["insert", "chrome amulet", "into", "indentation"],
                                 uSentence ["put", "chrome amulet", "into", "indentation"],
                                 uSentence ["use", "chrome amulet", "on", "indentation"],
                                 uSentence ["insert", "chrome amulet", "into", "chrome indentation"],
                                 uSentence ["put", "chrome amulet", "into", "chrome indentation"],
                                 uSentence ["use", "chrome amulet", "on", "chrome indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "chrome amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You have already inserted the <chrome amulet> into the <chrome indentation>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "chrome amulet"),
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You don't have a <chrome amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You insert the <chrome amulet> into the <chrome indentation> on the <pedestal>.", [SetFlag "chrome amulet installed"]),
                                        (CAnd (FlagSet "chrome amulet installed") (CAnd (FlagSet "jade amulet installed") (FlagSet "ruby amulet installed")), "The blue crystal of the <wizard's tower> shifts, revealing a cloaked <gateway>", [SetFlag "gateway opened"])
                                    ],
                                stateChanges = [RemoveFromInventory "chrome amulet"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["insert", "jade amulet", "into", "pedestal"],
                                 uSentence ["put", "jade amulet", "into", "pedestal"],
                                 uSentence ["use", "jade amulet", "on", "pedestal"],
                                 uSentence ["insert", "jade amulet", "into", "indentation"],
                                 uSentence ["put", "jade amulet", "into", "indentation"],
                                 uSentence ["use", "jade amulet", "on", "indentation"],
                                 uSentence ["insert", "jade amulet", "into", "jade indentation"],
                                 uSentence ["put", "jade amulet", "into", "jade indentation"],
                                 uSentence ["use", "jade amulet", "on", "jade indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "jade amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You have already inserted the <jade amulet> into the <jade indentation>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "jade amulet"),
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You don't have a <jade amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You insert the <jade amulet> into the <jade indentation> on the <pedestal>.", [SetFlag "jade amulet installed"]),
                                        (CAnd (FlagSet "chrome amulet installed") (CAnd (FlagSet "jade amulet installed") (FlagSet "ruby amulet installed")), "The blue crystal of the <wizard's tower> shifts, revealing a cloaked <gateway>", [SetFlag "gateway opened"])
                                    ],
                                stateChanges = [RemoveFromInventory "jade amulet"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["insert", "ruby amulet", "into", "pedestal"],
                                 uSentence ["put", "ruby amulet", "into", "pedestal"],
                                 uSentence ["use", "ruby amulet", "on", "pedestal"],
                                 uSentence ["insert", "ruby amulet", "into", "indentation"],
                                 uSentence ["put", "ruby amulet", "into", "indentation"],
                                 uSentence ["use", "ruby amulet", "on", "indentation"],
                                 uSentence ["insert", "ruby amulet", "into", "ruby indentation"],
                                 uSentence ["put", "ruby amulet", "into", "ruby indentation"],
                                 uSentence ["use", "ruby amulet", "on", "ruby indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "ruby amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You have already inserted the <ruby amulet> into the <ruby indentation>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CNot (InInventory "ruby amulet"),
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You don't have a <ruby amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You insert the <ruby amulet> into the <ruby indentation> on the <pedestal>.", [SetFlag "ruby amulet installed"]),
                                        (CAnd (FlagSet "chrome amulet installed") (CAnd (FlagSet "jade amulet installed") (FlagSet "ruby amulet installed")), "The blue crystal of the <wizard's tower> shifts, revealing a cloaked <gateway>", [SetFlag "gateway opened"])
                                    ],
                                stateChanges = [RemoveFromInventory "ruby amulet"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "through", "gateway"],
                                 uSentence ["walk", "into", "gateway"],
                                 uSentence ["walk", "into", "tower"],
                                 uSentence ["walk", "into", "tower", "through", "gateway"],
                                 uSentence ["walk", "into", "tower", "using", "gateway"],
                                 uSentence ["walk", "inside"],
                                 uSentence ["enter"],
                                 uSentence ["enter", "gateway"],
                                 uSentence ["enter", "tower"],
                                 uSentence ["enter", "through", "gateway"],
                                 uSentence ["enter", "using", "gateway"],
                                 uSentence ["enter", "tower", "through", "gateway"],
                                 uSentence ["enter", "into", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "gateway opened",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You walk through the cloaked <gateway>.", [])
                                    ],
                                stateChanges = [SceneChange "tower ground floor"]
                            }
                        ]
                }
            ]
    }

wizardTowerGroundFloorDescriptionString :: String
wizardTowerGroundFloorDescriptionString = "You are inside <wizard's tower>, inhabited by <Isvald>, the <village>'s resident <wizard>. The tall tower’s blue crystal interiors playfully bounce the sunlight streaming in. To your <west>, is the cloaked <gateway> that let you in. To your east, is an elevator with a <button> to summon it, which seems to be the only way to get past the foyer."

wizardTowerGroundFloorScene :: Scene
wizardTowerGroundFloorScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizardTowerGroundFloor described"), wizardTowerGroundFloorDescriptionString, [SetFlag "wizardTowerGroundFloor described"])],
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
                                        (CTrue, wizardTowerGroundFloorDescriptionString, [])
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
                                        (CTrue, "You see the cloaked <gateway> that let you in.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "through", "gateway"],
                                 uSentence ["walk", "out", "gateway"],
                                 uSentence ["walk", "outside"],
                                 uSentence ["walk", "west"],
                                 uSentence ["leave"],
                                 uSentence ["leave", "tower"],
                                 uSentence ["leave", "tower", "through", "gateway"],
                                 uSentence ["leave", "tower", "using", "gateway"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You exit through the cloaked gateway, and find yourself at the base of <wizard's tower>.", [RemoveFlag "wizardTowerGroundFloor described"])
                                        ],
                                stateChanges = [SceneChange "tower"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["press", "button"],
                                 uSentence ["use", "button"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "elevator arrived")),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> arrives with a *ping* sound, and opens its arms to welcome you in.", [SetFlag "elevator arrived"]) 
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is already waiting for you with open arms! Go on in before it tires out.", [])
                                       ],
                                stateChanges = []
                            }
                        ]   
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "elevator"],
                                 uSentence ["look", "in", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "elevator"],
                                 uSentence ["walk", "in", "elevator"],
                                 uSentence ["use", "elevator"],
                                 uSentence ["enter", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You enter the cool glass cube elevator. The doors slide shut as you take in a 360-degree view of the tower.", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator ground floor"]
                            }
                        ]
                }
            ]
    }

elevatorDescriptionString :: String
elevatorDescriptionString = "The elevator has a magic <button panel> so you can pick a destination for your ride."
elevatorScene :: Scene
elevatorScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "elevator described"), elevatorDescriptionString, [SetFlag "elevator described"])],
        interactions = 
            [
                Interaction
                {
                    sentences = [uSentence ["look", "at", "button panel"],
                                 uSentence ["observe", "button panel"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator ground floor"),
                                conditionalDescription = 
                                    ConditionalDescription 
                                       [
                                           (CTrue, "You may go to the <bedroom>, the <music room>, or the <guest room>", []) 
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator bedroom"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You may go to the <ground floor>, the <music room>, or the <guest room>", [])
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator music room"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You may go to the <ground floor>, the <bedroom>, or the <guest room>", [])
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator guest room"),
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "You may go to the <ground floor>, the <bedroom>, or the <music room>", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["go", "to", "ground floor"],
                                 uSentence ["press", "ground floor"],
                                 uSentence ["ride", "to", "ground floor"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "elevator ground floor")),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> whisks you off, and brings you to the ground floor with a gentle bounce. Exit <elevator> to hop off here, or ride to a different room by picking from the magical <button panel>.", [])
                                       ],
                                stateChanges = [SetFlag "elevator ground floor", RemoveFlag "elevator bedroom", RemoveFlag "elevator music room", RemoveFlag "elevator guest room"]
                            }
                        ]
               },
               Interaction
               {
                   sentences = [uSentence ["go", "to", "bedroom"],
                                uSentence ["press", "bedroom"],
                                uSentence ["ride", "to", "bedroom"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = (CNot (FlagSet "elevator bedroom")),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "The <elevator> zooms across the tower, and comes to a floating stop at the bedroom. Exit <elevator> to hop off here, or ride to a different room by picking from the magical <button panel>.", [])
                                      ],
                               stateChanges = [SetFlag "elevator bedroom", RemoveFlag "elevator ground floor", RemoveFlag "elevator music room", RemoveFlag "elevator guest room"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["go", "to", "music room"],
                                uSentence ["press", "music room"],
                                uSentence ["ride", "to", "music room"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = (CNot (FlagSet "elevator music room")),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "The <elevator> glides away, and comes to a rest at the music room. Exit <elevator> to hop off here, or ride to a different room by picking from the magical <button panel>.", []) 
                                      ],
                               stateChanges = [SetFlag "elevator music room", RemoveFlag "elevator ground floor", RemoveFlag "elevator bedroom", RemoveFlag "elevator guest room"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["go", "to", "guest room"],
                                uSentence ["press", "guest room"],
                                uSentence ["ride", "to", "guest room"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = (CNot (FlagSet "elevator guest room")),
                               conditionalDescription = 
                                   ConditionalDescription 
                                      [
                                          (CTrue, "The <elevator> sails across the tower, and docks at the guest room. Exit <elevator> to hop off here, or ride to a different room by picking from the magical <button panel>.", [])
                                      ],
                               stateChanges = [SetFlag "elevator guest room", RemoveFlag "elevator ground floor", RemoveFlag "elevator bedroom", RemoveFlag "elevator music room"]
                           }
                      ]
               },
               Interaction
               {
                   sentences = [uSentence ["walk", "out", "elevator"],
                                uSentence ["leave", "elevator"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator ground floor"),
                               conditionalDescription = 
                                   ConditionalDescription 
                                      [
                                          (CTrue, "You step out to the ground floor, and the elevator lifts off.", [])
                                      ],
                               stateChanges = [SceneChange "tower ground floor", RemoveFlag "elevator ground floor", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator bedroom"),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the bedroom, and the elevator drifts away.", [])
                                      ],
                               stateChanges = [SceneChange "tower bedroom", RemoveFlag "elevator bedroom", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator music room"),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the music room, and the elevator cruises away.", [])
                                      ],
                               stateChanges = [SceneChange "tower music room", RemoveFlag "elevator music room", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator guest room"),
                               conditionalDescription =
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the guest room, and the elevator slides away.", [])
                                      ],
                               stateChanges = [SceneChange "tower guest room", RemoveFlag "elevator guest room", RemoveFlag "elevator described"]
                           }
                       ]
               }
            ]
    }

wizardTowerBedroomDescriptionString :: String
wizardTowerBedroomDescriptionString = "You are in the ornate bedroom. The <wizard>, <Isvald>, is sleeping on the <bed>. One corner of the room is inhabited by a <chest of drawers>, with a mirror atop. You hear music floating up to the bedroom, presumably from the music room. It sounds like a lullaby, and is making you sleepy. Behind you is the <elevator>, with a call <button> to summon it."

wizardTowerBedroomScene :: Scene
wizardTowerBedroomScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizardTowerBedroom described"), wizardTowerBedroomDescriptionString, [SetFlag "wizardTowerBedroom described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look", "at", "wizard"],
                                 uSentence ["see", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <wizard> has a big smile on his face because he is dreaming. He is clutching a <gear> to his chest. A luxurious purple velvet blanket envelopes him.", [])],
                                stateChanges = [SetFlag "wizard seen"]
                            }
                        ]
               },
               Interaction
               {
                   sentences = [uSentence ["look", "at", "bed"],
                                uSentence ["see", "bed"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The magnificent four poster bed looks quite comfortable. Alas, no one save the <wizard> can be in it. The bed's canopy looks lovely even drawn aside.", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["look", "at", "chest of drawers"],
                                uSentence ["see", "chest of drawers"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The eye-catching distressed dresser has a gold-hued finish that accents the jewel-toned tabletop trinkets. It has three <drawers> to stow away unwanted clutter.", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["look", "at", "elevator"],
                                uSentence ["see", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["open", "drawers"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You see clothes that don't seem like the right size. You also find dusty old books written in a language you don't understand. The magical drawers tuck themselves away upon noticing your disinterest.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["get", "gear"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "wizard seen",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <wizard> is holding the <gear> magically. Thus, you are not able to pry it free.", [])],
                               stateChanges = [SetFlag "at wizard", RemoveFlag "at chest of drawers", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["walk", "to", "wizard"],
                                uSentence ["walk", "to", "bed"],
                                uSentence ["approach", "wizard"],
                                uSentence ["approach", "bed"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CNot (FlagSet "at wizard"),
                               conditionalDescription = ConditionalDescription [(CTrue, "You walk to the sleeping <wizard>.", [])],
                               stateChanges = [SetFlag "at wizard", RemoveFlag "at chest of drawers", RemoveFlag "at elevator"]
                           },
                           ConditionalAction
                           {
                               condition = FlagSet "at wizard",
                               conditionalDescription = ConditionalDescription [(CTrue, "You are close enough to the wizard that you can see his whiskers vibrate with every sleeping breath", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["walk", "to", "chest of drawers"],
                                uSentence ["approach", "chest of drawers"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CNot (FlagSet "at chest of drawers"),
                               conditionalDescription = ConditionalDescription [(CTrue, "You noiselessly walk to the <chest of drawers>.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
                           },
                           ConditionalAction
                           {
                               condition = FlagSet "at chest of drawers",
                               conditionalDescription = ConditionalDescription [(CTrue, "To get any closer, you'd have to tuck yourself into a drawer", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["walk", "to", "elevator"],
                                uSentence ["approach", "elevator"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CNot (FlagSet "at elevator"),
                               conditionalDescription = ConditionalDescription [(CTrue, "You walk to the <elevator> and call <button>.", [])],
                               stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers"]
                           },
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is waiting for you with open arms! Go on in before it tires out.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You are at the <elevator>. Press the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                    sentences = [uSentence ["press", "button"],
                                 uSentence ["use", "button"],
                                 uSentence ["call", "elevator"]],
                    conditionalActions =
                        [   
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is already waiting for you with open arms! Go on in before it tires out.", [])
                                       ],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> arrives with a *ping* sound, and opens its arms to welcome you in.", [SetFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "elevator"],
                                 uSentence ["walk", "in", "elevator"],
                                 uSentence ["use", "elevator"],
                                 uSentence ["enter", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You enter the cool glass cube elevator. The doors slide shut as you take in a 360-degree view of the tower.", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers", SceneChange "elevator", SetFlag "elevator bedroom"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers"]
                            }
                        ]
                }
            ]
    }

wizardTowerMusicRoomDescriptionString :: String
wizardTowerMusicRoomDescriptionString = "You are in a harmonious room. The walls are white, with black musical notes running across them. Their tune interleaves with framed pictures of the wizard playing the piano. At the head of the room, sits a grand paino. Arm chairs with delicate detail on the cushions face the piano. The <gramophone> to the right of the piano, seems to be the source of a lullaby that's making you sleepy."

wizardTowerMusicRoomScene :: Scene
wizardTowerMusicRoomScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "music room described"), wizardTowerMusicRoomDescriptionString, [SetFlag "music room described"])],
        interactions = 
            [
                Interaction
                {
                    sentences = [uSentence ["look", "at", "gramophone"],
                                 uSentence ["see", "gramophone"],
                                 uSentence ["walk", "to", "gramophone"],
                                 uSentence ["approach", "gramophone"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The exquisite artwork on the record is mesmerizing you as it spins. The <needle> is amicably vibrating to produce the melodious sound.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["lift", "needle"],
                                 uSentence ["touch", "needle"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The music does not turn off. Yet, it dims gently, and you hear the wizard's voice say, \"I am trapped in dream land by accident. I need a pendulum in the dream world to break the spell.\" The music is distinct again.", [])],
                                stateChanges = [SetFlag "heard wizard"]
                            }
                        ]
                },
                Interaction
                {
                   sentences = [uSentence ["look", "at", "elevator"],
                                uSentence ["see", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                   sentences = [uSentence ["walk", "to", "elevator"],
                                uSentence ["approach", "elevator"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is waiting for you with open arms! Go on in before it tires out.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You are at the <elevator>. Press the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                    sentences = [uSentence ["press", "button"],
                                 uSentence ["use", "button"],
                                 uSentence ["call", "elevator"]],
                    conditionalActions =
                        [   
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is already waiting for you with open arms! Go on in before it tires out.", [])
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> arrives with a *ping* sound, and opens its arms to welcome you in.", [SetFlag "elevator arrived"]) 
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "elevator"],
                                 uSentence ["walk", "in", "elevator"],
                                 uSentence ["use", "elevator"],
                                 uSentence ["enter", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You enter the cool glass cube elevator. The doors slide shut as you take in a 360-degree view of the tower.", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator music room"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }
 
wizardTowerGuestRoomDescriptionString :: String
wizardTowerGuestRoomDescriptionString = "You are in a simple, yet cozy room. It is setup with a four poster <bed>, plain wooden chest of drawers, and a full length mirror. The <elevator>'s call <button> is behind you."

wizardTowerGuestRoomScene :: Scene
wizardTowerGuestRoomScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizard tower guest room described"), wizardTowerGuestRoomDescriptionString, [SetFlag "wizard tower guest room described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["walk", "to", "bed"],
                                 uSentence ["approach", "bed"],
                                 uSentence ["look", "at", "bed"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The four poster <bed> and a soft blanket look inviting", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["fall asleep"],
                                 uSentence ["sleep", "upon", "bed"],
                                 uSentence ["lie down", "upon", "bed"],
                                 uSentence ["go", "to", "sleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "heard wizard",
                                conditionalDescription = ConditionalDescription [(CTrue, "You fall into a serene slumber as soon as your head hits the pillow. You feel a gentle breeze, and are transported into Dreamland.", [])],
                                stateChanges = [SceneChange "starfield"]
                            },
                            ConditionalAction
                            {
                                condition = CNot (FlagSet "heard wizard"),
                                conditionalDescription = ConditionalDescription [(CTrue, "Anh anh aanh! Sleep now, and your town is lost forever! See if you can find and eliminate whatever is inducing this sleepy state", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                   sentences = [uSentence ["look", "at", "elevator"],
                                uSentence ["see", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                   sentences = [uSentence ["walk", "to", "elevator"],
                                uSentence ["approach", "elevator"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is waiting for you with open arms! Go on in before it tires out.", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You are at the <elevator>. Press the call <button> to summon it.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                    sentences = [uSentence ["press", "button"],
                                 uSentence ["use", "button"],
                                 uSentence ["call", "elevator"]],
                    conditionalActions =
                        [   
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is already waiting for you with open arms! Go on in before it tires out.", [])
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> arrives with a *ping* sound, and opens its arms to welcome you in.", [SetFlag "elevator arrived"]) 
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "elevator"],
                                 uSentence ["walk", "in", "elevator"],
                                 uSentence ["use", "elevator"],
                                 uSentence ["enter", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You enter the cool glass cube elevator. The doors slide shut as you take in a 360-degree view of the tower.", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator guest room"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                                stateChanges = []
                            }
                        ]
               }
            ]
    }

starFieldDescriptionString :: String
starFieldDescriptionString = "You open your eyes and find yourself floating in a sea of stars. To your left you see a constellation which looks like a [Clock]." ++
                             "To your right you see a constellation which you know is called [Hypnotism]. Above you is a constellation called [Cup Cake]."

starFieldScene :: Scene
starFieldScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "star field described"), starFieldDescriptionString, [SetFlag "star field described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, starFieldDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "star"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription = ConditionalDescription [(CTrue, "You already have a star.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You pluck a star out of the sky and put it in your pocket.", [])],
                                stateChanges = [AddToInventory "star"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "clock constellation"],
                                 uSentence ["walk", "to", "clock constellation"],
                                 uSentence ["approach", "clock constellation"],
                                 uSentence ["fly", "into", "clock constellation"],
                                 uSentence ["fly", "to", "clock constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the [Clock] constellation", [])],
                            stateChanges = [SceneChange "clock",
                                            RemoveFlag "star field described"]
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "hypnotism constellation"],
                                 uSentence ["walk", "to", "hypnotism constellation"],
                                 uSentence ["approach", "hypnotism constellation"],
                                 uSentence ["fly", "into", "hypnotism constellation"],
                                 uSentence ["fly", "to", "hypnotism constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the [Hypnotism] constellation", [])],
                            stateChanges = [SceneChange "hypnotism",
                                            RemoveFlag "star field described"]
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "cupcake constellation"],
                                 uSentence ["walk", "to", "cupcake constellation"],
                                 uSentence ["approach", "cupcake constellation"],
                                 uSentence ["fly", "into", "cupcake constellation"],
                                 uSentence ["fly", "to", "cupcake constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the [Cup Cake] constellation", [])],
                            stateChanges = [SceneChange "cupcake",
                                            RemoveFlag "star field described"]
                        }
                    ]
                }
            ]
    }

clockDescriptionString :: String
clockDescriptionString = "You are on the outside of a <prison> made of stars. Right across from the twinkling prison bars is a grandfather <clock> that's missing a pendulum."

clockScene :: Scene
clockScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "clock described"), clockDescriptionString, [SetFlag "clock described"])],
        interactions = 
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, clockDescriptionString, [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "prison"],
                                 uSentence ["walk", "to", "prison"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "The <prison> holds all the people from the <village>. You spot your <parents>, <Jorryn>, and <Evanna>. Even the <wizard> is here.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "<Jorryn> appears unharmed. It seems like he wants to say something to you.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "parents"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Your <parents> appear unharmed. It seems like they want to say something to you.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "Evanna"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "<Evanna> appears unharmed. It seems like she wants to talk to you.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "The <wizard> appears unharmed. It seems like he wants to talk to you.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "parents"],
                                 uSentence ["talk", "with", "parents"],
                                 uSentence ["walk", "to", "parents"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Get us out of here! I hope our crops are not wilting away.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "Jorryn"],
                                 uSentence ["talk", "with", "Jorryn"],
                                 uSentence ["walk", "to", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Please get me out of this nightmare land!", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "Evanna"],
                                 uSentence ["talk", "with", "Evanna"],
                                 uSentence ["walk", "to", "Evanna"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Oh my gosh, this gravel is so rough, why am I sleeping on the road?", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "wizard"],
                                 uSentence ["talk", "with", "wizard"],
                                 uSentence ["walk", "to", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Fix <clock> with pendulum! Everyone wakes up, and you win.", [])],
                                stateChanges = [] 
                            }
                        ]
                },
                Interaction
                {
                   sentences = [uSentence ["fix", "clock", "with", "pendulum"],
                               uSentence ["fix", "clock"],
                               uSentence ["put", "pendulum", "in", "clock"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = CNot (InInventory "pendulum"),
                               conditionalDescription = ConditionalDescription[(CTrue, "Alas! You don't have a pendulum! Hurry, and find one!", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = InInventory "pendulum",
                               conditionalDescription = ConditionalDescription[(CTrue, "Alas! The pendulum easily fits in the clock, and resumes its to-and-fro motion!", [])],
                               stateChanges = [SceneChange "win"]
                           }
                       ]
                }
            ]
    }

hypnotismDescriptionString :: String
hypnotismDescriptionString = "You step up onto the stage, the lights overhead are blindingly bright. Before you is a ghostly audience, to your right on the stage is a <white rabbit> wearing a tuxedo and a top hat. Next to the <white rabbit> there's a giant red <couch>. You can see the <star field> off stage to your <left>. "

hypnotismDescriptionStringBefore :: String
hypnotismDescriptionStringBefore = hypnotismDescriptionString ++ "The <white rabbit> has a <pendulum> in its paw."

hypnotismScene :: Scene
hypnotismScene =
    Scene
    {
        sceneDescription = ConditionalDescription [((CAnd (CNot (FlagSet "hypnotism described")) (CNot (InInventory "pendulum"))), hypnotismDescriptionStringBefore, [SetFlag "hypnotism described"]),
                                                   ((CAnd (CNot (FlagSet "hypnotism described")) (InInventory "pendulum")), hypnotismDescriptionString, [SetFlag "hypnotism described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "pendulum"), hypnotismDescriptionStringBefore, []),
                                        (InInventory "pendulum", hypnotismDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "rabbit"],
                                 uSentence ["talk", "with", "rabbit"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "pendulum",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The rabbit speaks to you, \"You took my pendulum, so I can't do my trick anymore, but it was worth swapping it for that cake. It was delicious!\"", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The rabbit asks, \"Are you volunteering for my trick?, just lie down on the couch.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["sit", "upon", "couch"],
                                 uSentence ["lie down", "upon", "couch"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CNot (InInventory "pendulum"),
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You lie down on the couch. The rabbit walks up to you and swings it before you. You feel your eyes growing heavy. You fall into a deep slumber, never to awake again.", [SceneChange "lose"])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You lie down on the couch. The rabbit walks up to you says \"I hope you're enjoying yourself, I can't do my trick without that pendulum.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "to", "star field"],
                                 uSentence ["approach", "star field"],
                                 uSentence ["walk", "left"],
                                 uSentence ["leave"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You walk off stage and your vision fades to black.", [])],
                            stateChanges = [SceneChange "starfield",
                                            RemoveFlag "hypnotism described"]
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "pendulum"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CNot (InInventory "pendulum"),
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You reach for the rabbit's pendulum. It pulls it away and wags its finger at you. \"Not so fast, if you want this, I want a treat in exchange!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You already have the rabbit's pendulum.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "pendulum", "to", "rabbit"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CNot (InInventory "pendulum"),
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "\"You don't have a pendulum.\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You give the pendulum to the rabbit. She says \"I thought you needed that. Well, thanks for giving it back to me!\"", [])],
                            stateChanges = [RemoveFromInventory "pendulum"]
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "pendulum"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"That was my favorite pendulum, but I have some more backstage.\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"Oh, this is what I use to hypnotize my volunteers!\".", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "clock"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"The time now is 6pm.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "carrot cake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"I love carrots!\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "chocolate cake"],
                                 uSentence ["ask", "rabbit", "about", "lemon cake"],
                                 uSentence ["ask", "rabbit", "about", "cupcake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"Those are nice I guess, but not my favorite flavor.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "hypnotism constellation"],
                                 uSentence ["ask", "rabbit", "about", "hypnosis"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit tells you \"If you want to watch the show, you've got to buy a ticket like everyone else. You can join the audience if you volunteer for my trick though! Just lie down on the couch.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "carrot cake", "to", "rabbit"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "carrot cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "carrot cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit takes the cake. She says \"I love carrots! You've got a deal, take my pendulum!\"", [])],
                            stateChanges = [AddToInventory "pendulum", RemoveFromInventory "carrot cake"]
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a carrot cake.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "chocolate cake", "to", "rabbit"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "chocolate cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "chocolate cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a chocolate cake.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "lemon cake", "to", "rabbit"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "lemon cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "lemon cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a lemon cake.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "cupcake", "to", "rabbit"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "cupcake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "cupcake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The rabbit says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a cupcake.", [])],
                            stateChanges = []
                        }
                    ]
                }
            ]
    }

cupcakeDescriptionString :: String
cupcakeDescriptionString = "You step out onto a green field that stretches to the horizon, above you is a <star field> in the sky. There are cupcake flowers growing all over the field, in every color of the rainbow. Before you, an elderly woman sits at a <table> with a red and white polkadot tablecloth."

cupcakeScene :: Scene
cupcakeScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "cupcake described"), cupcakeDescriptionString, [SetFlag "cupcake described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, cupcakeDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "table"],
                                 uSentence ["walk", "to", "table"],
                                 uSentence ["approach", "table"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The table has three cakes on it. Each cake is covered in pink frosting and chocolate sprinkles. The <left cake> has a <red card> in front of it, the <middle cake> has a <green card> in front of it, and the <right cake> has a <blue card> in front of it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "Instead of flowers, there are cupcakes on stems growing out of the ground.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You pluck a cupcake off its stem.", [])],
                                stateChanges = [AddToInventory "cupcake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "left cake"],
                                 uSentence ["look", "at", "middle cake"],
                                 uSentence ["look", "at", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The cake is covered in pink frosting and chocolate sprinkles. Nothing distinguishes it from the other cakes.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "red card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The red card in front of the left cake reads:\n\"Most people think I'm sweet but really I'm bitter. I take many forms, I flow when it's hot and snap when it's cold. I soften when it's warm but enough heat makes me brittle. Sweet, bitter, or spicy, I'm universally loved!\nPrice: 1 star\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "green card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The green card in front of the middle cake reads:\n\"I have a tough life. I'm born in darkness, you'll see my rosette first. It takes two years to show my true form, and when I do show myself I might get chopped to bits, strung up in front of someone, or shoved into a pile of snow.\nPrice: 1 star\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "blue card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The blue card in front of the right cake reads:\n\"I'm loved by many for my brilliant hue but parts of me are practical too. I clean stains, help sailors stay healthy at sea, but when I write, my words disappear!\nPrice: 1 star\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "red card"],
                                 uSentence ["buy", "green card"],
                                 uSentence ["buy", "blue card"],
                                 uSentence ["get", "red card"],
                                 uSentence ["get", "green card"],
                                 uSentence ["get", "blue card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The elderly lady says \"Those cards are not for sale.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "elderly woman"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You see an elderly lady sitting behind a table with three cakes. Her hair is tied in a bun and she's wearing a blue apron and a hair net.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "left cake"],
                                 uSentence ["get", "middle cake"],
                                 uSentence ["get", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You reach out to take one of the cakes. The elderly woman says \"Those cakes aren't free, you have to pay for them you know!\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "left cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the elderly woman a star and in exchange she cuts a slice of the left cake and hands it to you. \"Good choice.\" she says \"That one's my favorite!\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "chocolate cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The elderly woman says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "middle cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the elderly woman a star and in exchange she cuts a slice of the middle cake and hands it to you. \"That one huh?\" she says \"I'm not a big fan of that one, but I hear it's popular with some folks.\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "carrot cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The elderly woman says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the elderly woman a star and in exchange she cuts a slice of the right cake and hands it to you. The elderly woman says \"What a strange choice, well I'm sure you'll enjoy it anyway.\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "lemon cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The elderly woman says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "to", "star field"],
                                 uSentence ["approach", "star field"],
                                 uSentence ["fly", "to", "star field"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float back to the star field.", [])],
                            stateChanges = [SceneChange "starfield",
                                            RemoveFlag "cupcake described"]
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
                    sentences = [uSentence ["do nothing"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You blink out of existence for a moment.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "star"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription = ConditionalDescription [(CTrue, "You inspect the star, it twinkles in your hand.", [])],
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
                                condition = InInventory "chrome amulet",
                                conditionalDescription = ConditionalDescription [(CTrue, "You are wearing the <chrome amulet> your grandfather gave you as a child. It shimmers as you inspect it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "jade amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "jade amulet",
                                conditionalDescription = ConditionalDescription [(CTrue, "You look at <Evanna>'s <jade amulet>. It shimmers as you inspect it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "ruby amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "ruby amulet",
                                conditionalDescription = ConditionalDescription [(CTrue, "You look at the <ruby amulet> you picked up from the base of the <pedestal>. It shimmers as you inspect it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "chocolate cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "chocolate cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of chocolate cake.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "carrot cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "carrot cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of carrot cake.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "lemon cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "lemon cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of lemon cake.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "at", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "cupcake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a multi-colored cupcake.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["eat", "chocolate cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "chocolate cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the chocolate cake. It's delicious!", [])],
                                stateChanges = [RemoveFromInventory "chocolate cake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["eat", "carrot cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "carrot cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the carrot cake. It's delicious!", [])],
                                stateChanges = [RemoveFromInventory "carrot cake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["eat", "lemon cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "lemon cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the lemon cake. It's delicious!", [])],
                                stateChanges = [RemoveFromInventory "lemon cake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["eat", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "cupcake",
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the cupcake. It's delicious!", [])],
                                stateChanges = [RemoveFromInventory "cupcake"]
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

allScenes :: (Data.Map.Map String Scene, [String])
allScenes = (Data.Map.fromList [("cottage", cottageScene),
                                ("win", winScene),
                                ("lose", loseScene),
                                ("end", endScene),
                                ("village", villageScene),
                                ("tower", towerExteriorScene),
                                ("starfield", starFieldScene),
                                ("hypnotism", hypnotismScene),
                                ("cupcake", cupcakeScene),
                                ("clock", clockScene),
                                ("tower ground floor", wizardTowerGroundFloorScene),
                                ("tower bedroom", wizardTowerBedroomScene),
                                ("tower music room", wizardTowerMusicRoomScene),
                                ("tower guest room", wizardTowerGuestRoomScene),
                                ("elevator", elevatorScene)], --List of scenes
             ["end"]) --End scenes
