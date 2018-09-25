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
gameIntro = "Nightmare Adventure\n" ++
            "-------------------\n" ++
            "Authors: Vibha Laljani and Laurence Emms\n" ++
            "Editor: Jared Molton\n" ++
            "Play Testers: Kelly Townsend Jennings, Sowmya Pary, Aditi Roy, Tim Detwiler\n" ++
            "\n"

allVerbs :: [Token]
allVerbs =
    [
        TokenVerb "walk" ["walk", "stride", "strut", "step", "hike", "trot", "stroll", "march", "amble", "saunter", "trek", "wander", "trudge", "ambulate", "plod", "traverse", "prance", "promenade", "perambulate", "tread", "traipse", "hoof it", "move", "go", "jog", "run"],
        TokenVerb "get" ["get", "take", "pick up", "pluck", "snatch", "acquire", "grab", "pick", "attain", "capture", "fetch", "procure"],
        TokenVerb "put" ["put", "place", "put down", "set", "plant", "deposit", "embed"],
        TokenVerb "throw" ["throw", "pitch", "toss", "fling", "hurl", "lob", "chuck", "cast"],
        TokenVerb "say hi" ["say hi", "say hello"],
        TokenVerb "greet" ["greet"],
        TokenVerb "thank" ["thank"],
        TokenVerb "give" ["give", "share", "gift", "present"],
        TokenVerb "select" ["select", "pick", "choose", "take"],
        TokenVerb "read" ["read", "scan", "study", "view", "skim", "peruse"],
        TokenVerb "look" ["look", "view", "scan", "see", "look at"],
        TokenVerb "inspect" ["see", "view", "scan", "spy", "observe", "inspect", "check out", "look at", "look", "examine", "check on"],
        TokenVerb "look around" ["look around", "search"],
        TokenVerb "use" ["use", "initiate", "start", "manipulate", "utilize", "operate"],
        TokenVerb "jump" ["jump", "hop", "skip", "leap"],
        TokenVerb "sit" ["sit", "sit down"],
        TokenVerb "lie down" ["lie", "lie down", "lay down"],
        TokenVerb "lie" ["lie", "lay"],
        TokenVerb "go" ["go", "move", "proceed", "advance", "travel", "journey", "make for"],
        TokenVerb "ride" ["ride", "fly", "head"],
        TokenVerb "approach" ["approach"],
        TokenVerb "fly" ["fly", "float", "drift", "glide", "move", "go", "levitate", "flutter", "sail"],
        TokenVerb "wake" ["wake", "wake up", "awaken"],
        TokenVerb "flip" ["flip", "back flip", "backflip"],
        TokenVerb "shake" ["shake", "jostle", "agitate"],
        TokenVerb "run" ["run", "race", "jog", "sprint", "dash"],
        TokenVerb "sing" ["sing", "serenade", "yodel"],
        TokenVerb "dance" ["dance"],
        TokenVerb "smile" ["smile", "grin", "laugh"],
        TokenVerb "frown" ["frown", "glower", "grimace", "pout", "sulk"],
        TokenVerb "climb" ["climb", "scale", "get", "mount", "clamber"],
        TokenVerb "turn" ["turn"],
        TokenVerb "turn off" ["turn off", "switch off", "put off"],
        TokenVerb "unlock" ["unlock"],
        TokenVerb "lock" ["lock"],
        TokenVerb "open" ["open"],
        TokenVerb "close" ["close", "shut"],
        TokenVerb "enter" ["enter", "get into", "embark", "get in"],
        TokenVerb "insert" ["insert", "plug", "implant", "stick"],
        TokenVerb "remove" ["remove", "extract", "evacuate", "eject", "disengage", "extricate"],
        TokenVerb "hop off" ["hop off", "hop out", "jump off", "jump out"],
        TokenVerb "leave" ["leave", "exit", "get out", "depart", "abandon", "disembark", "vacate"],
        TokenVerb "eat" ["eat", "consume", "devour", "swallow", "ingest", "dine on", "snack on", "masticate", "digest", "munch", "scoff", "chew", "chow on"],
        TokenVerb "drink" ["drink", "consume", "sip", "quaff"],
        TokenVerb "do something" ["do something"],
        TokenVerb "do nothing" ["do nothing"],
        TokenVerb "press" ["press", "hit", "push", "depress", "poke", "tap", "touch"],
        TokenVerb "break" ["break", "crack", "throw", "deface", "harm", "destroy", "damage", "shatter", "smash", "demolish"],
        TokenVerb "stop" ["stop", "pause", "halt", "stall"],
        TokenVerb "lift" ["lift", "remove", "pick", "push", "raise", "rise", "hoist"],
        TokenVerb "touch" ["touch", "stroke", "grab", "feel", "handle", "pat", "brush", "tap", "poke"],
        TokenVerb "ask" ["ask", "question", "query", "inquire", "quiz", "interrogate"],
        TokenVerb "talk" ["talk", "chat", "converse", "communicate", "speak", "parley"],
        TokenVerb "call" ["call", "summon", "call for", "call in", "request", "beckon", "command", "muster", "call forth"],
        TokenVerb "buy" ["buy", "purchase", "ask to buy", "request", "procure"],
        TokenVerb "pay" ["pay", "compensate"],
        TokenVerb "fall asleep" ["fall asleep", "slumber"],
        TokenVerb "sleep" ["sleep", "slumber", "nap", "relax"],
        TokenVerb "fix" ["fix", "repair", "restore", "mend", "heal", "revive", "refurbish", "correct", "reform", "renew", "rejuvinate"],
        TokenVerb "shout" ["shout", "yell", "scream", "roar", "howl", "bellow", "shriek"]
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
        TokenNoun "outside" ["outside", "out"],
        TokenNoun "me" ["me"],
        TokenNoun "myself" ["myself"],
        TokenNoun "chrome amulet" ["chrome amulet"],
        TokenNoun "front door" ["front door", "door", "doorway", "front doorway"],
        TokenNoun "door" ["door", "doorway"],
        TokenNoun "home" ["home", "my house", "house", "cottage", "mud-brick cottage", "mud brick cottage"], --Cottage nouns
        TokenNoun "Jorryn" ["jorryn", "my brother", "brother"],
        TokenNoun "parents" ["parents", "family"],
        TokenNoun "square" ["square", "village square", "village"], --Village square nouns
        TokenNoun "clock" ["ancient clock", "clock"],
        TokenNoun "Evanna" ["evanna"],
        TokenNoun "jade amulet" ["jade amulet"],
        TokenNoun "tower" ["tower", "wizard's tower", "crystal tower"], --Tower nouns
        TokenNoun "pedestal" ["pedestal"],
        TokenNoun "indentations" ["indentations"],
        TokenNoun "indentation" ["indentation"],
        TokenNoun "chrome indentation" ["chrome indentation"],
        TokenNoun "jade indentation" ["jade indentation"],
        TokenNoun "ruby indentation" ["ruby indentation"],
        TokenNoun "ruby amulet" ["ruby amulet"],
        TokenNoun "gateway" ["gateway", "gate", "cloaked gateway", "cloaked door"],
        TokenNoun "elevator" ["elevator", "lift"],
        TokenNoun "elevator arms" ["elevator arms", "arms"],
        TokenNoun "view" ["view", "scene"],
        TokenNoun "ground floor" ["ground floor"],
        TokenNoun "wizard" ["wizard", "isvald"],
        TokenNoun "bedroom" ["bedroom", "bed room"],
        TokenNoun "ground floor" ["ground floor"],
        TokenNoun "music room" ["music room"],
        TokenNoun "guest room" ["guest room"],
        TokenNoun "button panel" ["panel", "button panel"],
        TokenNoun "button" ["button", "elevator button"],
        TokenNoun "chest of drawers" ["chest of drawers", "drawers", "dresser"],
        TokenNoun "first drawer" ["first drawer"],
        TokenNoun "second drawer" ["second drawer"],
        TokenNoun "third drawer" ["third drawer"],
        TokenNoun "bed" ["bed"],
        TokenNoun "sleep" ["sleep", "slumber"],
        TokenNoun "gramophone" ["gramophone", "phonograph"],
        TokenNoun "off" ["off", "down"],
        TokenNoun "needle" ["needle", "stylus", "pin"],
        TokenNoun "star" ["star"], -- Star field nouns
        TokenNoun "stars" ["sea of stars", "stars", "star field", "starfield"],
        TokenNoun "clock constellation" ["clock", "clock constellation"],
        TokenNoun "hypnotism constellation" ["hypnotism", "hypnotism constellation"],
        TokenNoun "hypnosis" ["hypnosis", "hypnotism"],
        TokenNoun "cupcake constellation" ["cupcake", "cup cake", "cupcake constellation", "cup cake constellation"],
        TokenNoun "rabbit" ["rabbit", "white rabbit"],
        TokenNoun "couch" ["couch", "red couch"],
        TokenNoun "star field" ["star field", "starfield", "stars", "sea of stars"],
        TokenNoun "pendulum" ["pendulum"],
        TokenNoun "cakes" ["cakes"],
        TokenNoun "price" ["price"],
        TokenNoun "carrot cake" ["carrot cake", "cake"],
        TokenNoun "chocolate cake" ["chocolate cake", "cake"],
        TokenNoun "lemon cake" ["lemon cake", "cake"],
        TokenNoun "left cake" ["left cake"],
        TokenNoun "middle cake" ["middle cake"],
        TokenNoun "right cake" ["right cake"],
        TokenNoun "red card" ["red card"],
        TokenNoun "green card" ["green card"],
        TokenNoun "blue card" ["blue card"],
        TokenNoun "table" ["table"],
        TokenNoun "cupcake" ["cupcake", "flower", "cake", "cup cake", "cupcake flower", "cup cake flower", "treat"],
        TokenNoun "audience" ["audience", "ghostly audience"],
        TokenNoun "elderly woman" ["elderly woman", "old woman", "elderly lady", "old lady", "woman"],
        TokenNoun "prison" ["prison", "jail", "cell"]
    ]

allPrepositions :: [Token]
allPrepositions =
    [
        TokenPreposition "in" ["in", "inside", "within"],
        TokenPreposition "into" ["into", "in", "inside"],
        TokenPreposition "out" ["out", "outside"],
        TokenPreposition "on" ["on"],
        TokenPreposition "upon" ["on", "on top", "upon"],
        TokenPreposition "above" ["above", "over"],
        TokenPreposition "over" ["over"],
        TokenPreposition "below" ["below", "under", "underneath", "beneath"],
        TokenPreposition "across" ["across"],
        TokenPreposition "before" ["before"],
        TokenPreposition "for" ["for"],
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
        TokenPreposition "to" ["to", "toward", "towards"],
        TokenPreposition "until" ["until", "till"],
        TokenPreposition "with" ["with"],
        TokenPreposition "together with" ["together with"],
        TokenPreposition "in through" ["in through"]
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
introString = "It's been a long day working at the hydration farms outside of your <village square>. You return home as the sun is setting. You open the door to your mud-brick <cottage> to find your brother, <Jorryn>, face down on the dirt floor just past the <front door>.\nYou fear the worst; life on the edge of the great desert can be dangerous.\nBut when you lean down to look at your <brother> you realize that he is not dead, he's asleep.\nYou look around the corner and find that your <parents> are also sprawled out on the floor by the stove, they are also asleep.\nNo amount of shaking or shouting seems to wake them.\n\n[Pssssst, objects enclosed in <> like front door, parents etc are available for interaction. For instance, try this command next: look at parents ]\n"

cottageDescriptionString :: String
cottageDescriptionString = "You're standing in your home. <Jorryn> is lying by the <front door>. Your <parents> are on the floor by the stove. You notice that the bustling cacophony of commerce from the <village square> to the <east> is eerily absent."

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
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["inspect", "home"],
                                 uSentence ["look around", "home"]],
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
                    sentences = [uSentence ["inspect", "parents"],
                                 uSentence ["wake", "parents"],
                                 uSentence ["walk", "to", "parents"]],
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
                    sentences = [uSentence ["shake", "parents"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You shake your <parents> like salt and pepper shakers. Yet, they're still in deep slumber.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "Jorryn"],
                                 uSentence ["wake", "Jorryn"],
                                 uSentence ["walk", "to", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "<Jorryn> lies asleep on the floor, you can't seem to wake him.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["shake", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You shake shake shake <Jorryn>, but he doesn't make a single move.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look", "east"],
                                 uSentence ["inspect", "square"]],
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
                    sentences = [uSentence ["inspect", "door"],
                                 uSentence ["inspect", "front door"]],
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
                                        (CNot (FlagSet "front door closed"), "You close your <front door>.", []),
                                        ((FlagSet "front door closed"), "Your <front door> is already closed.", [])
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
                                        (CNot (FlagSet "front door closed"), "Your <front door> is already open.", []),
                                        ((FlagSet "front door closed"), "You open your <front door>.", [])
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
                                            (FlagSet "square visited", "You walk through the front door of your <home> and towards the <village>. Everyone you pass is asleep. Your friend, <Evanna>, is fast asleep at the base of the <ancient clock>.", []),
                                            (CNot (FlagSet "square visited"), "You walk through the front door of your <home> and towards the <village>.\n\nAs you walk through the <village>, you come across several people asleep on the ground. You arrive at the <village square> and you notice that, for the first time in your life, the <ancient clock> has stopped.\n\nYou see your friend, <Evanna>, walking slowly through the <village square> in a dazed stupor. As you approach, she collapses to the ground. The <ancient clock> chimes and her body starts to glow a deep golden color, the glowing aura departs her body and shoots into the <ancient clock> as it falls silent.\n", [])
                                        ],
                                stateChanges = [SceneChange "village", SetFlag "square visited", RemoveFlag "cottage described"]
                            },
                            ConditionalAction
                            {
                                condition = FlagSet "front door closed",
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "Your <front door> is closed, blocking your way.", [])
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
        sceneDescription = ConditionalDescription [(CTrue, "", [SceneChange "end"])],
        interactions = []
    }

loseScene :: Scene
loseScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CTrue, "", [SceneChange "end"])],
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
villageDescriptionString = "You are standing in the <village square>. It is a dusty open area surrounded by mud-brick buildings. The <ancient clock> stands silent in the center of the <square>. Your friend, <Evanna>, is lying at the base of the <ancient clock>. Your <home> is to the <west>. You see the <wizard's tower> to the <south> at the edge of the great desert."

villageScene :: Scene
villageScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "village described"), villageDescriptionString, [SetFlag "village described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "clock"],
                                 uSentence ["walk", "to", "clock"],
                                 uSentence ["approach", "clock"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You inspect the <ancient clock>. It is carved out of a block of marble and has stood on this spot, steadily ticking, since long before your tribe came to settle in the great desert. But, the <ancient clock> is not ticking, and the hands have stopped. You notice a perfectly round hole in the middle of the marble. Inside, you see that the <ancient clock> operates with a complicated set of gears. The pendulum of the <ancient clock> appears to be missing.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "Evanna"],
                                 uSentence ["shake", "Evanna"],
                                 uSentence ["walk", "to", "Evanna"],
                                 uSentence ["approach", "Evanna"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "jade amulet"), "Your friend <Evanna>, lies in a deep slumber at the base of the <ancient clock>. You call her name and shake her, but nothing you do can wake her. You notice that her <jade amulet> is gently glowing.", []),
                                        (InInventory "jade amulet", "Your friend <Evanna>, lies in a deep slumber at the base of the <ancient clock>. You call her name and shake her, but nothing you do can wake her.", [])
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
                    sentences = [uSentence ["inspect", "jade amulet"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "jade amulet"), "<Evanna> is wearing her <jade amulet>. It is glowing gently.", []),
                                        (InInventory "jade amulet", "You have taken <Evanna>'s <jade amulet>. It is glowing gently.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["inspect", "square"],
                                 uSentence ["look around", "square"]],
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
                    sentences = [uSentence ["look", "west"],
                                 uSentence ["inspect", "home"]],
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
                                 uSentence ["inspect", "tower"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see the <wizard's tower>, a single massive crystal growing out of the great desert. It serves as Isvald's home.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "west"],
                                 uSentence ["walk", "home"],
                                 uSentence ["approach", "home"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You walk through the deserted streets of your <village>, passing sleeping bodies on your way home.", [RemoveFlag "village described"])
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
                                            (CTrue, "You walk towards the looming <wizard's tower>.\n", [RemoveFlag "village described"])
                                        ],
                                stateChanges = [SceneChange "tower"]
                            }
                        ]
                }
            ]
    }

towerExteriorDescriptionString :: String
towerExteriorDescriptionString = "You are standing at the base of the <wizard's tower>, inhabited by Isvald, the <village>'s resident wizard. The tower is a giant spiral of blue crystal, maybe 100 feet tall, with no visible entrances. Beyond the tower is the great desert, which stretches to the horizon.\n\nThe <village square> is to the <north>. In front of you is a <pedestal> with some strange <indentations>."

gatewayDescriptionString :: String
gatewayDescriptionString = " A cloaked <gateway> has been opened in the side of the <tower>."

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
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["inspect", "tower"]],
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
                    sentences = [uSentence ["inspect", "pedestal"],
                                 uSentence ["inspect", "indentations"],
                                 uSentence ["walk", "to", "pedestal"],
                                 uSentence ["approach", "pedestal"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "You see a <pedestal> made out of marble. It rises up to chest height and has three <indentations>: a <chrome indentation>, a <jade indentation>, and a <ruby indentation>. ", []),
                                        (CNot (FlagSet "chrome amulet installed"), "The <chrome indentation> matches the color of the <chrome amulet> your grandfather gave you in your <inventory>.", []),
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
                                        (CTrue, "You already have the <ruby amulet>.", [])
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
                    sentences = [uSentence ["inspect", "chrome indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "chrome amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The <chrome indentation> bears the <chrome amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The empty <chrome indentation> bears the shape of the <chrome amulet>. It matches the color of the <chrome amulet> your grandfather gave you.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "jade indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "jade amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The <jade indentation> bears the <jade amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The empty <jade indentation> bears the shape of the <jade amulet>.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "ruby indentation"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "ruby amulet installed",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The <ruby indentation> bears the <ruby amulet>.", [])
                                    ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CTrue, "The empty <ruby indentation> bears the shape of the <ruby amulet>.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["insert", "chrome amulet"],
                                 uSentence ["insert", "chrome amulet", "into", "pedestal"],
                                 uSentence ["put", "chrome amulet", "into", "pedestal"],
                                 uSentence ["put", "chrome amulet", "on", "pedestal"],
                                 uSentence ["use", "chrome amulet", "on", "pedestal"],
                                 uSentence ["use", "chrome amulet", "with", "pedestal"],
                                 uSentence ["insert", "chrome amulet", "into", "indentation"],
                                 uSentence ["put", "chrome amulet", "into", "indentation"],
                                 uSentence ["put", "chrome amulet", "on", "indentation"],
                                 uSentence ["use", "chrome amulet", "on", "indentation"],
                                 uSentence ["use", "chrome amulet", "with", "indentation"],
                                 uSentence ["use", "chrome amulet", "with", "indentation"],
                                 uSentence ["insert", "chrome amulet", "into", "chrome indentation"],
                                 uSentence ["put", "chrome amulet", "into", "chrome indentation"],
                                 uSentence ["put", "chrome amulet", "on", "chrome indentation"],
                                 uSentence ["use", "chrome amulet", "on", "chrome indentation"],
                                 uSentence ["use", "chrome amulet", "with", "chrome indentation"]],
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
                    sentences = [uSentence ["insert", "jade amulet"],
                                 uSentence ["insert", "jade amulet", "into", "pedestal"],
                                 uSentence ["put", "jade amulet", "into", "pedestal"],
                                 uSentence ["put", "jade amulet", "on", "pedestal"],
                                 uSentence ["use", "jade amulet", "on", "pedestal"],
                                 uSentence ["use", "jade amulet", "with", "pedestal"],
                                 uSentence ["insert", "jade amulet", "into", "indentation"],
                                 uSentence ["put", "jade amulet", "into", "indentation"],
                                 uSentence ["put", "jade amulet", "on", "indentation"],
                                 uSentence ["use", "jade amulet", "on", "indentation"],
                                 uSentence ["use", "jade amulet", "with", "indentation"],
                                 uSentence ["insert", "jade amulet", "into", "jade indentation"],
                                 uSentence ["put", "jade amulet", "into", "jade indentation"],
                                 uSentence ["put", "jade amulet", "on", "jade indentation"],
                                 uSentence ["use", "jade amulet", "on", "jade indentation"],
                                 uSentence ["use", "jade amulet", "with", "jade indentation"]],
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
                    sentences = [uSentence ["insert", "ruby amulet"],
                                 uSentence ["insert", "ruby amulet", "into", "pedestal"],
                                 uSentence ["put", "ruby amulet", "into", "pedestal"],
                                 uSentence ["put", "ruby amulet", "on", "pedestal"],
                                 uSentence ["use", "ruby amulet", "on", "pedestal"],
                                 uSentence ["use", "ruby amulet", "with", "pedestal"],
                                 uSentence ["insert", "ruby amulet", "into", "indentation"],
                                 uSentence ["put", "ruby amulet", "into", "indentation"],
                                 uSentence ["put", "ruby amulet", "on", "indentation"],
                                 uSentence ["use", "ruby amulet", "on", "indentation"],
                                 uSentence ["use", "ruby amulet", "with", "indentation"],
                                 uSentence ["insert", "ruby amulet", "into", "ruby indentation"],
                                 uSentence ["put", "ruby amulet", "into", "ruby indentation"],
                                 uSentence ["put", "ruby amulet", "on", "ruby indentation"],
                                 uSentence ["use", "ruby amulet", "on", "ruby indentation"],
                                 uSentence ["use", "ruby amulet", "with", "ruby indentation"]],
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
                    sentences = [uSentence ["inspect", "gateway"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "gateway opened",
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (FlagSet "gateway opened", gatewayDescriptionString, [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },

                Interaction
                {
                    sentences = [uSentence ["walk", "through", "gateway"],
                                 uSentence ["walk", "into", "gateway"],
                                 uSentence ["walk", "into", "tower"],
                                 uSentence ["walk", "into", "tower", "through", "gateway"],
                                 uSentence ["walk", "in through", "gateway"],
                                 uSentence ["walk", "into", "tower", "using", "gateway"],
                                 uSentence ["walk", "inside"],
                                 uSentence ["enter"],
                                 uSentence ["enter", "gateway"],
                                 uSentence ["enter", "tower"],
                                 uSentence ["approach", "tower"],
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
                                        (CTrue, "You walk through the cloaked <gateway>.\n", [])
                                    ],
                                stateChanges = [SceneChange "tower ground floor"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                    [
                                        (CTrue, "You don't see a way to enter the <tower>.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

wizardTowerGroundFloorDescriptionString :: String
wizardTowerGroundFloorDescriptionString = "You are inside the <wizard's tower>, inhabited by Isvald, the <village>'s resident wizard. The tall <tower>'s blue crystal interiors playfully bounce the sunlight streaming in.\n\nTo your <west>, is the cloaked <gateway> that let you in. To your <east>, is an <elevator> with a <button> to summon it, which seems to be the only way to get past the foyer to the other rooms in the <tower>."

wizardTowerGroundFloorScene :: Scene
wizardTowerGroundFloorScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizardTowerGroundFloor described"), wizardTowerGroundFloorDescriptionString, [SetFlag "wizardTowerGroundFloor described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["inspect", "tower"],
                                 uSentence ["look around", "tower"]],
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
                                 uSentence ["leave", "tower", "using", "gateway"],
                                 uSentence ["leave", "through", "gateway"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                        [
                                            (CTrue, "You exit through the cloaked <gateway>, and find yourself at the base of <wizard's tower>.", [RemoveFlag "wizardTowerGroundFloor described"])
                                        ],
                                stateChanges = [SceneChange "tower"]
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
                    sentences = [uSentence ["inspect", "elevator"],
                                 uSentence ["look", "in", "elevator"],
                                 uSentence ["look", "east"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (FlagSet "elevator arrived"),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the <tower>.\n", [])
                                       ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "You see a call <button> to summon the elevator.", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "elevator arms"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator>'s glass arms have an elaborate pattern etched across them.", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "elevator"],
                                 uSentence ["walk", "in", "elevator"],
                                 uSentence ["jump", "into", "elevator"],
                                 uSentence ["jump", "in", "elevator"],
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
                                           (CTrue, "You enter the cool glass cube <elevator>. The doors slide shut as you take in a 360-degree view of the tower.\n", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator ground floor", RemoveFlag "wizardTowerGroundFloor described"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["pick", "music room"],
                                 uSentence ["press", "music room"],
                                 uSentence ["go", "to", "music room"],
                                 uSentence ["pick", "guest room"],
                                 uSentence ["press", "guest room"],
                                 uSentence ["go", "to", "guest room"],
                                 uSentence ["pick", "bedroom"],
                                 uSentence ["press", "bedroom"],
                                 uSentence ["go", "to", "bedroom"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator arrived",
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "Yes! But first, let's enter the <elevator>!", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                }
            ]
    }

elevatorDescriptionString :: String
elevatorDescriptionString = "The <elevator> has a magic <button panel> so you can pick a destination for your ride."
elevatorScene :: Scene
elevatorScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "elevator described"), elevatorDescriptionString, [SetFlag "elevator described"])],
        interactions = 
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "view"],
                                 uSentence ["look around", "at", "view"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "As you float across the tower, you see blue crystal flights of stairs separating from, and meeting each other to form pathways to seemingly nowhere. Perhaps, the rooms have cloaked entrances too. When you look down at the base, you realize that you're quite far from solid ground. You instantly look away.", [])],
                                stateChanges = [] 
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "elevator"],
                                 uSentence ["look around"],
                                 uSentence ["look around", "elevator"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription 
                                       [
                                           (CTrue, elevatorDescriptionString, [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "button panel"]],
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
                                 uSentence ["press", "ground floor", "button"],
                                 uSentence ["select", "ground floor"],
                                 uSentence ["ride", "to", "ground floor"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = (CNot (FlagSet "elevator ground floor")),
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator> whisks you away, and brings you to the ground floor with a gentle bounce. Exit <elevator> to hop off here, or ride to a different room by picking from the magical <button panel>.", [])
                                       ],
                                stateChanges = [SetFlag "elevator ground floor", RemoveFlag "elevator bedroom", RemoveFlag "elevator music room", RemoveFlag "elevator guest room"]
                            }
                        ]
               },
               Interaction
               {
                   sentences = [uSentence ["go", "to", "bedroom"],
                                uSentence ["press", "bedroom"],
                                uSentence ["press", "bedroom", "button"],
                                uSentence ["select", "bedroom"],
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
                                uSentence ["press", "music room", "button"],
                                uSentence ["select", "music room"],
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
                                uSentence ["press", "guest room", "button"],
                                uSentence ["select", "guest room"],
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
                                uSentence ["leave", "elevator"],
                                uSentence ["leave"],
                                uSentence ["hop off"],
                                uSentence ["walk", "outside"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator ground floor"),
                               conditionalDescription = 
                                   ConditionalDescription 
                                      [
                                          (CTrue, "You step out to the <ground floor>, and the <elevator> lifts off.\n", [])
                                      ],
                               stateChanges = [SceneChange "tower ground floor", RemoveFlag "elevator ground floor", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator bedroom"),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <bedroom>, and the <elevator> drifts away.\n", [])
                                      ],
                               stateChanges = [SceneChange "tower bedroom", RemoveFlag "elevator bedroom", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator music room"),
                               conditionalDescription = 
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <music room>, and the <elevator> cruises away.\n", [])
                                      ],
                               stateChanges = [SceneChange "tower music room", RemoveFlag "elevator music room", RemoveFlag "elevator described"]
                           },
                           ConditionalAction
                           {
                               condition = (FlagSet "elevator guest room"),
                               conditionalDescription =
                                   ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <guest room>, and the <elevator> slides away.\n", [])
                                      ],
                               stateChanges = [SceneChange "tower guest room", RemoveFlag "elevator guest room", RemoveFlag "elevator described"]
                           }
                       ]
               },
               Interaction
               {
                    sentences = [uSentence ["inspect", "guest room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator guest room",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You see a simple, yet cozy room. It is setup with a four poster <bed>, plain wooden chest of drawers, and a full length mirror..", [])
                                      ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <guest room> from the <button panel> to fly to the <guest room>.", [])
                                      ],
                                stateChanges = []
                            }
                      
                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["inspect", "music room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator music room",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You see a harmonious room. The walls are white, with black musical notes running across them.", [])
                                      ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <music room> from the <button panel> to fly to the <music room>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["inspect", "bedroom"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator bedroom",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You see an ornate <bedroom>. The <wizard>, <Isvald>, is sleeping on the <bed>.", [])
                                      ],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <bedroom> from the <button panel> to fly to the <bedroom>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["inspect", "ground floor"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator ground floor",
                                conditionalDescription = 
                                   ConditionalDescription 
                                      [
                                          (CTrue, "You see the <wizard's tower> foyer. The tall <tower>'s blue crystal interiors playfully bounce the sunlight streaming in.", [])
                                      ],
                                stateChanges = [] 
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <ground floor> from the <button panel> to fly to the <ground floor>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["enter", "guest room"],
                                 uSentence ["walk", "to", "guest room"],
                                 uSentence ["approach", "guest room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator guest room",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <guest room>, and the <elevator> slides away.\n", [])
                                      ],
                                stateChanges = [SceneChange "tower guest room", RemoveFlag "elevator guest room", RemoveFlag "elevator described"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <guest room> from the <button panel> to fly to the <guest room>.", [])
                                      ],
                                stateChanges = []
                            }
                      
                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["enter", "music room"],
                                 uSentence ["walk", "to", "music room"],
                                 uSentence ["approach", "music room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator music room",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <music room>, and the <elevator> cruises away.\n", [])
                                      ],
                                stateChanges = [SceneChange "tower music room", RemoveFlag "elevator music room", RemoveFlag "elevator described"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <music room> from the <button panel> to fly to the <music room>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["enter", "bedroom"],
                                 uSentence ["walk", "to", "bedroom"],
                                 uSentence ["approach", "bedroom"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator bedroom",
                                conditionalDescription =
                                    ConditionalDescription
                                      [
                                          (CTrue, "You step out into the <bedroom>, and the <elevator> drifts away.\n", [])
                                      ],
                                stateChanges = [SceneChange "tower bedroom", RemoveFlag "elevator bedroom", RemoveFlag "elevator described"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <bedroom> from the <button panel> to fly to the <bedroom>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               },
               Interaction
               {
                    sentences = [uSentence ["enter", "ground floor"],
                                 uSentence ["walk", "to", "ground floor"],
                                 uSentence ["approach", "ground floor"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator ground floor",
                                conditionalDescription = 
                                   ConditionalDescription 
                                      [
                                          (CTrue, "You step out to the <ground floor>, and the <elevator> lifts off.\n", [])
                                      ],
                                stateChanges = [SceneChange "tower ground floor", RemoveFlag "elevator ground floor", RemoveFlag "elevator described"] 
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = 
                                    ConditionalDescription
                                      [
                                          (CTrue, "Pick <ground floor> from the <button panel> to fly to the <ground floor>.", [])
                                      ],
                                stateChanges = []
                            }

                        ]
               }
            ]
    }

wizardTowerBedroomDescriptionString :: String
wizardTowerBedroomDescriptionString = "You are in the ornate <bedroom>. The <wizard>, <Isvald>, is sleeping on the <bed>. He is clutching something in his arms. One corner of the room is inhabited by a <chest of drawers>, with a mirror atop. You hear music floating up to the <bedroom>, presumably from the music room. It sounds like a lullaby, and is making you sleepy. Behind you is the <elevator>, with a call <button> to summon it."

wizardTowerBedroomScene :: Scene
wizardTowerBedroomScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizardTowerBedroom described"), wizardTowerBedroomDescriptionString, [SetFlag "wizardTowerBedroom described"])],
        interactions =
            [   
                Interaction
                {
                    sentences = [uSentence ["fall asleep"],
                                 uSentence ["sleep", "upon", "bed"],
                                 uSentence ["lie down", "upon", "bed"],
                                 uSentence ["go", "to", "sleep"],
                                 uSentence ["sleep", "in", "bed"],
                                 uSentence ["go", "to", "bed"],
                                 uSentence ["lie", "in", "bed"],
                                 uSentence ["sleep", "on", "bed"],
                                 uSentence ["lie", "on", "bed"],
                                 uSentence ["enter", "bed"],
                                 uSentence ["sit", "on", "bed"],
                                 uSentence ["climb", "in", "bed"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <bed> is occupied by <Isvald>. The floor here seems like an uncomfortable place to sit or sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look around"],
                                 uSentence ["look"],
                                 uSentence ["look around", "bedroom"],
                                 uSentence ["inspect", "bedroom"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, wizardTowerBedroomDescriptionString, [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <wizard> has a big smile on his face because he is dreaming. He is clutching a <pendulum> to his chest.  A luxurious purple velvet blanket envelopes him.", [])],
                                stateChanges = [SetFlag "wizard seen"]
                            }
                        ]
               },
               Interaction
               {
                   sentences = [uSentence ["touch", "wizard"],
                                uSentence ["shake", "wizard"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "As you get closer to touch the <wizard>, his body starts glowing and emanating radiation. You pull your hand away from the heat, and the glow dims.", [])],
                               stateChanges = [SetFlag "at wizard", RemoveFlag "at chest of drawers", RemoveFlag "at elevator"] 
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
                               condition = FlagSet "wizard seen",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <wizard> is holding the <pendulum> magically. Thus, you are not able to pry it free.", [])],
                               stateChanges = [SetFlag "at wizard", RemoveFlag "at chest of drawers", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                    sentences = [uSentence ["talk", "to", "wizard"],
                                 uSentence ["talk", "with", "wizard"],
                                 uSentence ["shout", "at", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The sleeping <wizard> is in such a deep slumber that he doesn't even stir at your voice.", [])],
                                stateChanges = []
                            }
                        ]
               },
               Interaction
               {
                   sentences = [uSentence ["inspect", "bed"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The magnificent four poster <bed> looks quite comfortable. Alas, no one save the <wizard> can occupy it. The <bed>'s canopy looks lovely even drawn aside.", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["inspect", "chest of drawers"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "The eye-catching distressed <dresser> has a gold-hued finish that accents the jewel-toned tabletop trinkets. It has three <drawers> to stow away unwanted clutter.", [])],
                               stateChanges = []
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["inspect", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.\n", [])],
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
                    sentences = [uSentence ["inspect", "elevator arms"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator>'s glass arms have an elaborate pattern etched across them.", [])
                                       ],
                                stateChanges = []
                            }
                        ]
                },
               Interaction
               {
                   sentences = [uSentence ["open", "chest of drawers"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You see clothes that don't seem like the right size. You also find dusty old books written in a language you don't understand. The magical <drawers> tuck themselves away upon noticing your disinterest.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["open", "first drawer"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You see clothes that don't seem like your size. The <drawer> tucks itself away upon noticing your disinterest.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["open", "second drawer"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You see shoes that don't seem like your style. The <drawer> shies away upon noticing your inattention.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
                           }
                       ]
               },
               Interaction
               {
                   sentences = [uSentence ["open", "third drawer"]],
                   conditionalActions = 
                       [
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "You see books written in a language that you don't understand. The <drawer> puts itself away upon noticing your indifference.", [])],
                               stateChanges = [SetFlag "at chest of drawers", RemoveFlag "at wizard", RemoveFlag "at elevator"]
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
                               conditionalDescription = ConditionalDescription [(CTrue, "You are close enough to the <wizard> that you can see his whiskers flutter with every sleeping breath.", [])],
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
                               conditionalDescription = ConditionalDescription [(CTrue, "To get any closer, you'd have to tuck yourself into a <drawer>", [])],
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
                                 uSentence ["jump", "into", "elevator"],
                                 uSentence ["jump", "in", "elevator"],
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
                                           (CTrue, "You enter the cool glass cube <elevator>. The doors slide shut as you take in a 360-degree view of the tower.\n", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers", SceneChange "elevator", SetFlag "elevator bedroom", RemoveFlag "wizardTowerBedroom described"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is not here. Use the call <button> to summon it.", [])],
                                stateChanges = [SetFlag "at elevator", RemoveFlag "at wizard", RemoveFlag "at chest of drawers"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["pick", "music room"],
                                 uSentence ["press", "music room"],
                                 uSentence ["go", "to", "music room"],
                                 uSentence ["pick", "guest room"],
                                 uSentence ["press", "guest room"],
                                 uSentence ["go", "to", "guest room"],
                                 uSentence ["pick", "ground floor"],
                                 uSentence ["press", "ground floor"],
                                 uSentence ["go", "to", "ground floor"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator arrived",
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "Yes! But first, let's enter the <elevator>!", [])
                                       ],
                                stateChanges = []
                            }
                        ]   
                }      
            ]
    }

wizardTowerMusicRoomDescriptionString :: String
wizardTowerMusicRoomDescriptionString = "You are in a harmonious room. The walls are white, with black musical notes running across them. Their tune interweaves with framed pictures of the wizard playing the piano. At the head of the room, sits a grand piano. Arm chairs with delicate details on the cushions face the piano. The <gramophone> to the right of the piano, seems to be the source of a lullaby that's making you sleepy."

wizardTowerMusicRoomScene :: Scene
wizardTowerMusicRoomScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "music room described"), wizardTowerMusicRoomDescriptionString, [SetFlag "music room described"])],
        interactions = 
            [   
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "This seems like an uncomfortable place to sleep.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["look"],
                                 uSentence ["look around"],
                                 uSentence ["inspect", "music room"],
                                 uSentence ["look around", "music room"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, wizardTowerMusicRoomDescriptionString, [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "gramophone"],
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
                    sentences = [uSentence ["break", "gramophone"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "A magic spell senses danger to the <gramophone> and activates a shield around it. The <gramophone> is still intact.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["stop", "gramophone"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The only way to try to stop the <gramophone> is by lifting the <needle>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["lift", "needle"],
                                 uSentence ["touch", "needle"],
                                 uSentence ["lift", "needle", "off"],
                                 uSentence ["turn", "gramophone", "off"],
                                 uSentence ["turn off", "gramophone"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The music does not turn off. Yet, it dims gently, and you hear the wizard's voice say, \"I am trapped in dream land by accident. I need the pendulum in the dream world to break the spell. You must fall asleep in the real world to join me here in order to save the village.\" The music is distinct again.", [])],
                                stateChanges = [SetFlag "heard wizard"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["touch", "gramophone"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You gently lay your fingers on the <gramophone>. Yet, it feels so delicate. You lift your fingers to avoid harming the dainty piece.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                   sentences = [uSentence ["inspect", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.\n", [])],
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
                    sentences = [uSentence ["inspect", "elevator arms"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator>'s glass arms have an elaborate pattern etched across them.", [])
                                       ],
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
                                 uSentence ["jump", "into", "elevator"],
                                 uSentence ["jump", "in", "elevator"],
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
                                           (CTrue, "You enter the cool glass cube <elevator>. The doors slide shut as you take in a 360-degree view of the tower.\n", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator music room", RemoveFlag "music room described"]
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
                    sentences = [uSentence ["pick", "ground floor"],
                                 uSentence ["press", "ground floor"],
                                 uSentence ["go", "to", "ground floor"],
                                 uSentence ["pick", "guest room"],
                                 uSentence ["press", "guest room"],
                                 uSentence ["go", "to", "guest room"],
                                 uSentence ["pick", "bedroom"],
                                 uSentence ["press", "bedroom"],
                                 uSentence ["go", "to", "bedroom"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator arrived",
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "Yes! But first, let's enter the <elevator>!", [])
                                       ],
                                stateChanges = []
                            }
                        ]   
                }      
            ]
    }
 
wizardTowerGuestRoomDescriptionString :: String
wizardTowerGuestRoomDescriptionString = "You are in a simple, yet cozy room. It contains a four poster <bed>, plain wooden chest of drawers, and a full length mirror. The <elevator>'s call <button> is behind you."

wizardTowerGuestRoomScene :: Scene
wizardTowerGuestRoomScene = 
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "wizard tower guest room described"), wizardTowerGuestRoomDescriptionString, [SetFlag "wizard tower guest room described"])],
        interactions =
            [   
                Interaction
                {
                    sentences = [uSentence ["look around"],
                                 uSentence ["look"],
                                 uSentence ["look around", "guest room"],
                                 uSentence ["inspect", "guest room"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, wizardTowerGuestRoomDescriptionString, [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["approach", "bed"],
                                 uSentence ["inspect", "bed"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The four poster <bed> and a soft blanket looks inviting", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["fall asleep"],
                                 uSentence ["sleep", "upon", "bed"],
                                 uSentence ["lie down", "upon", "bed"],
                                 uSentence ["go", "to", "sleep"],
                                 uSentence ["sleep", "in", "bed"],
                                 uSentence ["go", "to", "bed"],
                                 uSentence ["lie", "in", "bed"],
                                 uSentence ["sleep", "on", "bed"],
                                 uSentence ["lie", "on", "bed"],
                                 uSentence ["enter", "bed"],
                                 uSentence ["climb", "in", "bed"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "heard wizard",
                                conditionalDescription = ConditionalDescription [(CTrue, "You fall into a serene slumber as soon as your head hits the pillow. You feel a gentle breeze, and are transported into Dreamland.\n", [])],
                                stateChanges = [SceneChange "starfield"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "Anh anh aanh! Sleep now, and your town is lost forever! See if you can find and eliminate whatever is inducing this sleepy state.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                   sentences = [uSentence ["sit", "on", "bed"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "heard wizard",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <bed> is so cozy that the second you sit on it, you lie down. You fall into a serene slumber as soon as your head hits the pillow. You feel a gentle breeze, and are transported into Dreamland.", [])],
                               stateChanges = [SceneChange "starfield"]
                           },
                           ConditionalAction
                           {
                               condition = CTrue,
                               conditionalDescription = ConditionalDescription [(CTrue, "Anh anh aanh! If you sit on this cozy <bed> now, it's not long before you fall asleep! Sleep now, and your town is lost forever! See if you can find and eliminate whatever is inducing this sleepy state.", [])],
                               stateChanges = []
                           }
                       ]
                },
                Interaction
                {
                   sentences = [uSentence ["inspect", "elevator"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = FlagSet "elevator arrived",
                               conditionalDescription = ConditionalDescription [(CTrue, "The <elevator> is a cool glass cube that allows a 360-degree view of the tower.\n", [])],
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
                    sentences = [uSentence ["inspect", "elevator arms"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                       [
                                           (CTrue, "The <elevator>'s glass arms have an elaborate pattern etched across them.", [])
                                       ],
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
                                 uSentence ["jump", "into", "elevator"],
                                 uSentence ["jump", "in", "elevator"],
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
                                           (CTrue, "You enter the cool glass cube <elevator>. The doors slide shut as you take in a 360-degree view of the tower.\n", [RemoveFlag "elevator arrived"]) 
                                       ],
                                stateChanges = [SceneChange "elevator", SetFlag "elevator guest room", RemoveFlag "wizard tower guest room described"]
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
                    sentences = [uSentence ["pick", "music room"],
                                 uSentence ["press", "music room"],
                                 uSentence ["go", "to", "music room"],
                                 uSentence ["pick", "ground floor"],
                                 uSentence ["press", "ground floor"],
                                 uSentence ["go", "to", "ground floor"],
                                 uSentence ["pick", "bedroom"],
                                 uSentence ["press", "bedroom"],
                                 uSentence ["go", "to", "bedroom"]],
                    conditionalActions = 
                        [
                            ConditionalAction
                            {
                                condition = FlagSet "elevator arrived",
                                conditionalDescription = 
                                    ConditionalDescription
                                       [
                                           (CTrue, "Yes! But first, let's enter the <elevator>!", [])
                                       ],
                                stateChanges = []
                            }
                        ]   
                }      
            ]
    }

starFieldDescriptionString :: String
starFieldDescriptionString = "You open your eyes and find yourself floating in a sea of <stars>. As the world darkens, the <stars> get brighter. They are so close that you can touch them.\n\nTo your left you see a constellation which looks like a <clock>." ++
                             "To your right you see a constellation which you know is called <hypnotism>. Above you is a constellation called <cupcake>. It seems like you could <float> right over to them."

starFieldScene :: Scene
starFieldScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "star field described"), starFieldDescriptionString, [SetFlag "star field described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You aren't feeling tired.", [])],
                                stateChanges = []
                            }
                        ]
                },
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You already have a <star>.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You pluck a <star> out of the sky and put it in your pocket.", [])],
                                stateChanges = [AddToInventory "star"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["get", "stars"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "Greedy! Greedy! You can only take one <star> at a time.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["touch", "star"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You reach out and touch a <star>, it feels warm.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "star"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You see a star twinkling next to you, it looks so tiny from here.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "stars"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You see <stars> twinkling all around you.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "clock constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You see a constellation shaped like a <clock> to your left.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "hypnotism constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You see a constellation shaped like a <hypnotism> to your right.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "cupcake constellation"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You see a constellation shaped like a <cupcake> above you.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "into", "clock constellation"],
                                 uSentence ["walk", "to", "clock constellation"],
                                 uSentence ["approach", "clock constellation"],
                                 uSentence ["fly", "into", "clock constellation"],
                                 uSentence ["fly", "to", "clock constellation"],
                                 uSentence ["walk", "left"],
                                 uSentence ["fly", "left"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the <clock> constellation.\n", [])],
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
                                 uSentence ["fly", "to", "hypnotism constellation"],
                                 uSentence ["walk", "right"],
                                 uSentence ["fly", "right"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the <hypnotism> constellation.\n", [])],
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
                                 uSentence ["fly", "to", "cupcake constellation"],
                                 uSentence ["walk", "up"],
                                 uSentence ["fly", "up"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float towards the <cupcake> constellation.\n", [])],
                            stateChanges = [SceneChange "cupcake",
                                            RemoveFlag "star field described"]
                        }
                    ]
                }
            ]
    }

clockDescriptionString :: String
clockDescriptionString = "You are on the outside of a <prison> made of stars. Right across from the twinkling <prison> bars is a grandfather <clock> that's missing a pendulum. The <star field> is to your <right>."
clockDescriptionWithPendulumString = "You are on the outside of a <prison> made of stars. Right across from the twinkling <prison> bars is a grandfather <clock> that's missing a <pendulum>. The <star field> is to your <right>."

clockScene :: Scene
clockScene = 
    Scene
    {
        sceneDescription = ConditionalDescription 
                           [
                               ((CAnd (CNot (FlagSet "clock described")) (CNot (InInventory "pendulum"))), clockDescriptionString, [SetFlag "clock described"]),
                               ((CAnd (CNot (FlagSet "clock described")) (InInventory "pendulum")), clockDescriptionWithPendulumString, [SetFlag "clock described"])
                           ],
        interactions = 
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You aren't feeling tired.", [])],
                                stateChanges = []
                            }
                        ]
                },
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
                    sentences = [uSentence ["inspect", "clock"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "pendulum",
                                conditionalDescription = ConditionalDescription[(CTrue, "The mighty grandfather <clock> feels incomplete without its <pendulum>.", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "The mighty grandfather <clock> feels incomplete without its pendulum.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "prison"],
                                 uSentence ["walk", "to", "prison"],
                                 uSentence ["approach", "prison"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "The <prison> holds all the people from the village. You spot your <parents>, <Jorryn>, and <Evanna>. Even the <wizard> is here.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "Jorryn"]],
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
                    sentences = [uSentence ["inspect", "parents"]],
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
                    sentences = [uSentence ["inspect", "Evanna"]],
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
                    sentences = [uSentence ["inspect", "wizard"]],
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
                                 uSentence ["shout", "to", "parents"],
                                 uSentence ["talk", "with", "parents"],
                                 uSentence ["walk", "to", "parents"],
                                 uSentence ["approach", "parents"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "Your <parents> say, \"Get us out of here! I hope our crops are not wilting away.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "Jorryn"],
                                 uSentence ["shout", "to", "Jorryn"],
                                 uSentence ["talk", "with", "Jorryn"],
                                 uSentence ["walk", "to", "Jorryn"],
                                 uSentence ["approach", "Jorryn"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "<Jorryn> says, \"Please get me out of this nightmare land!\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "Evanna"],
                                 uSentence ["shout", "to", "Evanna"],
                                 uSentence ["talk", "with", "Evanna"],
                                 uSentence ["walk", "to", "Evanna"],
                                 uSentence ["approach", "Evanna"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "<Evanna> says, \"Oh my gosh, this gravel is so rough, why am I sleeping on the road?\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "wizard"],
                                 uSentence ["shout", "to", "wizard"],
                                 uSentence ["talk", "with", "wizard"],
                                 uSentence ["walk", "to", "wizard"],
                                 uSentence ["approach", "wizard"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription[(CTrue, "The <wizard> says, \"Fix <clock> with <pendulum>! Everyone wakes up, and you win!\"", [])],
                                stateChanges = [] 
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "to", "star field"],
                                 uSentence ["fly", "to", "star field"],
                                 uSentence ["approach", "star field"],
                                 uSentence ["walk", "right"],
                                 uSentence ["fly", "right"],
                                 uSentence ["leave"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "Your loved ones blur as you walk away into the <star field>.\n", [])],
                            stateChanges = [SceneChange "starfield",
                                            RemoveFlag "clock described"]
                        }
                    ]
                },
                Interaction
                {
                   sentences = [uSentence ["fix", "clock", "with", "pendulum"],
                                uSentence ["fix", "clock"],
                                uSentence ["put", "pendulum", "in", "clock"],
                                uSentence ["use", "pendulum", "with", "clock"],
                                uSentence ["use", "pendulum", "on", "clock"]],
                   conditionalActions =
                       [
                           ConditionalAction
                           {
                               condition = CNot (InInventory "pendulum"),
                               conditionalDescription = ConditionalDescription[(CTrue, "Alas! You don't have a <pendulum>! Hurry, and find one!", [])],
                               stateChanges = []
                           },
                           ConditionalAction
                           {
                               condition = InInventory "pendulum",
                               conditionalDescription = ConditionalDescription[(CTrue, "The <pendulum> easily fits in the <clock>, and resumes its to-and-fro motion! Everything swirls around, and you find yourself back in your <cottage> with your <parents>, and <Jorryn>. <Jorryn> is stretching out of his sleep. \"I feel so refreshed! Get ready, we have to go tend to the crops!\", he says.", [])],
                               stateChanges = [SceneChange "win"]
                           }
                       ]
                }
            ]
    }

hypnotismDescriptionString :: String
hypnotismDescriptionString = "You step up onto the stage, the lights overhead are blindingly bright. Before you is a ghostly <audience>, to your right on the stage is a <white rabbit> wearing a tuxedo and a top hat. Next to the <white rabbit> there's a giant red <couch>.\n\nYou can see the <star field> off stage to your <left>."

hypnotismDescriptionStringBefore :: String
hypnotismDescriptionStringBefore = hypnotismDescriptionString ++ " The <white rabbit> has a <pendulum> in its paw."

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
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You aren't feeling tired.", [])],
                                stateChanges = []
                            }
                        ]
                },
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
                    sentences = [uSentence ["inspect", "rabbit"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "pendulum"), "The <white rabbit> has a <pendulum> in its paw.", []),
                                        (InInventory "pendulum", "The <white rabbit> is standing next to the red couch.", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["thank", "rabbit"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription
                                    [
                                        (CNot (InInventory "pendulum"), "The <white rabbit> looks at you curiously.", []),
                                        (InInventory "pendulum", "The <white rabbit> says \"No problem.\".", [])
                                    ],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["say hi", "to", "rabbit"],
                                 uSentence ["greet", "rabbit"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The <rabbit> says, \"Hi there! Welcome to my magic show!.\"", [])],
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
                                    ConditionalDescription [(CTrue, "The <rabbit> speaks to you, \"You took my pendulum, so I can't do my trick anymore, but it was worth swapping it for that cake. It was delicious!\"", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The <rabbit> asks, \"Are you here to volunteer for my next trick?, just lie down on the <couch>.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "audience"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "Thousands of hollow eyes stare at you. The ghostly <audience> hovers in the darkness before you, held at bay by the bright lights of the stage.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "couch"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You see a comfortable red <couch> on stage next to the white <rabbit>.", [])],
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
                                    ConditionalDescription [(CTrue, "You lie down on the <couch>. The <rabbit> walks up to you and swings its <pendulum> in front of you. You feel your eyes growing heavy. You fall into a deep slumber, never to awake again. Game over.", [SceneChange "lose"])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You lie down on the <couch>. The <rabbit> walks up to you says \"I hope you're enjoying yourself, I can't do my trick without that pendulum.\".", [])],
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
                                ConditionalDescription [(CTrue, "You reach for the <rabbit>'s <pendulum>. The <rabbit> pulls it away and wags its finger at you. \"Not so fast, if you want this, I want a treat in exchange!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You already have the <rabbit>'s <pendulum>.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "for", "pendulum"],
                                 uSentence ["buy", "pendulum"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CNot (InInventory "pendulum"),
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"I want a treat in exchange!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You already have the <rabbit>'s <pendulum>.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "pendulum", "to", "rabbit"],
                                 uSentence ["give", "rabbit", "pendulum"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CNot (InInventory "pendulum"),
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "\"You don't have a <pendulum>.\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You give the <pendulum> to the <rabbit>. She says \"I thought you needed that. Well, thanks for giving it back to me!\"", [])],
                            stateChanges = [RemoveFromInventory "pendulum"]
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "pendulum"],
                                 uSentence ["ask", "about", "pendulum"],
                                 uSentence ["talk", "about", "pendulum"],
                                 uSentence ["talk", "to", "rabbit", "about", "pendulum"],
                                 uSentence ["talk", "with", "rabbit", "about", "pendulum"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"That was my favorite pendulum, but I have some more backstage.\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"Oh, this is what I use to hypnotize my volunteers!\".", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "clock"],
                                 uSentence ["ask", "about", "clock"],
                                 uSentence ["talk", "about", "clock"],
                                 uSentence ["talk", "to", "rabbit", "about", "clock"],
                                 uSentence ["talk", "with", "rabbit", "about", "clock"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"The time now is 6pm.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "carrot cake"],
                                 uSentence ["ask", "about", "carrot cake"],
                                 uSentence ["talk", "about", "carrot cake"],
                                 uSentence ["talk", "to", "rabbit", "about", "carrot cake"],
                                 uSentence ["talk", "with", "rabbit", "about", "carrot cake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"I love carrots!\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "chocolate cake"],
                                 uSentence ["ask", "rabbit", "about", "lemon cake"],
                                 uSentence ["ask", "rabbit", "about", "cupcake"],
                                 uSentence ["ask", "about", "chocolate cake"],
                                 uSentence ["ask", "about", "lemon cake"],
                                 uSentence ["ask", "about", "cupcake"],
                                 uSentence ["talk", "about", "chocolate cake"],
                                 uSentence ["talk", "about", "lemon cake"],
                                 uSentence ["talk", "about", "cupcake"],
                                 uSentence ["talk", "to", "rabbit", "about", "chocolate cake"],
                                 uSentence ["talk", "to", "rabbit", "about", "lemon cake"],
                                 uSentence ["talk", "to", "rabbit", "about", "cupcake"],
                                 uSentence ["talk", "with", "rabbit", "about", "chocolate cake"],
                                 uSentence ["talk", "with", "rabbit", "about", "lemon cake"],
                                 uSentence ["talk", "with", "rabbit", "about", "cupcake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"Those are nice I guess, but not my favorite flavor.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["ask", "rabbit", "about", "hypnotism constellation"],
                                 uSentence ["ask", "rabbit", "about", "hypnosis"],
                                 uSentence ["ask", "rabbit", "about", "couch"],
                                 uSentence ["ask", "about", "hypnotism constellation"],
                                 uSentence ["ask", "about", "hypnosis"],
                                 uSentence ["ask", "about", "couch"],
                                 uSentence ["talk", "about", "hypnotism constellation"],
                                 uSentence ["talk", "about", "hypnosis"],
                                 uSentence ["talk", "about", "couch"],
                                 uSentence ["talk", "to", "rabbit", "about", "hypnotism constellation"],
                                 uSentence ["talk", "to", "rabbit", "about", "hypnosis"],
                                 uSentence ["talk", "to", "rabbit", "about", "couch"],
                                 uSentence ["talk", "with", "rabbit", "about", "hypnotism constellation"],
                                 uSentence ["talk", "with", "rabbit", "about", "hypnosis"],
                                 uSentence ["talk", "with", "rabbit", "about", "couch"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> tells you \"If you want to watch the show, you've got to buy a ticket like everyone else. You can join the <audience> if you volunteer for my trick though! Just lie down on the <couch>.\"", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "carrot cake", "to", "rabbit"],
                                 uSentence ["give", "rabbit", "carrot cake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "carrot cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "carrot cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> takes the <cake>. She says \"I love carrots! You've got a deal, take my pendulum!\"", [])],
                            stateChanges = [AddToInventory "pendulum", RemoveFromInventory "carrot cake"]
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a <carrot cake>.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "chocolate cake", "to", "rabbit"],
                                 uSentence ["give", "rabbit", "chocolate cake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "chocolate cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "chocolate cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a <chocolate cake>.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "lemon cake", "to", "rabbit"],
                                 uSentence ["give", "rabbit", "lemon cake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "lemon cake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "lemon cake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a <lemon cake>.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "cupcake", "to", "rabbit"],
                                 uSentence ["give", "rabbit", "cupcake"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = InInventory "pendulum",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"Thanks but I already gave you my pendulum!\"", [])],
                            stateChanges = [RemoveFromInventory "cupcake"]
                        },
                        ConditionalAction
                        {
                            condition = InInventory "cupcake",
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "The <rabbit> says \"I don't like this flavor, no deal!\"", [])],
                            stateChanges = []
                        },
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription =
                                ConditionalDescription [(CTrue, "You don't have a <cupcake>.", [])],
                            stateChanges = []
                        }
                    ]
                }
            ]
    }

cupcakeDescriptionString :: String
cupcakeDescriptionString = "You step out onto a green field that stretches to the horizon, above you is a <star field> in the sky. There are <cupcake> flowers growing all over the field, in every color of the rainbow. Before you, an <elderly woman> sits at a <table> with a red and white polka-dot tablecloth. There are three <cakes> on the <table>."

cupcakeScene :: Scene
cupcakeScene =
    Scene
    {
        sceneDescription = ConditionalDescription [(CNot (FlagSet "cupcake described"), cupcakeDescriptionString, [SetFlag "cupcake described"])],
        interactions =
            [
                Interaction
                {
                    sentences = [uSentence ["fall asleep"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You aren't feeling tired.", [])],
                                stateChanges = []
                            }
                        ]
                },
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
                    sentences = [uSentence ["inspect", "table"],
                                 uSentence ["walk", "to", "table"],
                                 uSentence ["approach", "table"],
                                 uSentence ["inspect", "cakes"],
                                 uSentence ["walk", "to", "cakes"],
                                 uSentence ["approach", "cakes"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <table> has three <cakes> on it. Each <cake> is covered in pink frosting and chocolate sprinkles. The <left cake> has a <red card> in front of it, the <middle cake> has a <green card> in front of it, and the <right cake> has a <blue card> in front of it.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "Instead of flowers, there are <cupcakes> on stems growing out of the ground.", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You pluck a <cupcake> off its stem.", [])],
                                stateChanges = [AddToInventory "cupcake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "left cake"],
                                 uSentence ["inspect", "middle cake"],
                                 uSentence ["inspect", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <cake> is covered in pink frosting and chocolate sprinkles. Nothing distinguishes it from the other cakes.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "red card"],
                                 uSentence ["read", "red card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <red card> in front of the <left cake> reads:\n\"Most people think I'm sweet but really I'm bitter. I take many forms, I flow when it's hot and snap when it's cold. I soften when it's warm but enough heat makes me brittle. Sweet, bitter, or spicy, I'm universally loved!\nPrice: 1 star\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "green card"],
                                 uSentence ["read", "green card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <green card> in front of the <middle cake> reads:\n\"I have a tough life. I'm born in darkness, you'll see my rosette first. It takes two years to show my true form, and when I do show myself I might get chopped to bits, strung up in front of someone, or shoved into a pile of snow.\nPrice: 1 star\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "blue card"],
                                 uSentence ["read", "blue card"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <blue card> in front of the <right cake> reads:\n\"I'm loved by many for my brilliant hue but parts of me are practical too. I clean stains, help sailors stay healthy at sea, but when I write, my words disappear!\nPrice: 1 star\"", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> says \"Those cards are not for sale.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "elderly woman"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You see an <elderly woman> sitting behind a <table> with three <cakes>. Her hair is tied in a bun and she's wearing a blue apron and a hair net.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["thank", "elderly woman"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> says \"You are most welcome\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["say hi", "to", "elderly woman"],
                                 uSentence ["greet", "elderly woman"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> says \"Hello dear. Are you here to buy a cake? I baked them this morning.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "with", "elderly woman"],
                                 uSentence ["talk", "to", "elderly woman"],
                                 uSentence ["talk", "to", "elderly woman", "about", "cakes"],
                                 uSentence ["talk", "about", "cakes"],
                                 uSentence ["ask", "elderly woman", "about", "cakes"],
                                 uSentence ["ask", "about", "cakes"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> gestures at the <table> and says \"I have three cakes for sale, I baked them all myself this morning.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["talk", "to", "elderly woman", "about", "price"],
                                 uSentence ["talk", "about", "price"],
                                 uSentence ["ask", "elderly woman", "about", "price"],
                                 uSentence ["ask", "about", "price"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> says \"Each cake costs only one star!.\"", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["give", "star", "to", "elderly woman"],
                                 uSentence ["give", "elderly woman", "star"],
                                 uSentence ["pay", "elderly woman", "with", "star"],
                                 uSentence ["pay", "elderly woman", "for", "cake"],
                                 uSentence ["pay", "elderly woman"],
                                 uSentence ["buy", "cake"],
                                 uSentence ["pay", "for", "cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription = ConditionalDescription [(CTrue, "The <elderly woman> gestures at the <table> and says \"Which cake do you want to buy?\"", [])],
                                stateChanges = []
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You don't have a <star> to pay the <elderly woman>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["eat", "left cake"],
                                 uSentence ["eat", "middle cake"],
                                 uSentence ["eat", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription = ConditionalDescription [(CTrue, "You reach out to take one of the <cakes>. The <elderly woman> says \"Those cakes aren't free, you have to pay for them!\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "left cake"],
                                 uSentence ["get", "left cake"],
                                 uSentence ["give", "elderly woman", "star", "for", "left cake"],
                                 uSentence ["ask", "elderly woman", "for", "left cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the <elderly woman> a <star> and in exchange she cuts a slice of the <left cake> and hands it to you. \"Good choice.\" she says \"That one's my favorite!\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "chocolate cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The <elderly woman> says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "middle cake"],
                                 uSentence ["get", "middle cake"],
                                 uSentence ["give", "elderly woman", "star", "for", "middle cake"],
                                 uSentence ["ask", "elderly woman", "for", "middle cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the <elderly woman> a <star> and in exchange she cuts a slice of the <middle cake> and hands it to you. \"That one huh?\" she says \"I'm not a big fan of that one, but I hear it's popular with some folks.\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "carrot cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The <elderly woman> says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["buy", "right cake"],
                                 uSentence ["get", "right cake"],
                                 uSentence ["give", "elderly woman", "star", "for", "right cake"],
                                 uSentence ["ask", "elderly woman", "for", "right cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "You give the <elderly woman> a <star> and in exchange she cuts a slice of the <right cake> and hands it to you. The <elderly woman> says \"What a strange choice, well I'm sure you'll enjoy it anyway.\".", [])],
                                stateChanges = [RemoveFromInventory "star", AddToInventory "lemon cake"]
                            },
                            ConditionalAction
                            {
                                condition = CTrue,
                                conditionalDescription =
                                    ConditionalDescription [(CTrue, "The <elderly woman> says \"The cakes aren't free, come back when you can pay.\".", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "star field"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You see a <star field> above you.", [])],
                            stateChanges = []
                        }
                    ]
                },
                Interaction
                {
                    sentences = [uSentence ["walk", "to", "star field"],
                                 uSentence ["approach", "star field"],
                                 uSentence ["fly", "to", "star field"],
                                 uSentence ["fly", "up"],
                                 uSentence ["leave"]],
                    conditionalActions =
                    [
                        ConditionalAction
                        {
                            condition = CTrue,
                            conditionalDescription = ConditionalDescription [(CTrue, "You float back to the <star field>.\n", [])],
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
                    sentences = [uSentence ["flip"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You do a backflip.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["sing"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = CTrue, --Always do this
                                conditionalDescription = ConditionalDescription [(CTrue, "You sing a happy song.", [])],
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
                    sentences = [uSentence ["inspect", "star"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "star",
                                conditionalDescription = ConditionalDescription [(CTrue, "You inspect the <star>, it twinkles in your hand.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "pendulum"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "pendulum",
                                conditionalDescription = ConditionalDescription [(CTrue, "You inspect the <pendulum> of the <ancient clock>. It seems to be intact.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "chrome amulet"]],
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
                    sentences = [uSentence ["inspect", "jade amulet"]],
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
                    sentences = [uSentence ["inspect", "ruby amulet"]],
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
                    sentences = [uSentence ["inspect", "chocolate cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "chocolate cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of <chocolate cake>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "carrot cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "carrot cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of <carrot cake>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "lemon cake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "lemon cake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a slice of <lemon cake>.", [])],
                                stateChanges = []
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "cupcake"]],
                    conditionalActions =
                        [
                            ConditionalAction
                            {
                                condition = InInventory "cupcake",
                                conditionalDescription = ConditionalDescription [(CTrue, "It's a multi-colored <cupcake>.", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the <chocolate cake>. It's delicious!", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the <carrot cake>. It's delicious!", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the <lemon cake>. It's delicious!", [])],
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
                                conditionalDescription = ConditionalDescription [(CTrue, "You eat the <cupcake>. It's delicious!", [])],
                                stateChanges = [RemoveFromInventory "cupcake"]
                            }
                        ]
                },
                Interaction
                {
                    sentences = [uSentence ["inspect", "me"],
                                 uSentence ["inspect", "myself"]],
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
