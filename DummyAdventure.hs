--DummyAdventure.hs
--Copyright Laurence Emms 2018
--Module with a dummy vocabulary and adventure for testing

module DummyAdventure (allVerbs, allNouns, allPrepositions, allTokens, startInventory, startFlags, allScenes) where

import NaturalLanguageLexer
import NaturalLanguageParser
import NarrativeGraph

allVerbs :: [Token]
allVerbs = [TokenVerb ["get", "take", "pick up"],
            TokenVerb ["put", "place", "put down"],
            TokenVerb ["throw", "pitch"],
            TokenVerb ["give"],
            TokenVerb ["select", "pick"],
            TokenVerb ["look"],
            TokenVerb ["look around"],
            TokenVerb ["use"],
            TokenVerb ["jump"],
            TokenVerb ["move", "walk", "go", "run"],
            TokenVerb ["move down", "walk down", "go down", "run down"],
            TokenVerb ["move up", "walk up", "go up", "run up"],
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
            TokenNoun "white door",
            TokenNoun "window",
            TokenNoun "chest",
            TokenNoun "chair",
            TokenNoun "table",
            TokenNoun "pen",
            TokenNoun "north entrance",
            TokenNoun "exit",
            TokenNoun "key",
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
                   TokenPreposition ["together with"]]

allTokens :: [Token]
allTokens = allNouns ++ allVerbs ++ allPrepositions

startInventory :: Inventory
startInventory = Inventory ["fork"]

startFlags :: Flags
startFlags = Flags ["started game"]

scene0Description :: SceneDescription
scene0Description = SceneDescription {description = "You're standing in a green room. The room has a [white door]. ",
                                      conditionalDescriptions = [(NotCondition (InInventory "key"), "There is a [key] on the floor. "),
                                                                 (NotCondition (FlagSet "opened white door"), "The [white door] is closed. "),
                                                                 (FlagSet "opened white door", "The [white door] is open. ")]}

scene0 :: Scene
scene0 = Scene {sceneDescription = scene0Description,
                interactions = [Interaction {sentences = [SimpleSentence (TokenVerb ["get", "take", "pick up"])
                                                                         (TokenNoun "key")],
                                             interactionDescription = "You pick up the [key].",
                                             preconditions = [(NotCondition (InInventory "key"), "The [key] is already in your inventory.")],
                                             addedObjects = ObjectChanges ["key"],
                                             removedObjects = ObjectChanges [],
                                             setFlags = FlagChanges [],
                                             removeFlags = FlagChanges [],
                                             nextScene = Nothing},
                                Interaction {sentences = [SimpleSentence (TokenVerb ["open"])
                                                                         (TokenNoun "white door")],
                                             interactionDescription = "The door is locked.",
                                             preconditions = [],
                                             addedObjects = ObjectChanges [],
                                             removedObjects = ObjectChanges [],
                                             setFlags = FlagChanges [],
                                             removeFlags = FlagChanges [],
                                             nextScene = Nothing},
                                Interaction {sentences = [Phrase (TokenVerb ["look around"])],
                                             interactionDescription = "You take a look around.",
                                             preconditions = [],
                                             addedObjects = ObjectChanges [],
                                             removedObjects = ObjectChanges [],
                                             setFlags = FlagChanges [],
                                             removeFlags = FlagChanges [],
                                             nextScene = Nothing},
                                Interaction {sentences = [ComplexSentence (TokenVerb ["unlock"])
                                                                          (TokenNoun "white door")
                                                                          (TokenPreposition ["with"])
                                                                          (TokenNoun "key"),
                                                          ComplexSentence (TokenVerb ["open"])
                                                                          (TokenNoun "white door")
                                                                          (TokenPreposition ["with"])
                                                                          (TokenNoun "key")],
                                             interactionDescription = "You unlock the white door with the key and open it.",
                                             preconditions = [(InInventory "key", "You need a [key] to open the [white door].")],
                                             addedObjects = ObjectChanges [],
                                             removedObjects = ObjectChanges [],
                                             setFlags = FlagChanges ["opened white door"],
                                             removeFlags = FlagChanges [],
                                             nextScene = Nothing},
                               Interaction {sentences = [SimplePrepositionSentence (TokenVerb ["move", "walk", "go", "run"])
                                                                                   (TokenPreposition ["through"])
                                                                                   (TokenNoun "white door")],
                                            interactionDescription = "You walk through the [white door]. Congratulations, you escaped the green room!",
                                            preconditions = [(FlagSet "opened white door", "The [white door] is closed.")],
                                            addedObjects = ObjectChanges [],
                                            removedObjects = ObjectChanges [],
                                            setFlags = FlagChanges [],
                                            removeFlags = FlagChanges [],
                                            nextScene = Just 1}
                               ]}

scene1Description :: SceneDescription
scene1Description = SceneDescription {description = "You escaped the green room!",
                                      conditionalDescriptions = []}

scene1 :: Scene
scene1 = Scene {sceneDescription = scene1Description,
                interactions = []}

allScenes :: ([Scene], [SceneIndex])
allScenes = ([scene0, scene1], --List of scenes
             [1]) --End scenes
