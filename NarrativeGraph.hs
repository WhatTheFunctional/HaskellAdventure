--NarrativeGraph.hs
--Copyright Laurence Emms 2018
--Module for representing a narrative graph in a text adventure

module NarrativeGraph (SceneIndex,
                       Flags(..),
                       Inventory(..),
                       ObjectChanges(..),
                       FlagChanges(..),
                       NarrativeCondition(..),
                       ConditionalDescription(..),
                       ConditionalAction(..),
                       Interaction(..),
                       Scene(..),
                       NarrativeGraph(..),
                       makeNarrativeGraph,
                       evaluateCondition,
                       printConditionalDescription,
                       printSceneDescription,
                       performInteraction) where

import System.IO
import Data.Array

import NaturalLanguageLexer
import NaturalLanguageParser

type SceneIndex = Int
data Flags = Flags [String] deriving (Show, Eq)
data Inventory = Inventory [String] deriving (Show, Eq)
data ObjectChanges = ObjectChanges [String] deriving (Show, Eq)
data FlagChanges = FlagChanges [String] deriving (Show, Eq)

data NarrativeCondition = InInventory String | --Inventory has an item
                          FlagSet String | --Flag is set
                          TrueCondition | --Always true
                          FalseCondition | --Always false
                          NotCondition NarrativeCondition |
                          OrCondition NarrativeCondition NarrativeCondition |
                          AndCondition NarrativeCondition NarrativeCondition deriving (Show, Eq)

data ConditionalDescription = ConditionalDescription {description :: String, --This description is always printed
                                                      conditionalDescriptions :: [(NarrativeCondition, String)]} deriving (Show, Eq)

data ConditionalAction = ConditionalAction {condition :: NarrativeCondition, --Condition under which action occurs
                                            conditionalDescription :: ConditionalDescription, --Description of action
                                            addedObjects :: ObjectChanges, --Objects to add to inventory
                                            removedObjects :: ObjectChanges, --Objects to remove from inventory
                                            setFlags :: FlagChanges, --Flags to set
                                            removedFlags :: FlagChanges, --Flags to remove
                                            nextScene :: Maybe SceneIndex} deriving (Show, Eq)

data Interaction = Interaction {sentences :: [Sentence],
                                conditionalActions :: [ConditionalAction]} deriving (Show, Eq)

data Scene = Scene {sceneDescription :: ConditionalDescription,
                    interactions :: [Interaction]} deriving (Show, Eq)

--By definition the first node in the narrative graph is the start
data NarrativeGraph = NarrativeGraph {nodes :: Array SceneIndex Scene,
                                      endScenes :: [SceneIndex],
                                      graphDefaultScene :: Scene} deriving (Show, Eq)

--Takes a list of scenes and returns a starting index and a NarrativeGraph
makeNarrativeGraph :: [Scene] -> [SceneIndex] -> Scene -> NarrativeGraph
makeNarrativeGraph scenes scenesEndScenes scene
    = NarrativeGraph {nodes = array (0, length scenes) (zip [0..length scenes] scenes),
                      endScenes = scenesEndScenes,
                      graphDefaultScene = scene}

evaluateCondition :: NarrativeCondition -> Inventory -> Flags -> Bool
evaluateCondition TrueCondition _ _ = True
evaluateCondition FalseCondition _ _ = False
evaluateCondition (FlagSet flag) _ (Flags flags) = flag `elem` flags
evaluateCondition (InInventory object) (Inventory inventory) _ = object `elem` inventory
evaluateCondition (NotCondition condition) inventory flags = not (evaluateCondition condition inventory flags)
evaluateCondition (AndCondition condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) && (evaluateCondition condition1 inventory flags)
evaluateCondition (OrCondition condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) || (evaluateCondition condition1 inventory flags)

printSubDescriptions :: Inventory -> Flags -> ConditionalDescription -> IO ()
printSubDescriptions inventory flags (ConditionalDescription {description = thisDescription,
                                                              conditionalDescriptions = []})
    = return () --No sub-descriptions to print
printSubDescriptions inventory flags (ConditionalDescription {description = thisDescription,
                                                              conditionalDescriptions = ((condition, subDescription) : remainingDescriptions)})
    | evaluateCondition condition inventory flags = putStr subDescription >>
                                                    printSubDescriptions inventory flags (ConditionalDescription {description = thisDescription,
                                                                                                                  conditionalDescriptions = remainingDescriptions})
    | otherwise = printSubDescriptions inventory flags (ConditionalDescription {description = thisDescription,
                                                                                conditionalDescriptions = remainingDescriptions})

printConditionalDescription :: Inventory -> Flags -> ConditionalDescription -> IO ()
printConditionalDescription inventory flags conditionalDescription@(ConditionalDescription {description = thisDescription})
    = putStr thisDescription >> printSubDescriptions inventory flags conditionalDescription >> putStr "\n\n" >> hFlush stdout

printSceneDescription :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> IO ()
printSceneDescription (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) sceneIndex inventory flags
    = printConditionalDescription inventory flags thisSceneDescription >> hFlush stdout
        where Scene {sceneDescription = thisSceneDescription,
                     interactions = _} = graphNodes ! sceneIndex

updateFlags :: Flags -> FlagChanges -> FlagChanges -> Flags
updateFlags (Flags flags) (FlagChanges setFlags) (FlagChanges removedFlags) = Flags (setFlags ++ (filter (\x -> not (x `elem` removedFlags)) flags))

updateInventory :: Inventory -> ObjectChanges -> ObjectChanges -> Inventory
updateInventory (Inventory inventory) (ObjectChanges addedObjects) (ObjectChanges removedObjects) = Inventory (addedObjects ++ (filter (\x -> not (x `elem` removedObjects)) inventory))

--Scene transition takes the next scene index, the end scene index list, the current scene index, the inventory and flags, and a conditional action
--Scene transition evaluates to the next state of the game
sceneTransition :: Maybe SceneIndex -> [SceneIndex] -> SceneIndex -> Inventory -> Flags -> ConditionalAction -> IO (Maybe (SceneIndex, Inventory, Flags))
sceneTransition Nothing _ currentScene inventory flags
                conditionalAction@(ConditionalAction {addedObjects = thisAddedObjects,
                                                      removedObjects = thisRemovedObjects,
                                                      setFlags = thisSetFlags,
                                                      removedFlags = thisRemovedFlags})
    = return (Just (currentScene,
                    updateInventory inventory thisAddedObjects thisRemovedObjects,
                    updateFlags flags thisSetFlags thisRemovedFlags)) --If there is no scene transition, return to the current scene with updated inventory and flags
sceneTransition (Just nextScene) endScenes  _ inventory flags
                conditionalAction@(ConditionalAction {addedObjects = thisAddedObjects,
                                                      removedObjects = thisRemovedObjects,
                                                      setFlags = thisSetFlags,
                                                      removedFlags = thisRemovedFlags})
    = if nextScene `elem` endScenes
      then return Nothing --This is an end state for the game
      else return (Just (nextScene,
                         updateInventory inventory thisAddedObjects thisRemovedObjects,
                         updateFlags flags thisSetFlags thisRemovedFlags)) --Transition to the next scene with updated inventory and flags

--Update game state takes an interaction description, fail string, next scene index, end scene index, current scene index, inventory, and flags
--Update game state Scene transition evaluates to the next state of the game
updateGameState :: [SceneIndex] -> SceneIndex -> Inventory -> Flags -> ConditionalAction -> IO (Maybe (SceneIndex, Inventory, Flags))
updateGameState endScenes currentScene inventory flags conditionalAction@(ConditionalAction {nextScene = thisNextScene,
                                                                                             conditionalDescription = thisConditionalDescription})
    = printConditionalDescription inventory flags thisConditionalDescription >>
      sceneTransition thisNextScene endScenes currentScene inventory flags conditionalAction --This conditional action passed all of the preconditions, check whether we need to transition to a new scene

--Perform the interaction and return a tuple of (new scene index, new inventory, new flags)
performConditionalActions :: SceneIndex -> [SceneIndex] -> Inventory -> Flags -> Maybe Interaction -> Maybe Interaction -> IO (Maybe (SceneIndex, Inventory, Flags))
performConditionalActions currentScene _ inventory flags Nothing Nothing
    = putStrLn "That does nothing.\n" >>
      hFlush stdout >>
      return (Just (currentScene, inventory, flags)) --If there are no valid interactions actions but the sentence was valid, just return to the current state
performConditionalActions currentScene
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = []}))
                          defaultSceneInteractions --There are no remaining conditional actions for the current scene
    = performConditionalActions currentScene endScenes inventory flags Nothing defaultSceneInteractions --All current scene conditional actions were exhausted, try default scene interactions
performConditionalActions currentScene
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
                          defaultSceneInteractions -- Ignore default scene interactions if there are still current scene interactions
    | evaluateCondition thisCondition inventory flags = updateGameState endScenes currentScene inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions currentScene endScenes inventory flags (Just (Interaction {sentences = thisSentences,
                                                                                                       conditionalActions = remainingConditionalActions})) defaultSceneInteractions --The condition for the action failed, attempt other actions
performConditionalActions currentScene
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = []}))
    = performConditionalActions currentScene endScenes inventory flags Nothing Nothing --All possible conditional actions are exhausted
performConditionalActions currentScene
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
    | evaluateCondition thisCondition inventory flags = updateGameState endScenes currentScene inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions currentScene endScenes inventory flags Nothing (Just (Interaction {sentences = thisSentences,
                                                                                                               conditionalActions = remainingConditionalActions})) --The condition for the action failed, attempt other actions

--Find an interaction in the interactions list which matches the head of the sentences list
findSentenceInteraction :: [Interaction] -> [Sentence] -> [Sentence] -> Maybe Interaction
findSentenceInteraction [] _ _ = Nothing --You can't match no interactions
findSentenceInteraction (interaction : remainingInteractions) [] allSentences = findInteraction remainingInteractions allSentences --None of the sentences matched this enteraction, try with remaining interactions
findSentenceInteraction allInteractions@(interaction@(Interaction {sentences = thisSentences}) : remainingInteractions) (sentence : remainingSentences) allSentences
    | sentence `elem` thisSentences = Just interaction --This interaction matches this sentence
    | otherwise = findSentenceInteraction allInteractions remainingSentences allSentences --This interaction doesn't match this sentence, try all remaining sentences

findInteraction :: [Interaction] -> [Sentence] -> Maybe Interaction
findInteraction [] _ = Nothing --You can't match no interactions
findInteraction _ [] = Nothing --You can't match no sentences
findInteraction interactions sentences = findSentenceInteraction interactions sentences sentences

processInteraction :: Scene -> Scene -> SceneIndex -> [SceneIndex] -> Inventory -> Flags -> [Sentence] -> IO (Maybe (SceneIndex, Inventory, Flags))
processInteraction (Scene {sceneDescription = _,
                           interactions = thisSceneInteractions})
                   (Scene {sceneDescription = _,
                           interactions = defaultSceneInteractions})
                   currentScene
                   endScenes
                   inventory
                   flags
                   sentences
    = performConditionalActions currentScene endScenes inventory flags interaction defaultInteraction
        where interaction = findInteraction thisSceneInteractions sentences
              defaultInteraction = findInteraction defaultSceneInteractions sentences

--Perform an interaction with the current scene
--Takes the narrative graph, current scene index, inventory, and sentence as input
--Evaluates to Maybe of the next scene index and inventory state
performInteraction :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> [Sentence] -> IO (Maybe (NarrativeGraph, SceneIndex, Inventory, Flags))
performInteraction narrativeGraph sceneIndex inventory flags []
    = putStrLn "Please enter a command." >>
      hFlush stdout >>
      return (Just (narrativeGraph, sceneIndex, inventory, flags)) --If there are no valid sentences, just continue.
performInteraction narrativeGraph@(NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes, graphDefaultScene = thisDefaultScene}) sceneIndex inventory flags sentences
    = hFlush stdout >>
      fmap (\maybeNewSceneTuple -> maybeNewSceneTuple >>=
                                   (\(newSceneIndex, newSceneInventory, newSceneFlags) -> Just (narrativeGraph, newSceneIndex, newSceneInventory, newSceneFlags)))
      ioMaybeNewSceneTuple
        where currentScene = graphNodes ! sceneIndex
              ioMaybeNewSceneTuple = processInteraction currentScene thisDefaultScene sceneIndex graphEndScenes inventory flags sentences
