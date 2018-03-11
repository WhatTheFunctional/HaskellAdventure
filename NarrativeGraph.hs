--NarrativeGraph.hs
--Copyright Laurence Emms 2018
--Module for representing a narrative graph in a text adventure

module NarrativeGraph (SceneIndex,
                       Flags(..),
                       Inventory(..),
                       ObjectChanges(..),
                       FlagChanges(..),
                       NarrativeCondition(..),
                       Interaction(..),
                       SceneDescription(..),
                       Scene(..),
                       NarrativeGraph(..),
                       makeNarrativeGraph,
                       evaluateCondition,
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
                          NotCondition NarrativeCondition |
                          OrCondition NarrativeCondition NarrativeCondition |
                          AndCondition NarrativeCondition NarrativeCondition deriving (Show, Eq)

data Interaction = Interaction {sentences :: [Sentence],
                                interactionDescription :: String,
                                preconditions :: [(NarrativeCondition, String)], --Conditions required to perform interaction and strings to print if they aren't met
                                addedObjects :: ObjectChanges, --Objects to add to inventory
                                removedObjects :: ObjectChanges, --Objects to remove from inventory
                                setFlags :: FlagChanges, --Flags to set
                                removeFlags :: FlagChanges, --Flags to remove
                                nextScene :: Maybe SceneIndex} deriving (Show, Eq)

data SceneDescription = SceneDescription {description :: String, --This description is always printed
                                          conditionalDescriptions :: [(NarrativeCondition, String)]} deriving (Show, Eq)

data Scene = Scene {sceneDescription :: SceneDescription,
                    interactions :: [Interaction]} deriving (Show, Eq)

--By definition the first node in the narrative graph is the start
data NarrativeGraph = NarrativeGraph {nodes :: Array SceneIndex Scene,
                                      endScenes :: [SceneIndex]} deriving (Show, Eq)

--Takes a list of scenes and returns a starting index and a NarrativeGraph
makeNarrativeGraph :: [Scene] -> [SceneIndex] -> NarrativeGraph
makeNarrativeGraph scenes scenesEndScenes
    = NarrativeGraph {nodes = array (0, length scenes) (zip [0..length scenes] scenes),
                      endScenes = scenesEndScenes}

evaluateCondition :: NarrativeCondition -> Inventory -> Flags -> Bool
evaluateCondition (FlagSet flag) _ (Flags flags) = flag `elem` flags
evaluateCondition (InInventory object) (Inventory inventory) _ = object `elem` inventory
evaluateCondition (NotCondition condition) inventory flags = not (evaluateCondition condition inventory flags)
evaluateCondition (AndCondition condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) && (evaluateCondition condition1 inventory flags)
evaluateCondition (OrCondition condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) || (evaluateCondition condition1 inventory flags)

printConditionalDescriptions :: Inventory -> Flags -> [(NarrativeCondition, String)] -> IO ()
printConditionalDescriptions _ _ [] = return ()
printConditionalDescriptions inventory flags ((condition, description) : remainingDescriptions)
    | evaluateCondition condition inventory flags = putStr description >>
                                                    printConditionalDescriptions inventory flags remainingDescriptions
    | otherwise = printConditionalDescriptions inventory flags remainingDescriptions

printSceneDescription :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> IO ()
printSceneDescription (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) sceneIndex inventory flags
    = putStr thisSceneDescription >> printConditionalDescriptions inventory flags thisConditionalDescriptions >> putStr "\n" >> hFlush stdout
        where Scene {sceneDescription = (SceneDescription {description = thisSceneDescription,
                                                           conditionalDescriptions = thisConditionalDescriptions}),
                     interactions = _} = graphNodes ! sceneIndex

preconditionsMet :: [(NarrativeCondition, String)] -> Inventory -> Flags -> Maybe String
preconditionsMet [] _ _ = Nothing
preconditionsMet ((condition, failString) : remainingConditions) inventory flags
    | not (evaluateCondition condition inventory flags) = Just failString
    | otherwise = preconditionsMet remainingConditions inventory flags

updateFlags :: Flags -> FlagChanges -> FlagChanges -> Flags
updateFlags (Flags flags) (FlagChanges setFlags) (FlagChanges removedFlags) = Flags (setFlags ++ (filter (\x -> not (x `elem` removedFlags)) flags))

updateInventory :: Inventory -> ObjectChanges -> ObjectChanges -> Inventory
updateInventory (Inventory inventory) (ObjectChanges addedObjects) (ObjectChanges removedObjects) = Inventory (addedObjects ++ (filter (\x -> not (x `elem` removedObjects)) inventory))

--Perform the interaction and return a tuple of (new scene index, new inventory, new flags)
doInteraction :: SceneIndex -> [SceneIndex] -> Inventory -> Flags -> Maybe Interaction -> IO (Maybe (SceneIndex, Inventory, Flags))
doInteraction _ _ _ _ Nothing = return Nothing
doInteraction currentScene
              endScenes
              inventory
              flags
              (Just interaction@(Interaction {sentences = _,
                                              interactionDescription = thisInteractionDescription,
                                              preconditions = thisPreconditions,
                                              addedObjects = thisAddedObjects,
                                              removedObjects = thisRemovedObjects,
                                              setFlags = thisSetFlags,
                                              removeFlags = thisRemoveFlags,
                                              nextScene = thisNextScene}))
    = case thisPreconditionsMet of
          Nothing -> case thisNextScene of
                         Nothing -> putStrLn thisInteractionDescription >> return (Just (currentScene, inventory, flags))
                         Just justNextScene -> if justNextScene `elem` endScenes
                                               then putStrLn thisInteractionDescription >> return Nothing
                                               else putStrLn thisInteractionDescription >> return (Just (justNextScene, updateInventory inventory thisAddedObjects thisRemovedObjects, updateFlags flags thisSetFlags thisRemoveFlags))
          Just failString -> putStrLn failString >> return (Just (currentScene, inventory, flags))
          where thisPreconditionsMet = preconditionsMet thisPreconditions inventory flags

--Find an interaction in the interactions list which matches the head of the sentences list
findSentenceInteraction :: [Interaction] -> [Sentence] -> [Sentence] -> Maybe Interaction
findSentenceInteraction [] _ _ = Nothing --You can't match no interactions
findSentenceInteraction (interaction : remainingInteractions) [] allSentences = findInteraction remainingInteractions allSentences --None of the sentences matched this enteraction, try with remaining interactions
findSentenceInteraction allInteractions@(interaction@(Interaction {sentences = thisSentences,
                                                                   interactionDescription = _,
                                                                   preconditions = _,
                                                                   addedObjects = _,
                                                                   removedObjects = _,
                                                                   setFlags = _,
                                                                   removeFlags = _,
                                                                   nextScene = _}) : remainingInteractions) (sentence : remainingSentences) allSentences
    | sentence `elem` thisSentences = Just interaction --This interaction matches this sentence
    | otherwise = findSentenceInteraction allInteractions remainingSentences allSentences --This interaction doesn't match this sentence, try all remaining sentences

findInteraction :: [Interaction] -> [Sentence] -> Maybe Interaction
findInteraction [] _ = Nothing --You can't match no interactions
findInteraction _ [] = Nothing --You can't match no sentences
findInteraction interactions sentences = findSentenceInteraction interactions sentences sentences

processInteraction :: Scene -> SceneIndex -> [SceneIndex] -> Inventory -> Flags -> [Sentence] -> IO (Maybe (SceneIndex, Inventory, Flags))
processInteraction (Scene {sceneDescription = _,
                           interactions = thisSceneInteractions})
                   currentScene
                   endScenes
                   inventory
                   flags
                   sentences
    = doInteraction currentScene endScenes inventory flags interaction
        where interaction = findInteraction thisSceneInteractions sentences

--Perform an interaction with the current scene
--Takes the narrative graph, current scene index, inventory, and sentence as input
--Evaluates to Maybe of the next scene index and inventory state
performInteraction :: NarrativeGraph -> SceneIndex -> Inventory -> Flags -> [Sentence] -> IO (Maybe (NarrativeGraph, SceneIndex, Inventory, Flags))
performInteraction _ _ _ _ [] = return Nothing
performInteraction narrativeGraph@(NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) sceneIndex inventory flags sentences
    = newSceneTuple >>= \thisTuple -> case thisTuple of
                                          Nothing -> return Nothing
                                          (Just (newSceneIndex, newSceneInventory, newSceneFlags)) -> hFlush stdout >> return (Just (narrativeGraph, newSceneIndex, newSceneInventory, newSceneFlags))
        where currentScene = graphNodes ! sceneIndex
              newSceneTuple = processInteraction currentScene sceneIndex graphEndScenes inventory flags sentences
