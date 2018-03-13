--NarrativeGraph.hs
--Copyright Laurence Emms 2018
--Module for representing a narrative graph in a text adventure

module NarrativeGraph (SceneIndex,
                       Flags(..),
                       Inventory(..),
                       StateChange(..),
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
                       printInvalidInteractions,
                       performInteraction) where

import System.IO
import Data.Array
import qualified Data.List

import TextReflow
import NaturalLanguageLexer
import NaturalLanguageParser

type SceneIndex = Int
data Flags = Flags [String] deriving (Show, Eq)
data Inventory = Inventory [String] deriving (Show, Eq)
data StateChange = AddToInventory String |
                   RemoveFromInventory String |
                   SetFlag String |
                   RemoveFlag String |
                   SceneChange SceneIndex deriving (Show, Eq)

data NarrativeCondition = InInventory String | --Inventory has an item
                          FlagSet String | --Flag is set
                          CTrue | --Always true
                          CFalse | --Always false
                          CNot NarrativeCondition |
                          COr NarrativeCondition NarrativeCondition |
                          CAnd NarrativeCondition NarrativeCondition deriving (Show, Eq)

data ConditionalDescription = ConditionalDescription [(NarrativeCondition, String)] deriving (Show, Eq)

data ConditionalAction = ConditionalAction {condition :: NarrativeCondition, --Condition under which action occurs
                                            conditionalDescription :: ConditionalDescription, --Description of action
                                            stateChanges :: [StateChange]} deriving (Show, Eq) --State changes to make

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
evaluateCondition CTrue _ _ = True
evaluateCondition CFalse _ _ = False
evaluateCondition (FlagSet flag) _ (Flags flags) = flag `elem` flags
evaluateCondition (InInventory object) (Inventory inventory) _ = object `elem` inventory
evaluateCondition (CNot condition) inventory flags = not (evaluateCondition condition inventory flags)
evaluateCondition (COr condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) || (evaluateCondition condition1 inventory flags)
evaluateCondition (CAnd condition0 condition1) inventory flags = (evaluateCondition condition0 inventory flags) && (evaluateCondition condition1 inventory flags)

--Print a conditional description by evaluating which conditions are true, concatenating the description, and printing it with reflowPutStrs
printConditionalDescription :: [Char] -> Int -> Inventory -> Flags -> ConditionalDescription -> [String] -> IO ()
printConditionalDescription charsToSplit columnWidth
                            inventory flags (ConditionalDescription []) linesToPrint
    = reflowPutStrs charsToSplit columnWidth (reverse linesToPrint) >> putStr "\n" >> return () --No more sub-descriptions to print
printConditionalDescription charsToSplit columnWidth
                            inventory flags (ConditionalDescription ((condition, subDescription) : remainingDescriptions)) linesToPrint
    | evaluateCondition condition inventory flags
        = printConditionalDescription charsToSplit columnWidth
                                      inventory flags (ConditionalDescription remainingDescriptions) ((subDescription ++ " ") : linesToPrint) --Condition is true, add sub-description to print
    | otherwise
        = printConditionalDescription charsToSplit columnWidth inventory flags (ConditionalDescription remainingDescriptions) linesToPrint

printSceneDescription :: [Char] -> Int -> NarrativeGraph -> SceneIndex -> Inventory -> Flags -> IO ()
printSceneDescription charsToSplit columnWidth (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) sceneIndex inventory flags
    = printConditionalDescription charsToSplit columnWidth inventory flags thisSceneDescription [] >> hFlush stdout
        where Scene {sceneDescription = thisSceneDescription,
                     interactions = _} = graphNodes ! sceneIndex

updateFlags :: Flags -> [StateChange] -> Flags
updateFlags (Flags flags) [] = Flags flags
updateFlags (Flags flags) ((RemoveFlag flag) : remainingChanges) = updateFlags (Flags (filter (\x -> x /= flag) flags)) remainingChanges
updateFlags (Flags flags) ((SetFlag flag) : remainingChanges) = updateFlags (Flags (flag : flags)) remainingChanges
updateFlags (Flags flags) (_ : remainingChanges) = updateFlags (Flags flags) remainingChanges

updateInventory :: Inventory -> [StateChange] -> Inventory
updateInventory (Inventory inventory) [] = Inventory inventory
updateInventory (Inventory inventory) ((RemoveFromInventory object) : remainingChanges) = updateInventory (Inventory (filter (\x -> x /= object) inventory)) remainingChanges
updateInventory (Inventory inventory) ((AddToInventory object) : remainingChanges) = updateInventory (Inventory (object : inventory)) remainingChanges
updateInventory (Inventory inventory) (_ : remainingChanges) = updateInventory (Inventory inventory) remainingChanges

--Scene transition takes the next scene index, the end scene index list, the current scene index, the inventory and flags, and a conditional action
--Scene transition evaluates to the next state of the game
sceneTransition :: Maybe StateChange -> [SceneIndex] -> SceneIndex -> Inventory -> Flags -> ConditionalAction -> IO (Maybe (SceneIndex, Inventory, Flags))
sceneTransition Nothing _ currentScene inventory flags
                conditionalAction@(ConditionalAction {stateChanges = thisStateChanges})
    = return (Just (currentScene,
                    updateInventory inventory thisStateChanges,
                    updateFlags flags thisStateChanges)) --If there is no scene transition, return to the current scene with updated inventory and flags
sceneTransition (Just (SceneChange nextScene)) endScenes  _ inventory flags
                conditionalAction@(ConditionalAction {stateChanges = thisStateChanges})
    = if nextScene `elem` endScenes
      then return Nothing --This is an end state for the game
      else return (Just (nextScene,
                         updateInventory inventory thisStateChanges,
                         updateFlags flags thisStateChanges)) --Transition to the next scene with updated inventory and flags

--Update game state takes an interaction description, fail string, next scene index, end scene index, current scene index, inventory, and flags
--Update game state Scene transition evaluates to the next state of the game
updateGameState :: [Char] -> Int -> [SceneIndex] -> SceneIndex -> Inventory -> Flags -> ConditionalAction -> IO (Maybe (SceneIndex, Inventory, Flags))
updateGameState charsToSplit columnWidth endScenes currentScene inventory flags conditionalAction@(ConditionalAction {conditionalDescription = thisConditionalDescription,
                                                                                             stateChanges = thisStateChanges})
    = printConditionalDescription charsToSplit columnWidth inventory flags thisConditionalDescription [] >>
      sceneTransition (Data.List.find (\x -> case x of
                                             (SceneChange _) -> True
                                             otherwise -> False) thisStateChanges)
                      endScenes
                      currentScene
                      inventory
                      flags
                      conditionalAction --This conditional action passed all of the preconditions, check whether we need to transition to a new scene

--Perform the interaction and return a tuple of (new scene index, new inventory, new flags)
performConditionalActions :: [Char] -> Int -> SceneIndex -> [SceneIndex] -> Inventory -> Flags -> Maybe Interaction -> Maybe Interaction -> IO (Maybe (SceneIndex, Inventory, Flags))
performConditionalActions _ _ currentScene _ inventory flags Nothing Nothing
    = putStrLn "That does nothing.\n" >>
      hFlush stdout >>
      return (Just (currentScene, inventory, flags)) --If there are no valid interactions actions but the sentence was valid, just return to the current state
performConditionalActions charsToSplit
                          columnWidth
                          currentScene
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = _,
                                              conditionalActions = []}))
                          defaultSceneInteractions --There are no remaining conditional actions for the current scene
    = performConditionalActions charsToSplit columnWidth currentScene endScenes inventory flags Nothing defaultSceneInteractions --All current scene conditional actions were exhausted, try default scene interactions
performConditionalActions charsToSplit
                          columnWidth
                          currentScene
                          endScenes
                          inventory
                          flags
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
                          defaultSceneInteractions -- Ignore default scene interactions if there are still current scene interactions
    | evaluateCondition thisCondition inventory flags = updateGameState charsToSplit columnWidth endScenes currentScene inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions charsToSplit columnWidth currentScene endScenes inventory flags
                                            (Just (Interaction {sentences = thisSentences,
                                                                conditionalActions = remainingConditionalActions})) defaultSceneInteractions --The condition for the action failed, attempt other actions
performConditionalActions charsToSplit
                          columnWidth
                          currentScene
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = _,
                                              conditionalActions = []}))
    = performConditionalActions charsToSplit columnWidth currentScene endScenes inventory flags Nothing Nothing --All possible conditional actions are exhausted
performConditionalActions charsToSplit
                          columnWidth
                          currentScene
                          endScenes
                          inventory
                          flags
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sentences = thisSentences,
                                              conditionalActions = (conditionalAction@(ConditionalAction {condition = thisCondition}) : remainingConditionalActions)}))
    | evaluateCondition thisCondition inventory flags = updateGameState charsToSplit columnWidth endScenes currentScene inventory flags conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions charsToSplit columnWidth currentScene endScenes inventory flags Nothing
                                            (Just (Interaction {sentences = thisSentences,
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

processInteraction :: [Char] -> Int -> Scene -> Scene -> SceneIndex -> [SceneIndex] -> Inventory -> Flags -> [Sentence] -> IO (Maybe (SceneIndex, Inventory, Flags))
processInteraction charsToSplit
                   columnWidth
                   (Scene {sceneDescription = _,
                           interactions = thisSceneInteractions})
                   (Scene {sceneDescription = _,
                           interactions = defaultSceneInteractions})
                   currentScene
                   endScenes
                   inventory
                   flags
                   sentences
    = performConditionalActions charsToSplit columnWidth currentScene endScenes inventory flags interaction defaultInteraction
        where interaction = findInteraction thisSceneInteractions sentences
              defaultInteraction = findInteraction defaultSceneInteractions sentences

hasInvalidInteractions :: [Interaction] -> Maybe Interaction
hasInvalidInteractions [] = Nothing
hasInvalidInteractions (interaction@(Interaction {sentences = thisSentences}) : remainingInteractions)
    | NullSentence `elem` thisSentences = Just interaction
    | otherwise = hasInvalidInteractions remainingInteractions

printInvalidInteractions :: NarrativeGraph -> Int -> IO ()
printInvalidInteractions narrativeGraph@(NarrativeGraph {nodes = graphNodes}) sceneIndex
    = case hasInvalidInteractions sceneInteractions of
          Nothing -> return ()
          Just interaction@(Interaction {sentences = thisSentences}) -> putStrLn ("Invalid interaction: " ++  (show interaction))
        where (Scene {sceneDescription = _,
                      interactions = sceneInteractions}) = graphNodes ! sceneIndex

--Perform an interaction with the current scene
--Takes the narrative graph, current scene index, inventory, and sentence as input
--Evaluates to Maybe of the next scene index and inventory state
performInteraction :: [Char] -> Int -> NarrativeGraph -> SceneIndex -> Inventory -> Flags -> [Sentence] -> IO (Maybe (NarrativeGraph, SceneIndex, Inventory, Flags))
performInteraction _ _ narrativeGraph sceneIndex inventory flags []
    = putStrLn "Please enter a command." >>
      hFlush stdout >>
      return (Just (narrativeGraph, sceneIndex, inventory, flags)) --If there are no valid sentences, just continue.
performInteraction charsToSplit columnWidth narrativeGraph@(NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes, graphDefaultScene = thisDefaultScene}) sceneIndex inventory flags sentences
    = hFlush stdout >>
      fmap (\maybeNewSceneTuple -> maybeNewSceneTuple >>=
                                   (\(newSceneIndex, newSceneInventory, newSceneFlags) -> Just (narrativeGraph, newSceneIndex, newSceneInventory, newSceneFlags)))
      ioMaybeNewSceneTuple
        where currentScene = graphNodes ! sceneIndex
              ioMaybeNewSceneTuple = processInteraction charsToSplit columnWidth currentScene thisDefaultScene sceneIndex graphEndScenes inventory flags sentences
