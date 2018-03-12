--TextReflow.hs
--Copyright Laurence Emms 2018
--Module to reflow text to a specific column limit

module TextReflow (splitString,
                   joinStrings,
                   reflowPutStr,
                   reflowPutStrs,
                   reflowString,
                   reflowStrings) where

import qualified Data.List

splitString :: [Char] -> [Char] -> String -> [String] -> [String]
splitString _ [] [] allWords = [] --No characters remaining and no current word
splitString _ [] currentWord allWords = (reverse currentWord) : allWords --No characters remaining and a current word exists
splitString charsToSplit (c : cs) [] allWords
    | c `elem` charsToSplit = splitString charsToSplit cs [] allWords --Splitting character detected and no current word exists
    | otherwise = splitString charsToSplit cs [c] allWords --A non-splitting character was detected and no current word exists
splitString charsToSplit (c : cs) currentWord allWords
    | c `elem` charsToSplit = (reverse currentWord) : splitString charsToSplit cs [] allWords --Splitting character detected and a current word exists
    | otherwise = splitString charsToSplit cs (c : currentWord) allWords --A non-splitting character was detected

joinStrings :: String -> [String] -> String
joinStrings joinString [] = ""
joinStrings joinString (string : strings) = string ++ joinString ++ (joinStrings joinString strings)

splitLine :: Int -> Int -> [String] -> [String] -> ([String], [String])
splitLine c columnWidth wordsBeforeSplit [] = (wordsBeforeSplit, []) --No words remaining
splitLine c columnWidth wordsBeforeSplit (word : wordsAfterSplit)
    | c + (length word) + 1 < columnWidth = splitLine (c + (length word) + 1) columnWidth (word : wordsBeforeSplit) wordsAfterSplit
    | otherwise = (wordsBeforeSplit, (word : wordsAfterSplit)) --Exceeded length

reflowWords :: Int -> [String] -> [String]
reflowWords _ [] = [] --No words to join
reflowWords columnWidth words
    = (Data.List.intercalate " " (reverse wordsBeforeSplit)) : (reflowWords columnWidth wordsAfterSplit)
        where (wordsBeforeSplit, wordsAfterSplit) = splitLine 0 columnWidth [] words

reflowLines :: [Char] -> Int -> [String] -> [String]
reflowLines charsToSplit columnWidth [] = [] --No lines to reflow
reflowLines charsToSplit columnWidth (line : lines)
    = (reflowWords columnWidth (splitString charsToSplit line [] [])) ++ (reflowLines charsToSplit columnWidth lines)

reflowPutStr :: [Char] -> Int -> String -> IO ()
reflowPutStr charsToSplit columnWidth line
    = mapM_ putStrLn (reflowLines charsToSplit columnWidth lines)
        where lines = splitString ['\n'] line [] []

reflowPutStrs :: [Char] -> Int -> [String] -> IO ()
reflowPutStrs charsToSplit columnWidth lines = reflowPutStr charsToSplit columnWidth (Data.List.intercalate " " lines)

reflowString :: [Char] -> Int -> String -> [String]
reflowString charsToSplit columnWidth string
    = reflowLines charsToSplit columnWidth lines
        where lines = splitString ['\n'] string [] []

reflowStrings :: [Char] -> Int -> [String] -> [String]
reflowStrings charsToSplit columnWidth strings = reflowString charsToSplit columnWidth (Data.List.intercalate " " strings)
