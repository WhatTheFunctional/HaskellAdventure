--TextReflow.hs
--Copyright Laurence Emms 2018
--Module to reflow text to a specific column limit

module TextReflow (splitInput,
                   reflowWords,
                   reflowPutStr,
                   reflowPutStrs) where

import qualified Data.List

splitInput :: [Char] -> [Char] -> String -> [String] -> [String]
splitInput _ [] [] allWords = [] --No characters remaining and no current word
splitInput _ [] currentWord allWords = (reverse currentWord) : allWords --No characters remaining and a current word exists
splitInput charsToSplit (c : cs) [] allWords
    | c `elem` charsToSplit = splitInput charsToSplit cs [] allWords --Splitting character detected and no current word exists
    | otherwise = splitInput charsToSplit cs [c] allWords --A non-splitting character was detected and no current word exists
splitInput charsToSplit (c : cs) currentWord allWords
    | c `elem` charsToSplit = (reverse currentWord) : splitInput charsToSplit cs [] allWords --Splitting character detected and a current word exists
    | otherwise = splitInput charsToSplit cs (c : currentWord) allWords --A non-splitting character was detected

splitLine :: Int -> Int -> [String] -> [String] -> ([String], [String])
splitLine c columnWidth wordsBeforeSplit [] = (wordsBeforeSplit, []) --No words remaining
splitLine c columnWidth wordsBeforeSplit (word : wordsAfterSplit)
    | c + (length word) + 1 < columnWidth = splitLine (c + (length word) + 1) columnWidth (word : wordsBeforeSplit) wordsAfterSplit
    | otherwise = (wordsBeforeSplit, (word : wordsAfterSplit)) --Exceeded length

reflowWords :: Int -> [String] -> IO ()
reflowWords _ [] = return () --No words to print
reflowWords columnWidth words
    = putStr (Data.List.intercalate " " (reverse wordsBeforeSplit)) >>
      putStr "\n" >>
      reflowWords columnWidth wordsAfterSplit
        where (wordsBeforeSplit, wordsAfterSplit) = splitLine 0 columnWidth [] words

reflowLines :: [Char] -> Int -> [String] -> IO ()
reflowLines charsToSplit columnWidth [] = return () --No lines to print
reflowLines charsToSplit columnWidth (line : lines)
    = reflowWords columnWidth (splitInput charsToSplit line [] []) >>
      reflowLines charsToSplit columnWidth lines

reflowPutStr :: [Char] -> Int -> String -> IO ()
reflowPutStr charsToSplit columnWidth line
    = reflowLines charsToSplit columnWidth lines
        where lines = splitInput ['\n'] line [] []

reflowPutStrs :: [Char] -> Int -> [String] -> IO ()
reflowPutStrs charsToSplit columnWidth lines = reflowPutStr charsToSplit columnWidth (Data.List.intercalate " " lines)
