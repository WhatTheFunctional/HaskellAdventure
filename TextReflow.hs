--TextReflow.hs
--Copyright Laurence Emms 2018
--Module to reflow text to a specific column limit

module TextReflow (joinStrings,
                   splitString,
                   splitStringWithDelimiters,
                   reflowPutStr,
                   reflowPutStrs,
                   reflowString,
                   reflowStrings) where

import qualified Data.List


joinStrings :: String -> [String] -> String
joinStrings joinString [] = ""
joinStrings joinString (string : strings) = string ++ joinString ++ (joinStrings joinString strings)

--Split string by a set of delimiters
splitString :: [Char] -> [Char] -> String -> [String]
splitString _ [] [] = [] --No characters remaining and no current word
splitString _ [] currentWord = [reverse currentWord] --No characters remaining and a current word exists
splitString delimiters (c : cs) []
    | c `elem` delimiters = splitString delimiters cs [] --Splitting character detected and no current word exists
    | otherwise = splitString delimiters cs [c] --A non-splitting character was detected and no current word exists
splitString delimiters (c : cs) currentWord
    | c `elem` delimiters = (reverse currentWord) : splitString delimiters cs [] --Splitting character detected and a current word exists
    | otherwise = splitString delimiters cs (c : currentWord) --A non-splitting character was detected

--Split string by a set of delimiters and keep the delimiters in the list
--Only split when the previous character (head of current word) is a delimiter and the next character (head of string) is not a delimiter or
--when the previous character is not a delimiter and the next character is a delimiter. This is equivalent to (delim a) xor (delim b) where (delim x)
--is true if x is a delimiter
splitStringWithDelimiters :: [Char] -> [Char] -> String -> [String]
splitStringWithDelimiters _ [] [] = [] --No characters remaining and no current word
splitStringWithDelimiters _ [] currentWord = [reverse currentWord] --No characters remaining and a current word exists
splitStringWithDelimiters delimiters (c : cs) [] --Current word is empty
    = splitStringWithDelimiters delimiters cs [c] --Create new word with the current character
splitStringWithDelimiters delimiters (c : cs) currentWord@(cw : cws)
    | (not (c `elem` delimiters)) && (cw `elem` delimiters) ||
      (c `elem` delimiters) && (not (cw `elem` delimiters)) = (reverse currentWord) : splitStringWithDelimiters delimiters (c : cs) [] --New split detected and a current word exists
    | otherwise = splitStringWithDelimiters delimiters cs (c : currentWord) --A non-splitting character was detected, add it to the current word

--Split a single line after the column width limit
splitLine :: Int -> Int -> [String] -> [String] -> ([String], [String])
splitLine c columnWidth wordsBeforeSplit [] = (wordsBeforeSplit, []) --No words remaining
splitLine c columnWidth wordsBeforeSplit (word : wordsAfterSplit)
    | c + (length word) < columnWidth = splitLine (c + (length word)) columnWidth (word : wordsBeforeSplit) wordsAfterSplit
    | otherwise = (wordsBeforeSplit, (word : wordsAfterSplit)) --Exceeded column width, add a newline

--Reflow a single line
reflowLine :: Int -> [String] -> [String]
reflowLine _ [] = [] --No words to join
reflowLine columnWidth words
    = (concat (reverse wordsBeforeSplit)) : (reflowLine columnWidth wordsAfterSplit)
        where (wordsBeforeSplit, wordsAfterSplit) = splitLine 0 columnWidth [] words

--Reflow a set of lines
reflowLines :: [Char] -> Int -> [String] -> [String]
reflowLines delimiters columnWidth [] = [] --No lines to reflow
reflowLines delimiters columnWidth (line : lines)
    = (reflowLine columnWidth (splitStringWithDelimiters delimiters line [])) ++ (reflowLines delimiters columnWidth lines)

--Insert newlines into the reflowed lines, ignoring all lines which are followed by a delimiter line
intercalateNewlines :: [String] -> String
intercalateNewlines [] = ""
intercalateNewlines (line : []) = line
intercalateNewlines (line1 : line2 : linesRemaining)
    | ((length line2) > 0) && ((head line2) == '\n') = line1 ++ line2 ++ (intercalateNewlines linesRemaining) --If the next line is a delimiter, just concatenate the lines
    | otherwise = line1 ++ "\n" ++ intercalateNewlines (line2 : linesRemaining) --If the next line is not a delimiter, add one

reflowPutStr :: [Char] -> Int -> String -> IO ()
reflowPutStr delimiters columnWidth line
    = putStr (intercalateNewlines (reflowLines delimiters columnWidth lines))
        where lines = splitStringWithDelimiters ['\n'] line [] --Split into lines keeping existing newlines

reflowPutStrs :: [Char] -> Int -> [String] -> IO ()
reflowPutStrs delimiters columnWidth lines = reflowPutStr delimiters columnWidth (concat lines)

reflowString :: [Char] -> Int -> String -> [String]
reflowString delimiters columnWidth string
    = reflowLines delimiters columnWidth lines
        where lines = splitStringWithDelimiters ['\n'] string []

reflowStrings :: [Char] -> Int -> [String] -> [String]
reflowStrings delimiters columnWidth strings = reflowString delimiters columnWidth (concat strings)
