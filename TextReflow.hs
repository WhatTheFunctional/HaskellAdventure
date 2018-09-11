--TextReflow.hs
--Copyright Laurence Emms 2018
--Module to reflow text to a specific column limit

module TextReflow (joinStrings,
                   reflowLine,
                   reflowLines,
                   reflowPutStr,
                   reflowPutStrs,
                   reflowString,
                   reflowStrings) where

import qualified Data.List
import qualified Data.List.Split

joinStrings :: String -> [String] -> String
joinStrings joinString [] = ""
joinStrings joinString (string : strings) = string ++ joinString ++ (joinStrings joinString strings)

--Reflow a single line split into words by delimiters
reflowLine :: Int -> Int -> [String] -> [String] -> [String]
reflowLine _ _ [] result = reverse result
reflowLine c columnWidth (word : words) []
    | c + (length word) < columnWidth = reflowLine (c + (length word)) columnWidth words [word]
    | otherwise = reflowLine 0 columnWidth words [word]
reflowLine c columnWidth (word : words) (line : result)
    | c + (length word) < columnWidth = reflowLine (c + (length word)) columnWidth words ((line ++ word) : result)
    | otherwise = reflowLine 0 columnWidth words (word : line : result)

--Reflow a set of lines
reflowLines :: [Char] -> Int -> [String] -> [String]
reflowLines _ _ [] = [] --No lines to reflow
reflowLines delimiters columnWidth (line : lines)
    = (reflowLine 0 columnWidth (Data.List.Split.split (Data.List.Split.keepDelimsR $ Data.List.Split.oneOf delimiters) line) []) ++ (reflowLines delimiters columnWidth lines)

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
        where lines = Data.List.Split.split (Data.List.Split.keepDelimsR $ Data.List.Split.oneOf "\n") line --Split into lines keeping existing newlines

reflowPutStrs :: [Char] -> Int -> [String] -> IO ()
reflowPutStrs delimiters columnWidth lines = reflowPutStr delimiters columnWidth (concat lines)

reflowString :: [Char] -> Int -> String -> [String]
reflowString delimiters columnWidth string
    = reflowLines delimiters columnWidth lines
        where lines = Data.List.Split.split (Data.List.Split.keepDelimsR $ Data.List.Split.oneOf "\n") string

reflowStrings :: [Char] -> Int -> [String] -> [String]
reflowStrings delimiters columnWidth strings = reflowString delimiters columnWidth (concat strings)
