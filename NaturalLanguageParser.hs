--NaturalLanguageParser.hs
--Copyright Laurence Emms 2018
--Module for a general Natural Language parser

module NaturalLanguageParser (ParseTree(..)) where

import NaturalLanguageLexer

data ParseTree = ParseTreeLeaf Token |
                 ParseTreeNode ParseTree ParseTree
