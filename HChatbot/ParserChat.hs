module HChatbot.ParserChat where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad.Identity

import HChatbot.Rule
import HChatbot.Category

type ParsecC a b = ParsecT String a Identity b

parseWithRule :: Rule -> ParsecC s String
parseWithRule r = 
        foldl (<|>) (fail "rule doesn't match") 
              (map (\s -> string s >>
                          return (output r))
                   (input r))
              
parseWithRules :: [Rule] -> ParsecC s String
parseWithRules rs =
    foldl (<|>) (fail "neither rule matching") 
              (map parseWithRule rs)
              
parseWithCategory :: [Category] -> ParsecC s String
parseWithCategory cs =
    foldl (<|>) (fail "no matching")
              (map (parseWithRules . rules) cs)

parseInChat :: [Category] -> String -> Either ParseError String
parseInChat cs = parse (parseWithCategory cs) ""
    



