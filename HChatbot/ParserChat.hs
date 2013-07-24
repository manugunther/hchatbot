module HChatbot.ParserChat where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad.Identity

import HChatbot.Rule

type ParsecC a b = ParsecT String a Identity b

parseWithRule :: Rule -> ParsecC s String
parseWithRule r = 
        foldl (<|>) (fail "rule not match") 
              (map (\s -> string s >>
                          return (output r))
                   (input r))
              
parseWithRules :: [Rule] -> ParsecC s String
parseWithRules rs =
    foldl (<|>) (fail "neither rule match") 
              (map parseWithRule rs)

parseInChat :: [Rule] -> String -> Either ParseError String
parseInChat rs = parse (parseWithRules rs) ""
    











