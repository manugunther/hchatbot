module HChatbot.ParserRule where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad.Identity
import System.IO.Unsafe(unsafePerformIO)

import HChatbot.Rule

type ParsecR a b = ParsecT String a Identity b


beginVar = '['
endVar = ']'
wildcProd = '*'
wildcPlus = '+'

reservedChars = [beginVar,endVar,wildcPlus,wildcProd]

parseLiteral :: ParsecR s InputRule
parseLiteral = many1 (noneOf reservedChars) >>= return . Literal


parseVar :: ParsecR s String
parseVar = char beginVar >>
           many (noneOf reservedChars) >>= \s ->
           char endVar >> return s

parseVariable :: ParsecR s InputRule
parseVariable = parseVar >>= return .IVariable
                
parseWildCard :: ParsecR s InputRule
parseWildCard = (char wildcProd >> return (WC WildCardProd))
                <|>
                (char wildcPlus >> return (WC WildCardPlus))


parseRuleInput :: ParsecR s [InputRule]
parseRuleInput = many (parseLiteral <|> parseVariable <|> parseWildCard)
                 

parserInput :: String -> Either ParseError [InputRule]
parserInput = parse parseRuleInput ""


parseTextOut :: ParsecR s OutputRule
parseTextOut = many1 (noneOf reservedChars) >>= return . TextO

parseVarOut :: ParsecR s OutputRule
parseVarOut = parseVar >>= return . OVariable

parseCondOut :: ParsecR s OutputRule
parseCondOut = fail "not implemented conditional"

parseRuleOutput :: ParsecR s [OutputRule]
parseRuleOutput = many (parseTextOut <|> parseVarOut <|> parseCondOut)

parserOutput :: String -> Either ParseError [OutputRule]
parserOutput = parse parseRuleOutput ""



