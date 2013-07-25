module HChatbot.Rule where

import qualified Data.Text as T

import HChatbot.Normalization


-- Una regla puede contener texto para matchear literalmente,
-- variables, o comodin.
data InputRule = Literal String | IVariable String | WC WildCard

instance Show InputRule where
    show (Literal s) = show s
    show (IVariable s) = "[" ++ (show s) ++ "]"
    show (WC wc) = show wc

-- El output de una regla puede ser texto, variable o condicional.
data OutputRule = TextO String | OVariable String | 
                  Condition FormRule OutputRule OutputRule

-- Completar en algún momento
type FormRule = String
                  
data WildCard = WildCardProd | WildCardPlus

instance Show WildCard where
    show WildCardPlus = "+"
    show WildCardProd = "*"
                  
type RuleIn = [InputRule]
 
type RuleOut = [OutputRule]

-- Regla de chat. 
-- Invariante: input debe ser no vacia
data Rule = Rule { input :: [RuleIn]
                 , output :: RuleOut
                 }

instance Show Rule where 
    show (Rule rin rout) = (concat . (map show) . head) rin


rulename :: Rule -> String
rulename = show


normalizeInput :: InputRule -> InputRule
normalizeInput (Literal s) = Literal (normalize s)
normalizeInput ir = ir

