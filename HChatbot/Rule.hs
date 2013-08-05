module HChatbot.Rule where

import qualified Data.Text as T

import HChatbot.Normalization


-- Una regla puede contener texto para matchear literalmente,
-- variables, o comodin.
data InputRule = Literal String | IVariable String | WC WildCard

instance Show InputRule where
    show (Literal s) = s
    show (IVariable s) = "[" ++ s ++ "]"
    show (WC wc) = show wc

-- El output de una regla puede ser texto, variable o condicional.
data OutputRule = TextO String | OVariable String | 
                  Condition FormRule OutputRule OutputRule

instance Show OutputRule where
    show (TextO s) = s
    show (OVariable s) = "[" ++ s ++ "]"
    show _ = "not implemented"

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
data Rule = Rule { input  :: [RuleIn]
                 , output :: RuleOut
                 , rname  :: String
                 }

instance Show Rule where 
    show = rname


rulename :: Rule -> String
rulename = rname

choicesInStr :: Rule -> String
choicesInStr r =
    let rin = input r in
        toString (tail rin)
        
    where toString ls = 
            case ls of
                 []     -> ""
                 (c:cs) -> (showRuleInOut c) ++ 
                    (foldl (\acum rin -> acum ++ "\n" ++ (showRuleInOut rin)) "" cs)


showRuleInOut :: Show a => [a] -> String
showRuleInOut ls = 
    case ls of
         []     -> ""
         (c:cs) -> (show c) ++ (foldl (\acum ir -> acum ++ " " ++ (show ir)) "" cs)
    

normalizeInput :: InputRule -> InputRule
normalizeInput (Literal s) = Literal (normalize s)
normalizeInput ir = ir

