module HChatbot.Rule where

import qualified Data.Text as T

import HChatbot.Normalization

type RuleIn = String
 
type RuleOut = String

-- Regla de chat. 
-- Invariante: input debe ser no vacia
data Rule = Rule { input :: [RuleIn]
                 , output :: RuleOut
                 }

rulename :: Rule -> RuleIn
rulename = head . input



createRule :: String -> String -> String -> Rule
createRule inp otherInp out =
    let opts = if otherInp==""
                then []
                else map normalize $ splitInput otherInp
    in
        Rule { input = (normalize inp):opts
             , output = out
        }
    
    where splitInput st = map (T.unpack) $ T.split (=='\n') (T.pack st)
                 
                 
instance Show Rule where
    show (Rule ins output) =
        "Rule: input = " ++ (show ins) ++ "| output = " ++ (show output)
