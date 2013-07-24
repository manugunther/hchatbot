module HChatbot.Rule where


type RuleIn = String
 
type RuleOut = String

data Rule = Rule { input :: [RuleIn]
                 , output :: RuleOut
                 }

instance Show Rule where
    show (Rule ins output) =
        "Rule: input = " ++ (show ins) ++ "| output = " ++ (show output)
