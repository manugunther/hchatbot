module HChatbot.Category where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe

import HChatbot.Rule
import HChatbot.ParserRule

type CategName = String

type RuleId = Int

data Category = Category { name  :: CategName
                         , rules :: M.Map RuleId Rule
                         }
instance Show Category where
    show (Category n rs) = "Categoria " ++ n ++
                           " | Reglas: " ++ (show rs)

addRule :: Category -> Rule -> Category
addRule c r =
    let rid = newid c in
    Category { name = name c
             , rules = M.insert rid r (rules c)
    }

newid categ =
    let ks = M.keys (rules categ) in
        if ks == []
            then 1
            else succ $ maximum ks
         
updateRule :: Category -> RuleId -> Rule -> Category
updateRule c rid r =
    c { rules = M.insert rid r (rules c) }
         
         
createRInput :: String -> RuleIn
createRInput s =
    either (const $ [Literal "error de parseo"])
           (\ir -> map normalizeInput ir)
           (parserInput s)
           
createROutput :: String -> RuleOut
createROutput s =
    either (const $ [TextO "error de parseo"])
           (\r -> r)
           (parserOutput s)
         
createRule :: String -> String -> String -> Rule
createRule inp otherInp out =
    let opts = if otherInp==""
                then []
                else map createRInput $ splitInput otherInp
    in
        Rule { input = (createRInput inp):opts
             , output = createROutput out
             , rname = inp
        }
    
    where splitInput st = map (T.unpack) $ T.split (=='\n') (T.pack st)
          
          
getRule :: Category -> RuleId -> Maybe Rule
getRule c rid =
    M.lookup rid (rules c)
          