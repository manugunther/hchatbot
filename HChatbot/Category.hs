module HChatbot.Category where

import qualified Data.Map as M
import Data.Maybe

import HChatbot.Rule

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
    let rid = newid (rules c) in
    Category { name = name c
             , rules = M.insert rid r (rules c)
    }

    where newid m =
            let ks = M.keys m in
                if ks == []
                   then 1
                   else succ $ maximum ks
         