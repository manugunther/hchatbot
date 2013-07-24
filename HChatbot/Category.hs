module HChatbot.Category where

import qualified Data.Map as M
import Data.Maybe

import HChatbot.Rule

type CategName = String

data Category = Category { name :: CategName
                         , rules :: [Rule]
                         }

addRule :: Category -> Rule -> Category
addRule c r =
    Category { name = name c
             , rules = r:(rules c)
    }

