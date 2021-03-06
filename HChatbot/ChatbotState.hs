module HChatbot.ChatbotState where

import qualified Data.Map as M
import Data.Maybe

import HChatbot.Category

-- Estado del chatbot.
-- Invariante: actualCateg pertenece al mapa categs.
data ChatbotState = ChatbotState {
                        categs      :: M.Map CategName Category
                      , actualCateg :: CategName
}

-- Reemplaza una categoría dentro del estado
replaceCateg :: ChatbotState -> CategName -> Category -> ChatbotState
replaceCateg chst cname c =
    chst { categs = (M.update (const $ Just c) cname $ categs chst) }
        

-- Agrega una categoría al estado
addCateg :: ChatbotState -> Category -> ChatbotState
addCateg chst c =
    chst { categs = M.insert (name c) c (categs chst) }
        
-- Dado un estado de chatbot retorna una lista con todas las categorías
-- donde la primera es la actual.
categList :: ChatbotState -> [Category]
categList (ChatbotState m name) = 
    let cat = fromJust (M.lookup name m) in
        cat:M.elems (M.delete name m)

-- Obtiene la categoria con nombre cname en caso de que exista
getCateg :: ChatbotState -> CategName -> Maybe Category
getCateg chst cname =
    M.lookup cname (categs chst)

updActualCateg :: ChatbotState -> CategName -> ChatbotState
updActualCateg chst cname =
    chst { actualCateg = cname }