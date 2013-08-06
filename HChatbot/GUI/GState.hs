{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module HChatbot.GUI.GState where

import Lens.Family
import Lens.Family.TH

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Data.IORef
import Data.Reference
import qualified Data.Map as M

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS

import HChatbot.ChatbotState
import HChatbot.Category
import HChatbot.Rule


import GramLab.Morfette.Utils

-- | Item para mostrar categorias y reglas. Cada item puede ser una categoria o una regla.
data ListItem = ListItem { -- Nombre para mostrar en la lista
                           _nameItem   :: String
                           -- Nombre de la categoria del elemento
                         , _catName   :: String
                           -- Si es una regla ruleId es el id para acceder
                           -- a la regla en el mapa rules de la categoria.
                         , _ruleId  :: Maybe RuleId
                         }
$(mkLenses ''ListItem)


data RuleWidget = RuleWidget { _entryRule   :: Entry
                             , _tvRule      :: TextView
                             , _tvAnswer    :: TextView
                             , _applyButton :: Button
                             , _ruleTStore   :: TreeStore ListItem
                             , _ruleTv      :: TreeView
                             , _ruleBox     :: VBox
                             }
$(mkLenses ''RuleWidget)


data ChatWidget = ChatWidget { _tvChat    :: TextView
                             , _entryChat :: Entry
                             }
$(mkLenses ''ChatWidget)

data ToolsWidget = ToolsWidget { _newCategB :: ToolButton
                               , _newRuleB  :: ToolButton
                               }
$(mkLenses ''ToolsWidget)


data GReader = GReader { _hWindow      :: Window
                       , _hRuleWidget  :: RuleWidget
                       , _hChatWidget  :: ChatWidget
                       , _hToolsWidget :: ToolsWidget
                       , _hMorfette    :: MorfState 
                       }
$(mkLenses ''GReader)

data GState = GState { _chatState :: ChatbotState
                     -- nombre de la categoría seleccionada en la interfaz
                     , _selCateg :: CategName
                     , _selRule  :: Maybe RuleId
                     }
$(mkLenses ''GState)

type GStateRef = IORef GState

-- | Mónada de la interfaz.
type GuiMonad' = RWST GReader () GStateRef
type GuiMonad = GuiMonad' IO

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

-- | Retorna el estado de la mónada de la interfaz.
getGState :: GuiMonad GState
getGState = get >>= readRef

-- | Actualiza el estado de la mónada de la interfaz.
updateGState :: (GState -> GState) -> GuiMonad ()
updateGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r

io = liftIO

initChState :: ChatbotState
initChState =
    ChatbotState (M.insert "Saludos" initCat M.empty)
                 "Saludos"
                 
    where initCat = Category "Saludos" M.empty


initState :: GState
initState =
    GState initChState (actualCateg initChState) Nothing
