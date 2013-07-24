{-# Language DoAndIfThenElse, TemplateHaskell #-}
-- | Configuración de la lista de declaraciones del panel izquierdo.
module HChatbot.GUI.RuleList where

import HChatbot.GUI.GState

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import Data.Map (empty,elems)
import Data.Tree
import Data.Tuple (swap)

import qualified Data.Foldable as F (mapM_) 

import Control.Monad.Trans.RWS
import Control.Monad
import Control.Applicative

import Lens.Family
import Lens.Family.TH

import HChatbot.Rule

configRuleList :: GuiMonad ()
configRuleList = ask >>= \cnt -> do
        let boxRList = cnt ^. (hRuleWidget . ruleBox)
            rlist    = cnt ^. (hRuleWidget . ruleList)
            rtv      = cnt ^. (hRuleWidget . ruleTv)
        
        io $ setupRuleList boxRList rlist rtv
            
        eventsRuleListConfig rtv
        
        return ()
    where
        insert :: CellRendererTextClass cell => Rule -> [AttrOp cell]
        insert rule = [ cellText := head $ input rule ]
        setupRuleList :: VBox -> ListStore Rule -> TreeView -> IO ()
        setupRuleList boxRList ruleList ruleTv = do
            tvcolmn  <- treeViewGetColumn ruleTv 0
            F.mapM_ (treeViewRemoveColumn ruleTv) tvcolmn
            col <- treeViewColumnNew
            treeViewSetHeadersVisible ruleTv False
            treeViewSetModel ruleTv ruleList
            rend <- cellRendererTextNew
            cellLayoutPackStart col rend False
            cellLayoutSetAttributes col rend ruleList insert
            treeViewAppendColumn ruleTv col
            containerAdd boxRList ruleTv
            widgetShowAll boxRList

addRuleToList :: Rule -> GuiMonad ()
addRuleToList rule = ask >>= \cnt -> do
            let rlist = cnt ^. (hRuleWidget . ruleList)
            _ <- io $ listStoreAppend rlist rule
            return ()

eventsRuleListConfig :: TreeView -> GuiMonad ()
eventsRuleListConfig rtv = get >>= \ref -> ask >>= \cnt -> io $ do
            selTv <- treeViewGetSelection rtv
            treeSelectionSetMode selTv SelectionSingle
            treeSelectionUnselectAll selTv
            onSelectionChanged selTv (eval (fillRuleWidget selTv) cnt ref)
            return ()
    where
        eval action cnt ref = evalRWST action cnt ref >> return ()
        fillRuleWidget :: TreeSelection -> GuiMonad ()
        fillRuleWidget selTv = ask >>= \cnt -> io $ do
                let rlist = cnt ^. (hRuleWidget . ruleList)
                
                Just iter <- treeSelectionGetSelected selTv
                
                let i = listStoreIterToIndex iter
                
                rule <- listStoreGetValue rlist i
                
                putStrLn $ show rule
