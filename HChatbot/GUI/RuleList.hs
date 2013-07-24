{-# Language DoAndIfThenElse, TemplateHaskell #-}
-- | Configuración de la lista de declaraciones del panel izquierdo.
module HChatbot.GUI.RuleList where

import HChatbot.GUI.GState

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import qualified Data.Map as M
import Data.Tree
import Data.Tuple (swap)

import qualified Data.Foldable as F (mapM_) 

import Control.Monad.Trans.RWS
import Control.Monad
import Control.Applicative

import Lens.Family
import Lens.Family.TH

import HChatbot.Rule
import HChatbot.Category
import HChatbot.ChatbotState


ruleToTree :: Category -> (RuleId,Rule) -> Tree ListItem
ruleToTree categ (rid,r) =
    Node (ListItem (rulename r) categ (Just rid))
         []

categoryToTree :: Category -> Tree ListItem
categoryToTree c =
    Node (ListItem (name c) c Nothing)
         (map (ruleToTree c) (M.toList $ rules c))

stateToForest :: ChatbotState -> Forest ListItem
stateToForest cs =
    map categoryToTree (M.elems $ categs cs)

    
stateToTreeStore :: ChatbotState -> IO (TreeStore ListItem)
stateToTreeStore cs = treeStoreNew (stateToForest cs)
    
    
configRuleList :: GuiMonad ()
configRuleList = getGState >>= \st -> ask >>= \cnt -> do
        let boxRList = cnt ^. (hRuleWidget . ruleBox)
            rtv      = cnt ^. (hRuleWidget . ruleTv)
            rlist    = cnt ^. (hRuleWidget . ruleTStore)
            chstate  = st ^. chatState
        
        io $ setupRuleList boxRList rtv rlist
            
        eventsRuleListConfig rtv
        
        return ()
    where
        insert :: CellRendererTextClass cell => ListItem -> [AttrOp cell]
        insert item = [ cellText := item ^. nameItem]
        setupRuleList :: VBox -> TreeView -> TreeStore ListItem -> IO ()
        setupRuleList boxRList ruleTv ruleList = do
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

-- Agrega regla a la categoria actual
addRuleToList :: Rule -> GuiMonad ()
addRuleToList r =
    getGState >>= \st -> ask >>= \cnt ->
    do
        let chst       = st ^. chatState
        let actCatName = st ^. selCateg
        let actCat     = fromJust $ M.lookup actCatName (categs chst)
        let catIndex   = fromJust $ M.lookupIndex actCatName (categs chst)
        let sizeRules  = M.size $ rules actCat
        let tstore     = cnt ^. (hRuleWidget . ruleTStore)
        io $ treeStoreInsertTree tstore [catIndex] sizeRules (node r actCat sizeRules)
        
    where node r categ size = 
             Node (ListItem (rulename r) categ (Just size))
                  []


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
                    return ()
--                 let rlist = cnt ^. (hRuleWidget . ruleList)
--                 
--                 Just iter <- treeSelectionGetSelected selTv
--                 
--                 let i = listStoreIterToIndex iter
--                 
--                 rule <- listStoreGetValue rlist i
--                 
--                 putStrLn $ show rule
