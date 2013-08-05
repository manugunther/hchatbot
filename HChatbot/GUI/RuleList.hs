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
    Node (ListItem (rulename r) (name categ) (Just rid))
         []

categoryToTree :: Category -> Tree ListItem
categoryToTree c =
    Node (ListItem (name c) (name c) Nothing)
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
        let tstore = cnt ^. (hRuleWidget . ruleTStore)
        let rtv    = cnt ^. (hRuleWidget . ruleTv)
        selTv <- io $ treeViewGetSelection rtv
        Just iter <- io $ treeSelectionGetSelected selTv
        path <- io $ treeModelGetPath tstore iter
        let catIndex   = take 1 path -- fromJust $ M.lookupIndex actCatName (categs chst)
        let sizeRules  = M.size $ rules actCat
        let tstore     = cnt ^. (hRuleWidget . ruleTStore)
        
        io $ treeStoreInsertTree tstore catIndex sizeRules (node r actCat)
        
        io $ treeViewExpandToPath rtv (catIndex++[sizeRules])
        
    where node r categ = 
             Node (ListItem (rulename r) (name categ) (Just $ newid categ))
                  []
                  
-- Actualiza el nombre de la regla seleccionada
updateRuleList :: RuleId -> Rule -> Category -> GuiMonad ()
updateRuleList rid r c =
    ask >>= \cnt -> io $
    do
        let tstore = cnt ^. (hRuleWidget . ruleTStore)
        let rtv    = cnt ^. (hRuleWidget . ruleTv)
        selTv <- treeViewGetSelection rtv
        Just iter <- treeSelectionGetSelected selTv
        
        path <- treeModelGetPath tstore iter
        
        treeStoreSetValue tstore path 
                (ListItem (rulename r) (name c) (Just $ rid))

addCategToList :: Category -> GuiMonad ()
addCategToList c =
    getGState >>= \st -> ask >>= \cnt ->
    do
        let chst     = st ^. chatState
        let sizeCats = M.size $ categs chst
        let tstore   = cnt ^. (hRuleWidget . ruleTStore)
        io $ treeStoreInsertTree tstore [] sizeCats 
                    (Node (ListItem (name c) (name c) Nothing) [])
                  
                  
eventsRuleListConfig :: TreeView -> GuiMonad ()
eventsRuleListConfig rtv = get >>= \ref -> ask >>= \cnt -> io $ do
            selTv <- treeViewGetSelection rtv
            treeSelectionSetMode selTv SelectionSingle
            treeSelectionUnselectAll selTv
            onSelectionChanged selTv (eval (selAction selTv) cnt ref)
            return ()
    where
        eval action cnt ref = evalRWST action cnt ref >> return ()
        selAction :: TreeSelection -> GuiMonad ()
        selAction selTv = ask >>= \cnt -> get >>= \ref -> 
                getGState >>= \st -> io $ do
                let tstore = cnt ^. (hRuleWidget . ruleTStore)
                
                Just iter <- treeSelectionGetSelected selTv
                
                path <- treeModelGetPath tstore iter
                
                item <- treeStoreGetValue tstore path
                
                let categ = item ^. catName
                let mrid  = item ^. ruleId
                let chst  = st ^. chatState
                
                let (Just newSelCateg) = getCateg chst categ
                
                -- cambio la categoría seleccionada
                runRWST (updateGState ((<~) selCateg categ) >>
                         updateGState ((<~) selRule mrid)) cnt ref
                
                maybe (return ())
                      (\rid -> do
                          let (Just rule) = getRule newSelCateg rid
                          fillRuleWidget rule cnt
                          return ()
                       )
                      mrid
                
                return ()
                
        fillRuleWidget r cnt = do
                let entry   = cnt ^. (hRuleWidget . entryRule)
                let optInTv = cnt ^. (hRuleWidget . tvRule)
                let ansTv   = cnt ^. (hRuleWidget . tvAnswer)
                let rulein  = head $ input r
                let ruleout = output r
                
                optInTb <- textViewGetBuffer optInTv
                ansTb   <- textViewGetBuffer ansTv
                
                entrySetText entry (showRuleInOut rulein)
                textBufferSetText optInTb (choicesInStr r)
                textBufferSetText ansTb (showRuleInOut ruleout)
            
            
--                 let i = listStoreIterToIndex iter
--                 
--                 rule <- listStoreGetValue rlist i
--                 
--                 putStrLn $ show rule
