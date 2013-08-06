module Main where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Lens.Family

import Data.Reference (writeRef,newRef,readRef)
import qualified Data.Map as M
import Data.Maybe

import qualified Data.Text as T

import HChatbot.ParserChat
import HChatbot.Normalization
import HChatbot.ChatbotState
import HChatbot.Category
import HChatbot.Rule

import HChatbot.GUI.GState
import HChatbot.GUI.RuleList

import GramLab.Morfette.Utils

-- | Función principal de la interfaz.
main :: IO ()
main = do
    initGUI

    xml <- builderNew
    builderAddFromFile xml "HChatbot/GUI/hchatbot.ui"

    morfState <- loadModel "data/es"

    (gReader,gState) <- makeGState xml morfState
    
    runRWST (do configWindow
                configRuleWidget
                configChatWidget
                configRuleList
                configToolButtons
            ) gReader gState
    
    mainGUI


makeGState :: Builder -> MorfState -> IO (GReader,GStateRef) 
makeGState xml morfState = do
    -- Obtenemos elementos de la interfaz:
    
    -- RuleWidget
    
    entryRule   <- builderGetObject xml castToEntry "entryRule"
    tvRule      <- builderGetObject xml castToTextView "textviewRule"
    tvAnswer    <- builderGetObject xml castToTextView "textviewAnswer"
    applyButton <- builderGetObject xml castToButton "appButton"
    boxRuleList <- builderGetObject xml castToVBox "boxRuleList"

    ruleTv   <- treeViewNew
    ruleList <- stateToTreeStore initChState
    
    let rwidget = RuleWidget entryRule tvRule tvAnswer applyButton 
                             ruleList ruleTv boxRuleList
    
    -- ChatWidget
    
    tvChat    <- builderGetObject xml castToTextView "textviewChat"
    entryChat <- builderGetObject xml castToEntry "entryChat"
    
    let chatwidget = ChatWidget tvChat entryChat
        
    newCB <- builderGetObject xml castToToolButton "newCategB"
    newRB <- builderGetObject xml castToToolButton "newRuleB"
    
    let toolswidget = ToolsWidget newCB newRB
    
    win <- builderGetObject xml castToWindow "window"
    
    gstate <- newRef initState
    let greader = GReader win rwidget chatwidget toolswidget morfState
    
    return (greader,gstate)
    
    
-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content ->
            io $ do
            let window = content ^. hWindow
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()
    
configRuleWidget :: GuiMonad ()
configRuleWidget = get >>= \ref -> ask >>= \cnt ->
    do  
        let rulew  = cnt ^. hRuleWidget
        let button = rulew ^. applyButton
        _ <- io $ button `on` buttonActivated $ io $ 
                do
                    st  <- readRef ref
                    let cname = st ^. selCateg
                    let mrid  = st ^. selRule
                    let chst = (st ^. chatState)
                    let categMap = categs chst
                    let categ = fromJust $ M.lookup cname categMap
                    
                    maybe (return ())
                          (saveRule cnt ref categ)
                          mrid
                    
        return ()
        
    where saveRule cnt ref categ rid =
                do
                    let rulew  = cnt ^. hRuleWidget
                    st  <- readRef ref
                    let chst = (st ^. chatState)
                    putStrLn "Guardando regla"
                    inpRule <- entryGetText (rulew ^. entryRule)
                    tbufRule <- textViewGetBuffer (rulew ^. tvRule)
                    tbufAns  <- textViewGetBuffer (rulew ^. tvAnswer)
                    stitRule  <- textBufferGetStartIter tbufRule
                    enditRule <- textBufferGetEndIter tbufRule
                    stitAns  <- textBufferGetStartIter tbufAns
                    enditAns <- textBufferGetEndIter tbufAns
                    
                    inpOpts <- textBufferGetText tbufRule stitRule enditRule True
                    out <- textBufferGetText tbufAns stitAns enditAns True
                    
                    let newRule = createRule inpRule inpOpts out
                    let categ' = updateRule categ rid newRule
                    let cname = name categ
                    
                    runRWST (updateRuleList rid newRule categ') cnt ref
                    
                    writeRef ref (GState (replaceCateg chst cname categ')
                                            cname (Just rid))
                    
configChatWidget :: GuiMonad ()
configChatWidget = get >>= \ref -> ask >>= \cnt ->
    do
        let tvChat = _tvChat (cnt ^. hChatWidget)
        let entryChat = _entryChat (cnt ^. hChatWidget)
        
        _ <- io $ entryChat `on` entryActivate $ io $ 
                 do
                     st <- readRef ref
                     let chState = st ^. chatState
                     let catsList = categList chState
                     
                     str' <- entryGetText entryChat
                     let str = normalize str'
                     tvBuf <- textViewGetBuffer tvChat
                     
--                      putStrLn $ "Chat con categorias " ++ (show catsList)
                     
                     either (\_ -> showChat tvChat tvBuf str' "no entiendo")
                            (\(ans,c,r) -> showChat tvChat tvBuf str' ans >>
                               changeActCateg cnt ref chState (name c) >>
                               putStrLn ("Rule " ++ (rname r) ++ "| Category " ++ (name c)))
                            (parseInChat catsList str)
                            
                     entrySetText entryChat ""
        return ()
    
    where showChat tv buf i o = 
            do
                textBufferInsertLn buf ("Yo: " ++ i)
                textBufferInsertLn buf ("Chatbot: " ++ o ++"\n")
                titer2 <- textBufferGetEndIter buf
                
                mark <- textBufferCreateMark buf Nothing titer2 False
                textViewScrollToMark tv mark 0 Nothing
                widgetShowAll tv

          textBufferInsertLn buf str = textBufferGetEndIter buf >>= \titer ->
                                       textBufferInsert buf titer ('\n':str)
                                       
          changeActCateg cnt ref chState cname =
                runRWST (updateGState ((<~) chatState 
                      (updActualCateg chState cname))) cnt ref
                                       
configToolButtons :: GuiMonad ()
configToolButtons = get >>= \ref -> ask >>= \cnt ->
    do
        let categB = cnt ^. (hToolsWidget . newCategB)
        let ruleB = cnt ^. (hToolsWidget . newRuleB)
        
        io $ onToolButtonClicked categB $
            readRef ref >>= \st ->
            return (st ^. chatState) >>= \chst ->
            dialogCategory >>=
            maybe (return ()) (addCategory chst ref cnt)
        
        io $ onToolButtonClicked ruleB $
            do
                st <- readRef ref
                let chst = st ^. chatState
                let chst = st ^. chatState
                let selCatName = st ^. selCateg
                putStrLn $ "Categoria seleccionada "++ (show selCatName)
                let (Just selCat) = getCateg chst selCatName
                
                addR cnt ref selCat chst
        
        
        return ()
                  
    where addCategory chst ref cnt cname =
                let cat = Category cname M.empty in
                    (evalRWST (addCategToList cat >>
                             updateGState ((<~) chatState (addCateg chst cat))
                             ) 
                            cnt ref) >>
                    return ()
          
          addR cnt ref categ chst = do
              let cname = name categ
              
              let nrule = createRule "" "" ""
              let categ' = addRule categ nrule
              
              runRWST (addRuleToList nrule) cnt ref
              
              putStrLn $ "newRule = " ++ (show nrule)
              
              writeRef ref (GState (replaceCateg chst cname categ')
                                        cname (Just $ newid categ))
            
    
  
dialogCategory :: IO (Maybe String)
dialogCategory = do
    dialog <- dialogNew
    box <- dialogGetUpper dialog
    
    cancelB <- dialogAddButton dialog stockCancel ResponseCancel
    okB     <- dialogAddButton dialog stockOk ResponseOk
    
    entry <- entryNew
    boxPackStart box entry PackNatural 5
 
    widgetShowAll box
        
    rid <- dialogRun dialog
    
    case rid of
         ResponseCancel ->  widgetDestroy dialog >> return Nothing
         ResponseOk     -> entryGetText entry >>= \text ->
                            widgetDestroy dialog >> (return . Just) text
   
   
    
    
    