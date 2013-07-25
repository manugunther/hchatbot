module HChatbot.GUI.Gui where

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

-- | Función principal de la interfaz.
main :: IO ()
main = do
    initGUI

    xml <- builderNew
    builderAddFromFile xml "HChatbot/GUI/hchatbot.ui"

    (gReader,gState) <- makeGState xml
    
    runRWST (do configWindow
                configRuleWidget
                configChatWidget
                configRuleList
            ) gReader gState
    
    mainGUI


makeGState :: Builder -> IO (GReader,GStateRef) 
makeGState xml = do
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
    
    win <- builderGetObject xml castToWindow "window"
    
    gstate <- newRef initState
    let greader = GReader win rwidget chatwidget
    
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
                    let chst = (st ^. chatState)
                    let categMap = categs chst
                    let categ = fromJust $ M.lookup cname categMap
                    
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
                    putStrLn $ "Agregando regla a la categoria " ++ (show categ)
                    let categ' = addRule categ newRule
                    
                    runRWST (addRuleToList newRule) cnt ref
                    
                    putStrLn $ "newRule = " ++ (show newRule)
                    
                    writeRef ref (GState (replaceCateg chst cname categ')
                                         cname)
                    

                    
                    -- Agregar la nueva regla en la interfaz
        return ()
    
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
                            (\ans -> showChat tvChat tvBuf str' ans)
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