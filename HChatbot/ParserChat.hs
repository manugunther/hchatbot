module HChatbot.ParserChat where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import qualified Data.Map as M

import Control.Monad.Identity
import System.IO.Unsafe(unsafePerformIO)

import HChatbot.Rule
import HChatbot.Category

type ParsecC a b = ParsecT String a Identity b


-- Datos del matcheo con la regla. Por ahora solo
-- la correspondencia entre nombres de variables y strings asignados
-- a cada nombre
data MatchRuleData = MRData { vars :: [(String,String)] }


-- Dada una lista de RuleInput generamos una lista de pares con cada 
-- elemento de la lista seguido del parser del elemento siguiente.
-- Esto es para poder delimitar las variables. Una variable parsea
-- hasta que se parsee el RuleInput siguiente.
-- Por ejemplo, si la regla dice: Hola [variable] chau
-- entonces para parsear el string "Hola manu chau", manu corresponde
-- a la variable y se parseará hasta poder parsear la palabra "chau".
ruleInPairs :: RuleIn -> [(InputRule,Maybe (ParsecC MatchRuleData ()))]
ruleInPairs []         = []
ruleInPairs [ir]       = [(ir,Nothing)]
ruleInPairs (ir1:ir2:rs) = (ir1,Just $ parseSimpleInput ir2):(ruleInPairs $ ir2:rs)


-- Dada una lista de RuleInput devuelve una lista de parsers
-- para cada RuleInput. Guarda en el estado la asignación de variables.
genParser :: RuleIn -> [ParsecC MatchRuleData ()]
genParser rin = map parseIn (ruleInPairs rin)
    where parseIn (ir,mpstop) =
            case ir of
                 Literal s   -> string s >> return ()
                 IVariable v -> maybe (many anyChar >>= \value -> 
                                       updateMRData (v,value))
                                      (\pstop -> manyTill anyChar pstop >>=
                                       \value -> updateMRData (v,value))
                                      mpstop
                 WC WildCardProd -> maybe (many anyChar >> return ())
                                          (\pstop -> manyTill anyChar pstop >>
                                           return ())
                                          mpstop
                 WC WildCardPlus -> maybe (many1 anyChar >> return ())
                                          (\pstop -> manyTill anyChar pstop
                                                     >>= \s -> if s==""
                                                                  then fail "wcplus"
                                                                  else return ())
                                          mpstop
                                
          updateMRData (v,value) =
              getState >>= \mrd -> putState (mrd { vars = (v,value):(vars mrd)})
                  

parseSimpleInput :: InputRule -> ParsecC MatchRuleData ()
parseSimpleInput (Literal s)   = lookAhead (string s) >> return ()
parseSimpleInput _ = lookAhead (anyChar) >> return ()
                                
                                
-- dada una lista de InputRule, intenta parsear un string 
-- con los parser generados por esa lista.
parseRIn :: RuleIn -> ParsecC MatchRuleData ()
parseRIn rin = 
    let prin = genParser rin in
        foldl (\pres p -> pres >> try p) (return ()) prin

processOut :: Rule -> MatchRuleData -> String
processOut r mrd =
    let outr = output r in
        concat $ map (showChatOut mrd) outr
        
    where showChatOut _ (TextO t) = t
          showChatOut mrd (OVariable v) = 
                maybe (show v)
                      (\value -> value)
                      (lookup v $ vars mrd)
          showChatOut mrd _ = "not implemented"
        
parseWithRule :: Rule -> ParsecC MatchRuleData String
parseWithRule r = 
        foldl (<|>) (fail "rule doesn't match") 
              (map (\rin -> try (parseRIn rin >> eof) >> getState >>=
                            \mrdata -> return (processOut r mrdata))
                   (input r))
              
parseWithRules :: [Rule] -> ParsecC MatchRuleData String
parseWithRules rs =
    foldl (<|>) (fail "neither rule matching") 
              (map parseWithRule rs)
              
parseWithCategory :: [Category] -> ParsecC MatchRuleData String
parseWithCategory cs =
    foldl (<|>) (fail "no matching")
              (map (parseWithRules . M.elems . rules) cs)

parseInChat :: [Category] -> String -> Either ParseError String
parseInChat cs = runParser (parseWithCategory cs) (MRData []) ""
    



