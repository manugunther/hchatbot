module HChatbot.ParserChat where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import qualified Data.Map as M
import Data.Maybe(fromJust)

import Control.Monad.Identity
import System.IO.Unsafe(unsafePerformIO)

import HChatbot.Rule
import HChatbot.Category

type ParsecC a b = ParsecT String a Identity b


-- Datos del matcheo con la regla. Por ahora solo
-- la correspondencia entre nombres de variables y strings asignados
-- a cada nombre
data MatchRuleData = MRData { vars :: [(String,String)]
                            , ruleData :: Maybe (Category,Rule)}


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
                 -- s debe encontrarse literalmente y si no es el final del string
                 -- entonces debe seguir de un espacio.
                 Literal s   -> string s >> (eof <|> (char ' ' >> return ()))
                 -- una variable es cualquier texto encontrado hasta que haya matching
                 -- con la regla siguiente (pstop)
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
parseSimpleInput (Literal s)   = try $ lookAhead (string s) >> return ()
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
        
parseWithRule :: Category -> Rule -> ParsecC MatchRuleData String
parseWithRule c r = 
        foldl (<|>) (fail "rule doesn't match") 
              (map (\rin -> try (parseRIn rin >> eof) >> getState >>=
                            \mrdata -> putState (mrdata { ruleData = Just (c,r)}) >>
                            return (processOut r mrdata))
                   (input r))
              
parseWithRules :: Category -> [Rule] -> ParsecC MatchRuleData String
parseWithRules c rs =
    foldl (<|>) (fail "neither rule matching") 
              (map (parseWithRule c) rs)
              
parseWithCategory :: [Category] -> ParsecC MatchRuleData String
parseWithCategory cs =
    foldl (<|>) (fail "no matching")
              (map (\c -> (parseWithRules c . M.elems . rules) c) cs)

parseInChat :: [Category] -> String -> Either ParseError (String,Category,Rule)
parseInChat cs = runParser (parseWithCategory cs >>= \res -> getState >>= 
                           -- aqui usamos fromJust porque estamos seguros que si parseó,
                           -- tenemos una categoría y una regla correspondientes
                           \mrdata -> return (fromJust $ ruleData mrdata) >>= \(c,r) ->
                           return (res,c,r)) (MRData [] Nothing) ""
    



