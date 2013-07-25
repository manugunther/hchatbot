module HChatbot.Normalization where

import Data.Char(toLower)
import qualified Data.Map as M


ignoringChars = ['!','?','"','$','%','&','/','(',')','=','_','.',',',';',':','-']

accentChars = M.fromList [('á','a'),('é','e'),('í','i'),('ó','o'),('ú','u')]

-- Normaliza una palabra eliminando repeticiones en vocales, 
-- signos de puntuacion, distincion entre mayus y minus, acentos, 
-- espacios al inicio y fin de la palabra.
-- TODO: la doble r y la doble l no debería eliminarlas.
normalize :: String -> String
normalize s = 
    let lower = map toLower s in
        (noAccents . noWhitesEnd . noWhitesSt . noRepeat)
                (filter (not . flip elem ignoringChars) lower)

    where noRepeat s =
            foldl (\acum c -> if acum==[] || (last acum)/=c
                                 then acum++[c]
                                 else acum)
                  [] s
          
          noWhitesSt [] = []
          noWhitesSt s@(c:cs) = if c==' '
                                    then cs
                                    else s
          noWhitesEnd [] = []
          noWhitesEnd cs = if last cs==' '
                                    then take (length cs - 1) cs
                                    else cs
        
          noAccents [] = []
          noAccents (c:cs) = 
              let c' = maybe c id (M.lookup c accentChars) in
                  (c':noAccents cs)
              