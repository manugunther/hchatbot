module HChatbot.Normalization where

import Data.Char(toLower)


ignoringChars = ['!','?','"','$','%','&','/','(',')','=','_','.',',',';',':','-']

-- Normaliza una palabra eliminando repeticiones en vocales, 
-- signos de puntuacion, distincion entre mayus y minus
normalize :: String -> String
normalize s = 
    let lower = map toLower s in
        noRepeat $ filter (not . flip elem ignoringChars) lower

    where noRepeat s =
            foldl (\acum c -> if acum==[] || (last acum)/=c
                                 then acum++[c]
                                 else acum)
                  [] s
        
        
        