module HChatbot.Normalization where

import qualified Data.Text as T



-- Normaliza una palabra eliminando repeticiones en vocales, 
-- signos de puntuacion, distincion entre mayus y minus
normalize :: String -> String
normalize = T.unpack . normalizeT . T.pack

normalizeT :: T.Text -> T.Text
normalizeT = id

