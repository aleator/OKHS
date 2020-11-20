{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveAnyClass#-}
module OKHS.Configuration where
import Dhall

data Configuration 
  = Configuration {
      readMakefiles :: Bool
     ,argReaderCmd :: Text}
    deriving (Generic,ToDhall,FromDhall,Show)

