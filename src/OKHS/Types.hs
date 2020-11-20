{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE DeriveFoldable#-}
{-#LANGUAGE DeriveTraversable#-}
{-#LANGUAGE DeriveAnyClass#-}
{-#LANGUAGE DerivingVia#-}
module OKHS.Types where
import Limitations
import           Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Text.PrettyPrint (Doc)
import Dhall

data OkCommand' a
  = OkCommand {bash :: a
              ,comment :: Text
              ,argDocs :: Map Text Text
              ,limitations :: [Limitation]} 
    deriving (Generic,Show,Dhall.Interpret,Dhall.Inject,Functor,Foldable,Traversable)

okCommand bash comment = OkCommand bash comment mempty mempty

data OkSection' fa 
  = OkSection {name :: Text
              ,commands :: fa
              ,documentation :: Maybe Text}
    deriving (Generic,Show,Functor,Foldable,Traversable,Dhall.Interpret,Dhall.Inject)

