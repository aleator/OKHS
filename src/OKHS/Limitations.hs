{-#OPTIONS_GHC -Wall#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveAnyClass#-}
module OKHS.Limitations where
import Data.Text (Text,unpack)
import qualified Dhall
import           GHC.Generics
import System.Directory
-- import Flow
import Data.Time.Clock

data Limitation = FileExists Text
                | DirectoryExists Text
                | FileRecent Text
    deriving (Generic,Dhall.Interpret,Dhall.Inject)

data Disgression = MissingFile Text
                 | MissingDirectory Text
                 | OldFile Text
                 deriving (Show)

validateLimitations :: [Limitation] -> IO [Disgression]
validateLimitations lims = concat <$> traverse validateLimitation lims

validateLimitation :: Limitation -> IO [Disgression]
validateLimitation limitation = case limitation of
  FileExists filename ->
    checkPure (doesFileExist (unpack filename)) (MissingFile filename)
  DirectoryExists filename ->
    checkPure (doesDirectoryExist (unpack filename)) (MissingDirectory filename)
  FileRecent filename -> do
    exists <- doesFileExist (unpack filename)
    if exists
     then do
        modified <- getModificationTime (unpack filename)
        now      <- getCurrentTime
        if (utctDay modified == utctDay now)
          then pure []
          else (pure [OldFile filename])
     else pure [MissingFile filename]
 where
  checkPure cond retVal =
    cond >>= \condRes -> if condRes then pure [] else pure [retVal]


