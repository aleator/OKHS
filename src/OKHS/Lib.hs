{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE DerivingVia#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE DeriveFoldable#-}
{-#LANGUAGE DeriveTraversable#-}
{-#LANGUAGE DeriveAnyClass#-}
module OKHS.Lib where

import qualified Language.Bash.Parse           as Bash
import qualified Language.Bash.Word            as Bash
import qualified Language.Bash.Syntax          as Bash
import qualified Language.Bash.Pretty          as Bash
import           Data.Generics.Uniplate.Data
import           GHC.Generics
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Either
import           Data.Char                      ( isSpace )
import qualified Data.Map as Map
import Data.Map (Map)
import Numeric.Natural

import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Dhall
import Limitations
import OKHS.Types
import Data.Validation

-- <# TYPES #>
newtype RawCommand = RawCommand {getRaw :: Bash.List}
  deriving (Show,Eq)
  deriving Bash.Pretty via Bash.List

-- TODO. Substitute should work on contents of parametrizable
data ParametrizableCommand = ACommand Unparametrized
                           | Disgressions (NonEmpty Disgression) String

newtype Unparametrized = Unparametrized {getUnparametrized :: Bash.List}
 deriving (Show,Eq)
 deriving Bash.Pretty via Bash.List

newtype FinalCommand = Final {getFinal :: Bash.List}
 deriving (Show,Eq)
 deriving Bash.Pretty via Bash.List
 
type PerhapsRunnableCommand = OkCommand' ParametrizableCommand

validateOkSection
  :: Traversable t
  => OkSection' (t (OkCommand' RawCommand))
  -> IO (OkSection' (t (OkCommand' ParametrizableCommand)))
validateOkSection sect = traverse (traverse validateOkCommand) sect

validateOkCommand :: OkCommand' RawCommand -> IO (OkCommand' ParametrizableCommand) --(Validation [Disgression] (Runnable (OkCommand' a)))
validateOkCommand command = do
   disgressions <- validateLimitations (limitations command) 
   case disgressions of
    [] -> pure (command{bash=command |> bash |> getRaw |> Unparametrized |> ACommand})
    (x:xs) -> pure (command{bash=Disgressions (x:|xs) (renderBashPart command)})

renderOkCommand :: (Bash.Pretty a ) => OkCommand' a -> Text
renderOkCommand command 
  = T.pack (Bash.prettyText (bash command))
    <> T.pack "  #" 
    <> comment command

renderBashPart :: (Bash.Pretty a ) => OkCommand' a -> String
renderBashPart command = Bash.prettyText (bash command)

renderCommentPart :: OkCommand' a -> Text
renderCommentPart = comment

arguments :: Unparametrized -> [Text]
arguments (Unparametrized bash) =
  [ fromString n | p@(Bash.ParamSubst (Bash.Bare (Bash.Parameter n Nothing))) <- universeBi bash ]


-- substituteOkCommand
--   :: [(Text, Text)]
--   -> OkCommand' Un
--   -> OkCommand' FinalCommand
-- substituteOkCommand args = fmap (getRaw .> substituteNamedArgs args .> Final)

substituteNamedArgs :: [(Text, Text)] -> Unparametrized -> FinalCommand
substituteNamedArgs args bash = rewriteBi subst (getUnparametrized bash) |> Final
 where
  subst :: Bash.Span -> Maybe Bash.Span
  subst p@(Bash.ParamSubst (Bash.Bare (Bash.Parameter n Nothing))) = case lookup (T.pack n) args of
    Nothing  -> Nothing
    Just str -> Just (Bash.Single (Bash.stringToWord (T.unpack str)))
  subst _ = Nothing



splitter [] = ("", "")
splitter ('#' : xs) =
  let (start, end) = splitter xs
  in  case end of
        ""        -> ("", '#' : xs)
        something -> ('#' : start, end)
splitter (x : xs) = let (start, end) = splitter xs in (x : start, end)

parseOkSection 
  :: OkSection' [OkCommand' Text] 
  -> Either String (OkSection' [OkCommand' RawCommand])
parseOkSection =  traverse (traverse parseOkCommand)

unparseOkSection :: 
  OkSection' [OkCommand' RawCommand] ->  OkSection' [OkCommand' Text] 
unparseOkSection = fmap (fmap unparseOkCommand)

readOkSections :: Text -> [OkSection' [OkCommand' Text]]
readOkSections input =
  let isHeading str = (T.pack "#") `T.isPrefixOf` T.dropWhile isSpace str
      rec :: [Text] -> [OkSection' [OkCommand' Text]]
      rec more = case more of
        [] -> []
        (x : xs) ->
          let (this, next) = break isHeading xs
          in  if isHeading x
                then 
                      OkSection x (map splitOkCommand this) Nothing : rec next
                else
                      OkSection (T.pack ".ok") 
                                (map splitOkCommand (x : this)) 
                                Nothing : rec next

  in  input |> T.lines |> filter (not . T.null) |> rec

splitOkCommand :: Text -> OkCommand' Text
splitOkCommand s =
  let (cmd, comment) = T.span (/= '#') s
  in  okCommand cmd (T.dropWhile (== '#') comment) 

parseOkCommand :: OkCommand' Text -> Either String (OkCommand' RawCommand)
parseOkCommand command  =
  case Bash.parse "OKFile" (T.unpack (bash command)) of
        Left  err  -> Left (show err) 
        Right list -> Right (command{bash=RawCommand list})

unparseOkCommand :: OkCommand' RawCommand -> OkCommand' Text
unparseOkCommand = fmap (getRaw .> Bash.prettyText .> T.pack)

parseBash :: String -> String -> Either String Bash.List
parseBash src str =  Bash.parse src str |> either (Left . show) Right 

data LZipper a = LZipper [a] a [a] deriving (Eq,Show)

lzFromNonEmpty (x :| xs) = LZipper [] x xs

lzToListWith :: (a -> b) -> (a -> b) -> (a -> b) -> LZipper a -> [b]
lzToListWith front current back (LZipper x z y) =
  map front (reverse x) ++ [current z] ++ map back y

lzCurrent :: LZipper a -> a
lzCurrent (LZipper _ x _) = x

lzToList (LZipper x z y) = reverse x ++ [z] ++ y

lzLeft r@(LZipper []       z _) = r
lzLeft (  LZipper (x : xs) z y) = LZipper xs x (z : y)

lzIndex :: LZipper a -> Natural
lzIndex (LZipper prefix _ _) = fromIntegral (length prefix)

lzRight r@(LZipper _ z []      ) = r
lzRight (  LZipper x z (y : ys)) = LZipper (z : x) y ys

isRightmost lz = case lz of
  LZipper _ _ [] -> True
  other          -> False

changeCurrent :: (a -> a) -> LZipper a -> LZipper a
changeCurrent f (LZipper front current back) = LZipper front (f current) back

appCurrent :: Monad m => (a -> m a) -> LZipper a -> m (LZipper a)
appCurrent op (LZipper front current back) = do
  new <- op current
  pure (LZipper front new back)

instance Functor LZipper where
  fmap f (LZipper front current back) =
    LZipper (map f front) (f current) (map f back)


