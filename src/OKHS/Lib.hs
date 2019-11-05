{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE DeriveFoldable#-}
{-#LANGUAGE DeriveTraversable#-}
{-#LANGUAGE DeriveAnyClass#-}
{-#LANGUAGE DataKinds#-}
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

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Flow
import qualified Dhall


data Stage = Parsed | Raw deriving (Eq,Show)

type family BashContent (a :: Stage) where
        BashContent Parsed = Bash.List
        BashContent Raw    = String

data OkCommand' a
  = OkCommand {bash :: a, comment :: Text} 
    deriving (Generic,Dhall.Interpret,Dhall.Inject,Functor,Foldable,Traversable)

        

type OkCommand = OkCommand' Bash.List

data OkSection' fa 
  = OkSection {name :: Text
              ,commands :: fa
              ,documentation :: Maybe Text}
    deriving (Generic,Functor,Foldable,Traversable,Dhall.Interpret,Dhall.Inject)

type OkSection = OkSection' [OkCommand]


renderOkCommand :: OkCommand -> Text
renderOkCommand (OkCommand bash comment) 
  = T.pack (Bash.prettyText bash)
    <> T.pack "  #" 
    <> comment

renderBashPart :: OkCommand -> String
renderBashPart (OkCommand bash _) = Bash.prettyText bash

renderCommentPart :: OkCommand -> Text
renderCommentPart (OkCommand _ comment) = comment

arguments bash =
  [ n | p@(Bash.ParamSubst (Bash.Bare (Bash.Parameter n Nothing))) <- universeBi bash ]

substituteNamedArgs :: Bash.List -> [(String, String)] -> Bash.List
substituteNamedArgs bash args = rewriteBi subst bash
 where
  subst :: Bash.Span -> Maybe Bash.Span
  subst p@(Bash.ParamSubst (Bash.Bare (Bash.Parameter n Nothing))) = case lookup n args of
    Nothing  -> Nothing
    Just str -> Just (Bash.Single (Bash.stringToWord str))
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
  -> Either String (OkSection' [OkCommand' Bash.List])
parseOkSection =  traverse (traverse parseOkCommand)

unparseOkSection :: 
  OkSection' [OkCommand' Bash.List] ->  OkSection' [OkCommand' Text] 
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
  in  OkCommand cmd (T.dropWhile (== '#') comment)

parseOkCommand :: OkCommand' Text -> Either String (OkCommand' Bash.List)
parseOkCommand (OkCommand cmd comment) =
  case Bash.parse "OKFile" (T.unpack cmd) of
        Left  err  -> Left (show err) 
        Right list -> Right (OkCommand list comment)

unparseOkCommand :: OkCommand' Bash.List -> OkCommand' Text
unparseOkCommand = fmap (Bash.prettyText .> T.pack)

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


