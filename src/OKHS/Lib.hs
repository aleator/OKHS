{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DataKinds#-}
module OKHS.Lib where

import           Language.Bash.Parse           as Bash
import           Language.Bash.Word            as Bash
import           Language.Bash.Syntax          as Bash
import           Language.Bash.Pretty          as Bash
import           Data.Generics.Uniplate.Data
import           GHC.Generics
import           Data.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Either
import           Data.Char                      ( isSpace )

import           Data.List.NonEmpty             ( NonEmpty(..) )
import Flow


data Stage = Parsed | Raw deriving (Eq,Show)

type family BashContent (a :: Stage) where
        BashContent Parsed = Bash.List
        BashContent Raw    = String

data OkCommand' (a :: Stage) 
  = OkCommand {bash :: BashContent a, comment :: String} 
    deriving Generic

type OkCommand = OkCommand' Parsed

data OkSection' f a 
  = OkSection {name :: String
              ,commands ::  f (OkCommand' a)
              ,documentation :: Maybe String}
    deriving (Generic)

type OkSection = OkSection' [] Parsed

renderOkCommand :: OkCommand -> String
renderOkCommand (OkCommand bash comment) = prettyText bash ++ "  #" ++ comment

renderBashPart :: OkCommand -> String
renderBashPart (OkCommand bash _) = prettyText bash

renderCommentPart :: OkCommand -> String
renderCommentPart (OkCommand _ comment) = prettyText comment

arguments bash =
  [ n | p@(ParamSubst (Bare (Parameter n Nothing))) <- universeBi bash ]

substituteNamedArgs :: Bash.List -> [(String, String)] -> Bash.List
substituteNamedArgs bash args = rewriteBi subst bash
 where
  subst :: Span -> Maybe Span
  subst p@(ParamSubst (Bare (Parameter n Nothing))) = case lookup n args of
    Nothing  -> Nothing
    Just str -> Just (Single (stringToWord str))
  subst _ = Nothing


splitter [] = ("", "")
splitter ('#' : xs) =
  let (start, end) = splitter xs
  in  case end of
        ""        -> ("", '#' : xs)
        something -> ('#' : start, end)
splitter (x : xs) = let (start, end) = splitter xs in (x : start, end)

parseOkSections :: String -> [OkSection]
parseOkSections input =
  let isHeading str = "#" `isPrefixOf` dropWhile isSpace str
      rec more = case more of
        [] -> []
        (x : xs) ->
          let (this, next) = break isHeading xs
          in  if isHeading x
                then OkSection x (fmap toParts this) Nothing : rec next
                else OkSection ".ok" (fmap toParts (x : this)) (Just "Default ok section\nhave fun\n\nend default msg") 
                        : rec next
  in  rec . filter (not.null) . lines $ input

toParts :: String -> OkCommand
toParts s =
  let (cmd, comment) = span (/= '#') s
  in  case parse "OKFile" cmd of
        Left  err  -> error (show err) -- FIX before moving to lib
        Right list -> OkCommand list (dropWhile (== '#') comment)

parseBash :: String -> String -> Either String Bash.List
parseBash src str =  parse src str |> either (Left . show) Right 

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


