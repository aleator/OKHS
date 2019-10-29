{-#OPTIONS_GHC -Wincomplete-patterns -Wall -fno-warn-name-shadowing#-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Main where
-- import           Debug.Pretty.Simple

import           OKHS.Lib

import           Brick
import qualified Data.Sequence                 as Seq
import qualified Graphics.Vty                  as V

import           Brick.Widgets.List
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.Center
import           Data.Char

import           System.Process.Typed

import           Data.Text.Zipper               ( textZipper )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.List.NonEmpty             ( NonEmpty(..) )
-- TODO Drop bash direct import
import qualified Language.Bash.Pretty          as Bash
import qualified Language.Bash.Syntax          as Bash

import           Control.Exception
import           System.Exit
import qualified GHC.IO.Exception              as IOE
import qualified Database.SQLite.Simple        as SQL
import System.Directory (getHomeDirectory,getCurrentDirectory)
 
import Flow

ui :: State -> Widget String
ui (Select lst  ) = renderSelect lst
ui (GetArgs cmd argEditors) = renderEditors cmd argEditors
ui _ = str "Oops. This thing broke"

-- SELECT widget
mkSelect :: NonEmpty OkSection -> State
mkSelect = 
 fmap (\okSection -> okSection{commands=list ("Cmd_"++name okSection)
                                             (Seq.fromList (commands okSection))
                                             2}
                               )
  -- (\okSection  ->
  --   (name okSection
  --   , list ("Cmd_" ++ name okSection) 
  --          (Seq.fromList (commands okSection)) 2)
  -- )
  .> lzFromNonEmpty
  .> Select
  -- TODO: preselect last selection

renderSelect
  :: (Traversable t, Splittable t, Ord n, Show n)
  => LZipper (OkSection' (GenericList n t) 'Parsed)
  -> Widget n
renderSelect lst = joinBorders (hCenter (hBox labels) <=> border listAndDocs)
 where
  listAndDocs = case lst |> lzCurrent |> documentation of
                 Nothing -> border list
                 Just docs -> border (list <+> docuWidget docs)
  list   = (lst |> lzCurrent |> commands |> renderListWithIndex fun True)
  docuWidget = str .> border
  labels = lst |> fmap name |> lzToListWith nonSelected selected nonSelected       
  selected     = str .> border .> withAttr listSelectedAttr
  nonSelected  = str .> border 

  fun i _ s =
    padRight Max
      $ (   str (show (i + 1) ++ ". ")
        <+> (str (renderCommentPart s) <=> str (renderBashPart s))
        )

handleSelect
  :: LZipper (OkSection' (GenericList String Seq.Seq) 'Parsed)
  -> V.Event
  -> EventM String (Next State)
handleSelect theList ev = case ev of
  V.EvKey (V.KChar n) [] | isDigit n -> continue
    (Select (changeCurrentCommand (listMoveTo (read [n] - 1)) theList))
  V.EvKey V.KRight [] -> theList |> lzRight |> Select |> continue
  V.EvKey V.KLeft  [] -> theList |> lzLeft  |> Select |> continue
  V.EvKey V.KEnter [] -> case lzCurrent theList |> commands |> listSelectedElement of
    Nothing             -> halt Quit
    Just (_, selection) -> case arguments (bash selection) of
      []       -> halt (Done selection [])
      (x : xs) -> x:|xs |> mkEditor selection |> continue 
  _ -> appCurrentCommand (handleListEvent ev) theList >>=  Select .> continue


appCurrentCommand :: 
    Monad m
    => (f (OkCommand' a) -> m (f (OkCommand' a)))
    -> LZipper (OkSection' f a) -> m (LZipper (OkSection' f a))
appCurrentCommand op = appCurrent <| \okSection -> do
  xNew <- op (commands okSection)
  pure (okSection{commands = xNew})

changeCurrentCommand :: 
    (f (OkCommand' a) -> f (OkCommand' a))
    -> LZipper (OkSection' f a) 
    -> LZipper (OkSection' f a)
changeCurrentCommand f =
 changeCurrent (\okSection-> okSection{commands=f (commands okSection)})

changeCurrentWithName :: (a -> a) -> LZipper (n, a) -> (LZipper (n, a))
changeCurrentWithName f = changeCurrent (fmap f)

-- EDITOR widget
mkEditor :: OkCommand -> NonEmpty [Char] -> State
mkEditor cmd =
        fmap
            (\argName ->
              (argName, editorText ("ArgEditor-" ++ argName) (Just 1) "")
            )
        .> lzFromNonEmpty
        .> GetArgs cmd

renderEditors :: (Ord n, Show n) =>
                       OkCommand -> LZipper ([Char], Editor Text n) -> Widget n
renderEditors cmd argEditors =
    lzToListWith (named False) (named True) (named False) argEditors
    |> vBox
    |> borderWithLabel (str (" " <> renderOkCommand cmd <> " "))
 where
  named b (argName, editor) =
    name argName <+> renderEditor (vBox . map txt) b editor
  name x = str (x ++ ": ")

handleEditor :: V.Event
                      -> OkCommand
                      -> LZipper (String, Editor Text String)
                      -> EventM String (Next State)
handleEditor ev cmd editors = case ev of
  V.EvKey V.KEnter []
    | isRightmost editors
    -> let filledInArgs =
               [ (argName, T.unpack (T.concat (getEditContents editor)))
               | (argName, editor) <- lzToList editors
               ]
       in  halt (Done cmd filledInArgs)
    | otherwise
    -> continue (GetArgs cmd (lzRight editors))
  V.EvKey V.KDown       []        -> lzRight editors |> GetArgs cmd |> continue
  V.EvKey V.KUp         []        -> lzLeft editors  |> GetArgs cmd |> continue
  V.EvKey (V.KChar 'f') [V.MCtrl] -> suspendAndResume $ do
    (ec, fileBs) <- readProcessStdout "fzf" -- TODO: Be configurable etc. 
    case ec of
      ExitFailure _ -> return (GetArgs cmd editors) 
          -- TODO: Think if this needs to behave differently on different errors
      ExitSuccess   -> do
        let file = T.strip (LT.toStrict (LT.decodeUtf8 fileBs))
            n    = T.length file
        editors
          |> changeCurrentWithName
              (const (textZipper [file] (Just n)) |> applyEdit)
          |> GetArgs cmd 
          |> return
  _ -> do
    editorsNew <- appCurrent (traverse (handleEditorEvent ev)) editors
    GetArgs cmd editorsNew |> continue

data State = Select (LZipper (OkSection' (GenericList String Seq.Seq) 
                                         'Parsed))
           | GetArgs OkCommand (LZipper (String, Editor Text String))
           | Done OkCommand [(String,String)]
           | Quit

-- Does this throw?
openDB :: FilePath -> IO SQL.Connection
openDB homeDB = do
  conn <- SQL.open homeDB
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS \
                \LastCommands (id INTEGER PRIMARY KEY, \
                \workingDirectory TEXT, command TEXT, args TEXT)"
  pure conn

getLastCmd :: FilePath -> SQL.Connection -> IO (Maybe (Bash.List,String))
getLastCmd currentDirectory conn = do
  cmdargs <- SQL.query conn
                 "select (command,args) from LastCommands \
                 \where workingDirectory = ? \
                 \order by id desc" (SQL.Only currentDirectory)
  case cmdargs of
        [] -> pure Nothing
        ((cmd,args):_) -> case parseBash "last-cmd-db" cmd of
                Left _ -> pure Nothing 
                Right bash -> Just (bash,args) |> pure
                -- TODO: split args

indexLastCommand :: [OkSection] -> Bash.List -> Maybe (String,Int)
indexLastCommand _sections _cmd = undefined
  
  


-- <# MAIN #>
main :: IO ()
main = do
--  conn <- getHomeDirectory >>= openDB
  cwd <- getCurrentDirectory
--  _lastCommand <- getLastCmd cwd conn
--  SQL.close conn -- TODO: Use with & finally here
  ok <-
    readFile ".ok"
      `catch` (\(e :: IOException) -> do
                if IOE.ioe_type e == IOE.NoSuchThing
                  then putStrLn "The .ok file does not exist"
                  else print e
                exitFailure
              )

  let okcfg = parseOkSections ok
  let checkedOkCfg = case okcfg of
        []       -> error ".ok file has no content"
        (x : xs) -> (x :| xs)

  let state = mkSelect checkedOkCfg

  x <- defaultMain (app) state

  case x of
    Done theCmd theArgs -> case arguments (bash theCmd) of
      [] -> runProcess_ (proc "bash" ["-c", renderBashPart theCmd])
      _  ->
        let whole = substituteNamedArgs (bash theCmd) theArgs
        in  runProcess_ (proc "bash" ["-c", Bash.prettyText whole])
    Quit -> pure ()
    _    -> error "Wrong state"
 where

  handler state (VtyEvent ev) = case ev of
    V.EvKey V.KEsc [] -> halt Quit
    _                 -> case state of
      Select theList      -> handleSelect theList ev
      --TODO: get last args
      GetArgs cmd editors -> handleEditor ev cmd editors
      other               -> halt other
  handler state _ = continue state

  app = App { appDraw         = (\x -> [x]) <. ui
            , appChooseCursor = showFirstCursor
            , appHandleEvent  = handler
            , appStartEvent   = return
            , appAttrMap      = const theMap
            }

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [(listAttr, V.white `on` V.black)
  ,(listSelectedAttr, V.blue `on` V.white)]


