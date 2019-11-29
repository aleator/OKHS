{-#OPTIONS_GHC -Wincomplete-patterns -Wall -fno-warn-name-shadowing#-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Main where
-- import           Debug.Pretty.Simple

import           OKHS.Lib
import           OKHS.Types
import           Limitations

import           Brick
import qualified Data.Sequence                 as Seq
import qualified Graphics.Vty                  as V

import           Brick.Widgets.List
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Brick.Widgets.Center

import           System.Process.Typed

import           Data.Char
import           Data.Text.Zipper               ( textZipper )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Data.Map                      as Map
import           Data.FileEmbed
-- TODO Drop bash direct import
import qualified Language.Bash.Pretty          as Bash

import           Control.Exception
import           System.Exit
import qualified GHC.IO.Exception              as IOE
import qualified Database.SQLite.Simple        as SQL
import           System.Directory
import qualified Dhall
import qualified Dhall.Pretty                  as Dhall
import           CmdArgs


ui :: ProgramState -> Widget Text
ui (Select lst) = renderSelect lst
ui (GetArgs _ cmd argDocs argEditors) = renderEditors cmd argDocs argEditors
ui _ = str "Oops. This thing broke"

-- SELECT widget
mkSelect :: NonEmpty (OkSection' [PerhapsRunnableCommand]) -> ProgramState
mkSelect =
  fmap
      (\okSection -> okSection
        { commands = list ("Cmd_" <> name okSection)
                          (Seq.fromList (commands okSection))
                          2
        }
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
  => LZipper (OkSection' (GenericList n t (PerhapsRunnableCommand)))
  -> Widget n
renderSelect lst = joinBorders
  (withBorderStyle
    unicodeRounded
    ((labels |> fmap border |> hBox |> hCenter) <=> listAndDocs)
  )
 where
  listAndDocs = case lst |> lzCurrent |> documentation of
    Nothing   -> hBox (list : addConstraints) |> border
    Just docs -> border (list <+> vBox (docuWidget docs : addConstraints))
  addConstraints = case lst |> lzCurrent |> commands |> listSelectedElement of
    Nothing             -> []
    Just (_, something) -> case bash something of
      Disgressions ds _ ->
        [ toList ds |> map show |> unlines |> txt |> borderWithLabel
            (txt " Constraint violations ")
        ]
      _ -> []
  list       = (lst |> lzCurrent |> commands |> renderListWithIndex fun True)
  docuWidget = txt .> borderWithLabel (txt " Section docs ")
  labels =
    lst
      |> fmap name
      |> lzToListWith nonSelected selected nonSelected
      |> fmap joinBorders
  selected    = txt .> padLeftRight 1 .> withAttr listSelectedAttr
  nonSelected = txt .> padLeftRight 1

  fun i _ perhapsCommand =
    case (bash perhapsCommand :: ParametrizableCommand) of
      Disgressions _ commandString -> funAux
        i
        commandString
        (renderCommentPart perhapsCommand <> " (disabled)")
        cmdDisabledAttr
      ACommand s -> funAux i
                           (Bash.prettyText s)
                           (renderCommentPart perhapsCommand)
                           cmdHdrAttr
  funAux i commandStr comment style =
    padRight Max
      $ (   (show (i + 1) |> (<> ". ") |> txt |> withAttr cmdNumAttr)
        <+> (   (comment |> txtWrap |> withAttr style)
            <=> (commandStr |> toText|> txtWrap)
            )
        )


cmdNumAttr :: AttrName
cmdNumAttr = "select" <> "cmd" <> "num"

cmdHdrAttr :: AttrName
cmdHdrAttr = "select" <> "cmd" <> "hdr"

cmdDisabledAttr :: AttrName
cmdDisabledAttr = "select" <> "cmd" <> "hdr_disabled"

handleSelect
  :: LZipper (OkSection' (GenericList Text Seq.Seq PerhapsRunnableCommand))
  -> V.Event
  -> EventM Text (Next ProgramState)
handleSelect theList ev = case ev of
  V.EvKey (V.KChar n) [] | isDigit n ->
    let goTo = readMaybe [n] |> maybe 0 pred |> listMoveTo
    in  changeCurrentCommand goTo theList |> Select |> continue --TODO: Auto-enter after this
  V.EvKey V.KRight [] -> theList |> lzRight |> Select |> continue
  V.EvKey V.KLeft  [] -> theList |> lzLeft |> Select |> continue
  V.EvKey V.KEnter [] ->
    case lzCurrent theList |> commands |> listSelectedElement of
      Nothing -> halt Quit
      Just (_, OkCommand { bash = Disgressions _ _ }) ->
        Select theList |> continue
      Just (cmdPos, selection@OkCommand { bash = ACommand cmd }) ->
        case arguments cmd of
          [] ->
            substituteNamedArgs [] cmd
              |> Done (succ (lzIndex theList), succ (fromIntegral cmdPos))
              |> halt
          (x : xs) ->
            (x :| xs)
              |> mkEditor (lzIndex theList, fromIntegral cmdPos)
                          selection { bash = cmd }
              |> continue
  _ -> appCurrentCommand (handleListEvent ev) theList >>= Select .> continue


appCurrentCommand
  :: Monad m
  => (fa -> m fa)
  -> LZipper (OkSection' fa)
  -> m (LZipper (OkSection' fa))
appCurrentCommand op = appCurrent <| \okSection -> do
  xNew <- op (commands okSection)
  pure (okSection { commands = xNew })

changeCurrentCommand
  :: (t -> t) -> LZipper (OkSection' t) -> LZipper (OkSection' t)
changeCurrentCommand f =
  changeCurrent (\okSection -> okSection { commands = f (commands okSection) })

changeCurrentWithName :: (a -> a) -> LZipper (n, a) -> (LZipper (n, a))
changeCurrentWithName f = changeCurrent (fmap f)

-- EDITOR widget
mkEditor :: CmdPos -> OkCommand' Unparametrized -> NonEmpty Text -> ProgramState
mkEditor cmdPos cmd =
  fmap (\argName -> (argName, editorText ("ArgEditor-" <> argName) (Just 1) ""))
    .> lzFromNonEmpty
    .> GetArgs cmdPos cmd (argDocs cmd)

renderEditors
  :: (Ord n, Show n)
  => OkCommand' Unparametrized
  -> Map Text Text
  -> LZipper (Text, Editor Text n)
  -> Widget n
renderEditors cmd argDocs argEditors =
  lzToListWith (named False) (named True) (named False) argEditors
    |> vBox
    |> borderWithLabel (txt (" " <> renderOkCommand cmd <> " "))
 where
  named b (argName, editor) =
    let editing = (name argName <+> renderEditor (vBox . map txt) b editor)
    in  case (Map.lookup argName argDocs) of
          Nothing  -> editing
          Just doc -> editing <=> txt doc
  name x = txt (x <> ": ")

type CmdPos = (Natural, Natural)

handleEditor
  :: V.Event
  -> CmdPos
  -> (OkCommand' Unparametrized)
  -> Map Text Text
  -> LZipper (Text, Editor Text Text)
  -> EventM Text (Next ProgramState)
handleEditor ev cmdPos cmd argDocs editors = case ev of
  V.EvKey V.KEnter []
    | isRightmost editors
    -> let filledInArgs =
               [ (argName, (T.concat (getEditContents editor)))
               | (argName, editor) <- lzToList editors
               ]
       in  bash cmd |> substituteNamedArgs filledInArgs |> Done cmdPos |> halt
    | otherwise
    -> lzRight editors |> GetArgs cmdPos cmd argDocs |> continue
  V.EvKey V.KDown [] ->
    lzRight editors |> GetArgs cmdPos cmd argDocs |> continue
  V.EvKey V.KUp [] -> lzLeft editors |> GetArgs cmdPos cmd argDocs |> continue
  V.EvKey (V.KChar 'f') [V.MCtrl] -> suspendAndResume $ do
    let currentTxt = lzCurrent editors |> snd |> getEditContents |> T.concat
    (ec, fileBs) <- readProcessStdout
      (proc "fzf"
            ["-q", toString currentTxt, "--preview", "bat --color=always {}"]
      )
                    -- TODO: Be configurable etc. 
    case ec of
      ExitFailure _ -> return (GetArgs cmdPos cmd argDocs editors)
          -- TODO: Think if this needs to behave differently on different errors
      ExitSuccess   -> do
        let file = T.strip (toStrict (LT.decodeUtf8 fileBs))
            n    = T.length file
        editors
          |> changeCurrentWithName
               (const (textZipper [file] (Just n)) |> applyEdit)
          |> GetArgs cmdPos cmd argDocs
          |> return
  _ -> do
    editorsNew <- appCurrent (traverse (handleEditorEvent ev)) editors
    GetArgs cmdPos cmd argDocs editorsNew |> continue

-- <# STATE #> 

data ProgramState = Select (LZipper (OkSection' (GenericList Text Seq.Seq
                                         PerhapsRunnableCommand)))
           | GetArgs CmdPos
                     (OkCommand' Unparametrized)
                     (Map Text Text)
                     (LZipper (Text, Editor Text Text))
           | Done CmdPos FinalCommand -- [(Text,Text)]
           | Quit

-- Does this throw?
openDB :: FilePath -> IO SQL.Connection
openDB homeDB = do
  conn <- SQL.open homeDB
  SQL.execute_
    conn
    "CREATE TABLE IF NOT EXISTS \
                \LastCommands (id INTEGER PRIMARY KEY, \
                \workingDirectory TEXT, command TEXT, args TEXT)"
  pure conn

--getLastCmd :: FilePath -> SQL.Connection -> IO (Maybe (Bash.List,Text))
--getLastCmd currentDirectory conn = do
--  cmdargs <- SQL.query conn
--                 "select (command,args) from LastCommands \
--                 \where workingDirectory = ? \
--                 \order by id desc" (SQL.Only currentDirectory)
--  case cmdargs of
--        [] -> pure Nothing
--        ((cmd,args):_) -> case parseBash "last-cmd-db" cmd of
--                Left _ -> pure Nothing 
--                Right bash -> Just (bash,args) |> pure
--                -- TODO: split args

--indexLastCommand :: [OkSection a] -> Bash.List -> Maybe (String,Int)
-- indexLastCommand _sections _cmd = undefined




data OkParseError = OkParseError String deriving Show

instance Exception OkParseError

defaultUtilsFile :: ByteString
defaultUtilsFile = $(embedFile "util")

onNonExistingConfig :: FilePath -> (FilePath -> IO ()) -> IO ()
onNonExistingConfig name op = do
  utilPath <- getXdgDirectory XdgConfig ("OKHS" <> "/" <> name)
  doesFileExist utilPath >>= \e -> unless e (op utilPath)

writeAuxiliaryFiles :: IO ()
writeAuxiliaryFiles = do
  -- Be careful and don't overwrite stuff here
  configsAt <- getXdgDirectory XdgConfig "OKHS"
  createDirectoryIfMissing True configsAt -- TODO: Fix this in OKHS
  --
  -- The util file
  onNonExistingConfig "util" (flip writeFileBS defaultUtilsFile)

  -- Type specifiers
  let
    toString = Dhall.declared .> Dhall.prettyExpr .> show
    sectionType =
      (Dhall.inject :: Dhall.InputType (OkSection' [OkCommand' Text]))
        |> toString
    commandType =
      (Dhall.inject :: Dhall.InputType (OkCommand' Text)) |> toString
    limitationType = (Dhall.inject :: Dhall.InputType Limitation) |> toString
  onNonExistingConfig "OKSection.type"  (flip writeFile sectionType)
  onNonExistingConfig "OKCommand.type"  (flip writeFile commandType)
  onNonExistingConfig "Limitation.type" (flip writeFile limitationType)

-- <# MAIN #>
main :: IO ()
main = do
--  conn <- getHomeDirectory >>= openDB
--  cwd <- getCurrentDirectory
--  _lastCommand <- getLastCmd cwd conn
--  SQL.close conn -- TODO: Use with & finally here
  writeAuxiliaryFiles -- Should this be always done
  (okcfg, upgrade) <-
    (   readFileText ".ok"
    >>= readOkSections
    .>  traverse parseOkSection
    .>  either (throw . OkParseError) (, True)
    .>  evaluate
    )
    `catch` (\(_ :: OkParseError) ->
              Dhall.inputFile Dhall.auto ".ok"
                >>= traverse parseOkSection
                .>  either (fromString .> error) (, False)
                .>  pure
            )
    `catch` (\(e :: IOException) -> do
              if IOE.ioe_type e == IOE.NoSuchThing
                then putStrLn "The .ok file does not exist"
                else print e
              exitFailure
            )

  args <- getArguments

  when (convert args && upgrade) $
    writeFile
      ".ok"
      (  Dhall.embed Dhall.inject (map unparseOkSection okcfg)
      |> Dhall.prettyExpr
      |> show
      )

  checkedOkCfg :: NonEmpty (OkSection' [PerhapsRunnableCommand]) <-
    case okcfg of
      []       -> putStrLn ".ok file has no content" >> exitFailure
      (x : xs) -> traverse validateOkSection (x :| xs)

  let state = mkSelect checkedOkCfg

  let lookupCmd (n, m) = do
        section <- toList checkedOkCfg !!? fromIntegral (pred n)
        commands section !!? fromIntegral (pred m)

  case runById args >>= lookupCmd of
    Just (OkCommand { bash = ACommand command }) ->
      let theCmd       
           = substituteNamedArgs
                   (zip (arguments command)
                        (otherArguments args))
                   command
          cmdText = Bash.prettyText theCmd
      in  if printOut args
            then print cmdText
            else do
                 runProcess_ (proc "bash" ["-c",cmdText])
                 Prelude.exitSuccess

    Just (OkCommand { bash = Disgressions ds s }) -> do
      putStrLn ("The command " <> s <> " cannot be run because:")
      traverse_ print ds
      exitFailure

    Nothing -> pass

  x <- defaultMain app state

  case x of
    Done cmdPos theCmd
      | printOut args -> putStrLn $ Bash.prettyText theCmd
      | otherwise -> do
        runProcess_ (proc "bash" ["-c", Bash.prettyText theCmd])
        T.hPutStrLn
          stderr
          ("To run this command again use the flag \x1b[33m'-r "
          <> unparseSectionID cmdPos
          <> "'\x1b[0m optionally followed by arguments, if the command needs any"
          )
    Quit -> pass
    _    -> error "Wrong state"
 where

  handler state (VtyEvent ev) = case ev of
    V.EvKey V.KEsc [] -> halt Quit
    _                 -> case state of
      Select theList -> handleSelect theList ev
      --TODO: get last args
      GetArgs cmdPos cmd argDocs editors ->
        handleEditor ev cmdPos cmd argDocs editors
      other -> halt other
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
  [ (cmdNumAttr      , setFore V.green)
  , (listSelectedAttr, setBack (V.rgbColor 20 20 (20 :: Int)))
  , (cmdHdrAttr      , setFore V.brightGreen)
  , (cmdDisabledAttr , setFore (V.rgbColor 248 222 (126 :: Int)))
  ]


setFore, setBack :: V.Color -> V.Attr
setFore c = V.defAttr { V.attrForeColor = V.SetTo c }
setBack c = V.defAttr { V.attrBackColor = V.SetTo c }


