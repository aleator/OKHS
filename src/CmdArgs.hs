{-#LANGUAGE OverloadedStrings#-}
module CmdArgs where
import Options.Applicative
import Data.Bifunctor
import Numeric.Natural
import Data.List (span)

data Options = Options
               { printOut :: Bool
               , convert  :: Bool
               , runById  :: Maybe (Natural,Natural)
               , otherArguments :: [Text]}
               deriving (Eq,Ord,Show)

options :: Parser Options
options = Options 
            <$> switch (long "print" <> short 'p' <> help "Print the command instead of running it")
            <*> switch (long "convert" <> help "Convert the .ok file into normalized form")
            <*> optional (option (eitherReader parseSectionID)
                            (long "run"
                             <> short 'r'
                             <> metavar "CMD"
                             <> help "Run command by number"))
            <*> many (strArgument mempty)

unparseSectionID :: (Natural,Natural) -> Text
unparseSectionID (x,y) = show (succ x) <>"."<>show (succ y)

parseSectionID :: String -> Either String (Natural,Natural)
parseSectionID s =
  let x, r, y :: String
      (x, r) = span (/= '.') s
      y      = drop 1 r
  in  first toString $ do
            sect <-  readEither x 
            command <-  readEither y
            pure (pred sect,pred command)

getArguments = execParser theOpts
theOpts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Run a known command"
     <> header "OKHS - a simple runbook for terminal users" )
                            

