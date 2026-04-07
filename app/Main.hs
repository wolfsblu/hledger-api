module Main (main) where

import Options.Applicative
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), takeDirectory)

import Lib (runServer, AppConfig(..))

-- | Command line options
data Options = Options
  { optJournal  :: Maybe FilePath
  , optRulesDir :: Maybe FilePath
  , optPort     :: Int
  , optHost     :: String
  }

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
      ( long "journal"
     <> short 'f'
     <> metavar "FILE"
     <> help "Path to hledger journal file (default: $LEDGER_FILE or ~/.hledger.journal)"
      ))
  <*> optional (strOption
      ( long "rules-dir"
     <> short 'r'
     <> metavar "DIR"
     <> help "Directory containing .csv.rules files (default: $RULES_DIR or journal file's parent directory)"
      ))
  <*> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8080
     <> showDefault
     <> help "Port to listen on"
      )
  <*> strOption
      ( long "host"
     <> short 'h'
     <> metavar "HOST"
     <> value "127.0.0.1"
     <> showDefault
     <> help "Host to bind to"
      )

-- | Full parser with help info
optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Serve a hledger journal as a JSON API"
 <> header "hledger-api - REST API server for hledger"
  )

-- | Resolve journal path from options, env var, or default
resolveJournalPath :: Maybe FilePath -> IO FilePath
resolveJournalPath (Just path) = return path
resolveJournalPath Nothing = do
  envPath <- lookupEnv "LEDGER_FILE"
  case envPath of
    Just path -> return path
    Nothing -> do
      home <- getHomeDirectory
      return $ home </> ".hledger.journal"

-- | Resolve rules directory from options, env var, or journal path default
resolveRulesDir :: Maybe FilePath -> FilePath -> IO FilePath
resolveRulesDir (Just dir) _           = return dir
resolveRulesDir Nothing    journalPath = do
  envDir <- lookupEnv "RULES_DIR"
  case envDir of
    Just dir -> return dir
    Nothing  -> return (takeDirectory journalPath)

main :: IO ()
main = do
  opts <- execParser optsInfo
  journalPath <- resolveJournalPath (optJournal opts)
  rulesDir    <- resolveRulesDir (optRulesDir opts) journalPath
  let config = AppConfig
        { configJournalPath = journalPath
        , configRulesDir    = rulesDir
        , configPort        = optPort opts
        , configHost        = optHost opts
        }
  runServer config
