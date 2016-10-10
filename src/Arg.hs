module Arg
  ( configPath
  , token
  ) where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)

data Flag = ConfigDir String
          | Token String
          | Help
          deriving (Show)

flagDesc :: [OptDescr Flag]
flagDesc =
  [ Option ['c'] ["config"] (ReqArg ConfigDir "DIR") "Config directory containing anime.yaml and config.yaml"
  , Option ['t'] ["token"] (ReqArg Token "STRING") "Personal access token issued by LINE Notify"
  , Option ['h'] ["help"] (NoArg Help) "Show help"
  ]

helpText :: String
helpText = usageInfo "nyaa - the anime notifier\n" flagDesc

data Args = Args { getConfigDir :: String
                 , getToken :: String
                 }

args :: IO Args
args = do
  args <- getArgs
  case getOpt Permute flagDesc args of
    ([ConfigDir configDir, Token token], [], [])
      -> return $ Args configDir token
    ([Token token, ConfigDir configDir], [], [])
      -> return $ Args configDir token
    otherwise -> do
      putStrLn helpText
      exitSuccess

configPath :: IO String
configPath = getConfigDir <$> args

token :: IO String
token = getToken <$> args
