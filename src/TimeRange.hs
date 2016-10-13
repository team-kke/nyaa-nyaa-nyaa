module TimeRange
  ( TimeRange
  , getTimeRange
  ) where

import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Project (projectPath)
import System.IO.Error
import System.FilePath.Posix
import qualified System.IO.Strict as Strict

type TimeRange = (UTCTime, UTCTime)

recentTimePath :: IO FilePath
recentTimePath = (</> "recent") <$> projectPath

readRecentTime :: IO (Maybe UTCTime)
readRecentTime =
  (recentTimePath >>= Strict.readFile >>= return . Just . read)
    `catchIOError` (return . const Nothing)

writeTimestamp :: UTCTime -> IO ()
writeTimestamp = (recentTimePath >>=) . flip writeFile . show

minsAgo :: NominalDiffTime -> UTCTime -> UTCTime
minsAgo min = addUTCTime ((-60) * min)

getTimeRange :: IO TimeRange
getTimeRange = do
  maybeSince <- readRecentTime
  til <- getCurrentTime
  writeTimestamp til
  case maybeSince of
    Just since -> return (since, til)
    _ -> return (10 `minsAgo` til, til)
