module Timestamp
  ( getTimestampRange
  ) where

import Data.UnixTime
import Foreign.C.Types (CTime)
import Project (projectPath)
import System.IO.Error
import System.FilePath.Posix
import qualified System.IO.Strict as Strict

timestampPath :: IO FilePath
timestampPath = (</> "timestamp") <$> projectPath

encodeUnixTime :: UnixTime -> String
encodeUnixTime = show . utSeconds

decodeUnixTime :: String -> UnixTime
decodeUnixTime x = UnixTime (read x) 0

readTimestamp :: IO (Maybe UnixTime)
readTimestamp =
  (timestampPath >>= Strict.readFile >>= return . Just . decodeUnixTime)
    `catchIOError` (return . const Nothing)

writeTimestamp :: UnixTime -> IO ()
writeTimestamp = (timestampPath >>=) . flip writeFile . encodeUnixTime

minsAgo :: CTime -> UnixTime -> UnixTime
minsAgo min (UnixTime a _) = UnixTime (a - sec) 0
  where sec = min * 60

now :: IO UnixTime
now = getUnixTime >>= \(UnixTime a _) -> return $ UnixTime a 0

getTimestampRange :: IO (UnixTime, UnixTime)
getTimestampRange = do
  maybeSince <- readTimestamp
  til <- now
  writeTimestamp til
  case maybeSince of
    Just since -> return (since, til)
    otherwise -> return (5 `minsAgo` til, til)
