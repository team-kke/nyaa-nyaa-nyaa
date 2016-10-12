module Timestamp
  ( getTimestampRange
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (readFile, writeFile)
import Data.UnixTime
import Foreign.C.Types (CTime)
import Project (projectPath)
import System.IO.Error
import System.FilePath.Posix

timestampPath :: IO FilePath
timestampPath = (</> "timestamp") <$> projectPath

encodeUnixTime :: UnixTime -> Text
encodeUnixTime = pack . show . utSeconds

decodeUnixTime :: Text -> UnixTime
decodeUnixTime x = UnixTime (read (unpack x)) 0

readTimestamp :: IO (Maybe UnixTime)
readTimestamp =
  (timestampPath >>= readFile >>= return . Just . decodeUnixTime)
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
