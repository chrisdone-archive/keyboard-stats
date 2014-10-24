-- Simple keylogger
--
-- Compile & install
--
-- ghc --make Keylog.hs -O2 -threaded -o xinput-keylogger
-- sudo mv xinput-keylogger /usr/local/bin
--
-- Storage (for security)
--
-- $ sudo useradd xinput
-- $ sudo mkdir -p /var/log/xinput/
-- $ sudo touch /var/log/xinput/keys
-- $ sudo chown -R xinput:xinput /var/log/xinput/
-- $ sudo chmod 600 -R /var/log/xinput/
--
-- Run
--
-- $ sudo -u xinput xinput-keylogger 14 1024
--
-- (You can get your keyboard id, e.g. 14, from `xinput list`. The
-- latter number is for buffering before writing to the log file.)
--
-- Kill
--
-- $ sudo -u xinput killall xinput
--
-- (This will kill the child xinput process, which in turn will
-- cleanly end the xinput-keylogger process: will flush any remaining
-- key presses to the file.)
--

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Monoid
import           Data.Time.Clock.POSIX
import qualified Data.Vector as V
import           System.Environment
import           System.IO

-- | Main entry point.
main :: IO ()
main =
  do fp:i:b:_ <- getArgs
     h <- openFile fp AppendMode
     void (runResourceT
             (sourceCmdWithConsumer ("unbuffer xinput test " ++ i)
                                    (chew h (read b))))

-- | Chew on the input and spit out CSV-like rows to the given handle.
chew :: Handle -> Int -> ConduitM ByteString c (ResourceT IO) ()
chew handle size =
  CB.lines $=
  CL.mapM format $=
  CC.conduitVector size $=
  CL.concatMap V.toList $=
  CB.sinkHandle handle

-- | Format the bytestring with the current timestamp.
format :: MonadIO m => ByteString -> m ByteString
format line =
  do t <- liftIO getPOSIXTime
     return (formatted line t)

-- | Format the line into a CSV-like row.
formatted :: RealFrac a => ByteString -> a -> ByteString
formatted line diff =
  S8.pack (show (nominalDiffToMilli diff :: Integer)) <>
  "," <>
  (if mode == "release"
      then "r"
      else "p") <>
  "," <>
  code <>
  "\n"
  where nominalDiffToMilli i = round (i * 1000)
        [mode,code] = S8.words (S8.drop 4 line)
