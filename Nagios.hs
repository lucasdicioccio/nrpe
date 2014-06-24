{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Nagios (
    PluginOutput (..)
  , parseOutput
  , srvLongOutput
  , srvPerfData
  ) where

import Prelude hiding (takeWhile)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8
import Data.Typeable
import Control.Applicative ((<$>))

data PluginOutput = PluginOutput
  { srvOutput          :: ByteString
  , srvPerfDataLines   :: [ByteString]
  , srvLongOutputLines :: [ByteString]
  } deriving (Show, Typeable)

srvPerfData = C.intercalate " " . srvPerfDataLines
srvLongOutput = C.intercalate "\n" . srvLongOutputLines

pluginOutput :: Parser PluginOutput
pluginOutput = do
  short   <- tillPipe
  perf0   <- option Nothing (Just <$> (char '|' >> tillNewLine))
  longs   <- C.lines <$> (option "" $ char '\n' >> tillPipe)
  perfXs  <- C.lines <$> (option "" $ char '|' >> takeByteString)
  let perfs = maybe perfXs (:perfXs) perf0
  return $ PluginOutput short perfs longs

tillPipe = takeWhile (notInClass "|")
tillNewLine = takeWhile (notInClass "\n")

parseOutput :: ByteString -> Either String PluginOutput
parseOutput = parseOnly pluginOutput

{- Next are some examples from 
 - http://nagios.sourceforge.net/docs/3_0/pluginapi.html 
 - note the ambiguity on the 'pipe separator': should we consume the spaces around or not?
 - Here I've removed the spaces around the pipes because some plugin (e.g., check_users) do not add spaces around pipes.
 -}

example1 :: ByteString
example1 = "DISK OK - free space: / 3326 MB (56%);"
  
example2 :: ByteString
example2 = "DISK OK - free space: / 3326 MB (56%);|/=2643MB;5948;5958;0;5968"
  
example3 :: ByteString
example3 = C.unlines 
  [ "DISK OK - free space: / 3326 MB (56%);|/=2643MB;5948;5958;0;5968"
  , "/ 15272 MB (77%);"
  , "/boot 68 MB (69%);"
  , "/home 69357 MB (27%);"
  , "/var/log 819 MB (84%);|/boot=68MB;88;93;0;98"
  , "/home=69357MB;253404;253409;0;253414"
  , "/var/log=818MB;970;975;0;980"
  ]
