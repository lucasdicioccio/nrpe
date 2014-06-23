{-# LANGUAGE OverloadedStrings #-}

module Usage (
    exec
  )  where

import Nrpe
import Nagios
import Data.ByteString (ByteString)
import Control.Applicative ((<$>))

execParse :: Service -> ByteString -> NRPE PluginOutput
execParse s x = (fmap parseOutput) <$> check s x

type NRPE a = IO (Result (Either String a))

liftNRPE = fmap . fmap . fmap

exec :: Service -> ByteString -> (PluginOutput -> a) -> NRPE a
exec s x f = liftNRPE f $ execParse s x
