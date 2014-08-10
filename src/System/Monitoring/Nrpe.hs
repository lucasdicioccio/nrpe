
module System.Monitoring.Nrpe (
    checkNRPE
  , liftNRPE
  , IONRPE
  , NRPE
  -- re-exports lower-level modules
  , module System.Monitoring.Nrpe.Protocol
  , module System.Monitoring.Nrpe.Nagios
  ) where

import Data.ByteString (ByteString)
import Control.Applicative ((<$>))

import System.Monitoring.Nrpe.Protocol (Service (..), Result, check)
import System.Monitoring.Nrpe.Nagios (PluginOutput, parseOutput)

type NRPE a = Result (Either String a)
type IONRPE a = IO (NRPE a)

-- Lifts a pure function into an IONRPE.
liftNRPE :: (a -> b) -> IONRPE a -> IONRPE b
liftNRPE = fmap . fmap . fmap

-- Executes and parse an NRPE check result.
checkNRPE :: Service -> ByteString -> IONRPE PluginOutput
checkNRPE s x = fmap parseOutput <$> check s x
