{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Usage (
    exec
  )  where

import Nrpe
import Nagios
import Data.ByteString (ByteString)
import Control.Applicative ((<$>))

import Control.Concurrent.Async
import Control.Exception
import Control.Exception
import Data.Hashable
import Data.Typeable
import Data.Traversable (for)

import Haxl.Core


execParse :: Service -> ByteString -> IONRPE PluginOutput
execParse s x = (fmap parseOutput) <$> check s x

type NRPE a = Result (Either String a)
type IONRPE a = IO (Result (Either String a))

liftNRPE = fmap . fmap . fmap

exec :: Service -> ByteString -> (PluginOutput -> a) -> IONRPE a
exec s x f = liftNRPE f $ execParse s x


haxlExample = do
  let stateStore = stateSet NRPEState stateEmpty
  env0 <- initEnv stateStore ()
  rets <- runHaxl env0 getChecks
  print rets

getChecks :: GenHaxl () [NRPE PluginOutput]
getChecks = do
  for ["check_users", "check_load"] $ \cmd -> do
    dataFetch $ Check nrpe cmd
  

data NRPECheck a where  
  Check :: Service -> ByteString -> NRPECheck (NRPE PluginOutput)
  deriving (Typeable)

instance Show (NRPECheck a) where
  show (Check srv b) = "Check ("++ show srv ++ ", " ++ show b ++ ")" 

instance Eq (NRPECheck a) where
  (Check srv1 b1) == (Check srv2 b2) = srv1 == srv2 && b1 == b2

instance Show1 NRPECheck where show1 = show

instance Hashable Service where
  hashWithSalt s (Service h p _) = hashWithSalt s (0::Int, h, p)

instance Hashable (NRPECheck a) where
  hashWithSalt s (Check a c) = hashWithSalt s (0::Int, a, c)

instance StateKey NRPECheck where
  data State NRPECheck = NRPEState

instance DataSourceName NRPECheck where
  dataSourceName _ = "NRPE"

instance DataSource u NRPECheck where
  fetch = nrpeFetch

nrpeFetch NRPEState _flags _user bfs = do
  AsyncFetch $ \inner -> do
    asyncs <- mapM fetchAsync bfs
    inner
    mapM_ wait asyncs

fetchAsync
  :: BlockedFetch NRPECheck
  -> IO (Async ())
fetchAsync (BlockedFetch (Check srv cmd) rvar) =
  async $ do
    e <- Control.Exception.try $ exec srv cmd id
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar (a :: NRPE PluginOutput)
