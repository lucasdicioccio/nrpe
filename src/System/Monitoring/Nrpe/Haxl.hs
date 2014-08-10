{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Monitoring.Nrpe.Haxl where

import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (try, SomeException)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable(..), hashWithSalt)
import Data.Typeable (Typeable)
import Data.Traversable (for, traverse)
import Haxl.Core

import System.Monitoring.Nrpe

data NRPEState

haxlExample :: [Service] -> [ByteString] -> IO ()
haxlExample xs cmds = do
  let stateStore = stateSet NRPEState stateEmpty
  env0 <- initEnv stateStore ()
  rets <- runHaxl env0 (getChecks xs cmds)
  print rets

getChecks :: [Service] -> [ByteString] -> GenHaxl () [NRPE PluginOutput]
getChecks xs cmds = traverse dataFetch [Check x cmd | x <- xs, cmd <- cmds]

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
    e <- Control.Exception.try $ checkNRPE srv cmd
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar (a :: NRPE PluginOutput)
