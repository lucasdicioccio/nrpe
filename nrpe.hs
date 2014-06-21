{-# LANGUAGE OverloadedStrings #-}

module Nrpe where

import Prelude hiding (read)
import Data.Binary (Binary (..), encode, decode)
import Data.Binary.Put (putWord16be, putWord32be, putWord8, putByteString)
import Data.Binary.Get (getWord16be, getWord32be, getBytes)
import Data.ByteString hiding (repeat)
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Word (Word16, Word32)
import Data.Digest.CRC32 (crc32)
import Network.Simple.TCP (connect)
import OpenSSL (withOpenSSL)
import OpenSSL.Session hiding (connect)
import qualified OpenSSL.Session as SSL

type Host = String
type Port = Word16 

type Version = Word16
data QueryType = Req | Res
  deriving (Show, Eq, Ord)
data Code = Ok | Warning | Error | Unknown
  deriving (Show, Eq, Ord, Enum)
data Packet = Packet
  { packetVersion :: Version
  , packetType    :: QueryType
  , packetCRC     :: Word32
  , packetRetCode :: Maybe Code
  , packetBuffer  ::  Buffer
  } deriving (Show, Eq, Ord)
type Buffer = ByteString

e2w16 :: Enum a => a -> Word16
e2w16 = fromIntegral . fromEnum

w162e :: Enum a => Word16 -> a
w162e = toEnum . fromInteger . toInteger  

computeCRC :: Packet -> Word32
computeCRC = crc32 . encode

updateCRC :: Packet -> Packet
updateCRC pkt@(Packet v q _ c b) = Packet v q crc c b
  where crc = computeCRC pkt

instance Binary QueryType where
  put Req = putWord16be 1
  put Res = putWord16be 2
  get = do
    x <- getWord16be
    case x of
      1 -> return Req
      _ -> return Res

instance Binary Packet where
  put (Packet v q crc c b) = do
    putWord16be v
    put q
    putWord32be crc
    putWord16be $ maybe 0 e2w16 c
    putByteString b' >> putWord8 0
    putByteString pad 
    putByteString reserved
    where pad     = C.replicate padLen '\0'
          padLen  = 1024 - (1 + B.length b')
          b'      = B.take 1023 b
          reserved = ":P"
  get = do
    v <- getWord16be
    q <- get
    crc <- getWord32be
    c <- getWord16be
    b <- getBytes 1024
    _pad <- getBytes 2
    return $ Packet v q crc (Just $ w162e c) b

data Service = Service
  { nrpeHost   :: Host
  , nrpePort   :: Port
  , nrpeUseSSl :: Bool
  } deriving (Show, Eq, Ord)

data Request = Request Buffer
  deriving (Show, Eq, Ord)
data Result  = Result Code Buffer
  deriving (Show, Eq, Ord)

packRequest :: Request -> Packet
packRequest (Request b) = updateCRC $ Packet version Req 0 Nothing b
  where version = 2

exec :: Service -> Request -> IO (Packet)
exec s r@(Request b) = do
  let sbuf = encode (packRequest r)
  connect (nrpeHost s) (show $ nrpePort s) (uncurry (act sbuf))
  where act sbuf skt _ = withOpenSSL $ do
          ctx <- context
          contextSetCiphers ctx "ADH"
          ssl <- connection ctx skt
          SSL.connect ssl
          write ssl (L.toStrict sbuf)
          rbuf <- read ssl 1036
          return $ decode $ fromStrict rbuf

nrpe :: Service
nrpe = Service "127.0.0.1" 5666 True

checkUsers = exec nrpe (Request "check_users")
