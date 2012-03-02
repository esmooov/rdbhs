{- LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeFamilies -}

import Blaze.ByteString.Builder
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Data.Binary
import Data.Binary.Get

redis_header = fromByteString $ B8.pack "REDIS0003"

{-Redis Opcodes-}
opcode_eof = fromWord8 0xff
opcode_selectdb = fromWord8 0xfe
opcode_expiretime = fromWord8 0xfd
opcode_expiretimems = fromWord8 0xfc

{- Redis Length Codes -}

len_6bit = fromWord8 0x00
len_14bit = fromWord8 0x01
len_32bit = fromWord8 0x02
len_enc = fromWord8 0x04

data RDBObj = RDBString B8.ByteString | 
              RDBPair (B8.ByteString,RDBObj) |
              RDBDatabase Integer [RDBObj] |
              RDB [RDBObj]

instance Binary RDBObj where
  get = do
        header <- getBytes 9
        dbs <- getDBs
        eof <- getWord8
        return (RDB dbs)

getEncoding :: Word8 -> Word8
getEncoding = (flip shift (-6)) $ (.&.) 0xC0

get6bitLen :: Word8 -> Integer
get6bitLen = fromIntegral $ (.&.) 0x3f

get14bitLen :: Word8 -> Word8 -> Integer
get14bitLen f s = fromIntegral $ (ms .|. s) where 
                    ms = shift (a .&. 0x3f) 8

loadLen :: B8.ByteString -> Get (Bool, Integer)
loadLen = do
          first <- getWord8
          case (getEncoding first) of
            len_6bit -> do
              return (False, (get6bitLen first))
            len_14bit -> do
              second <- getWord8
              return (False, (get14bitLen first second))
            len_32bit -> do
              len <- getWord32be
              return (False, (fromIntegral len))
            len_enc -> do
              return (True, (get6bitLen first))

loadObjs :: B8.ByteString -> Get [RDBObj]

getDBs :: B8.ByteString -> Get [RDBObj]
getDBs = do
         opc <- lookAhead $ getWord8
         if opc == opcode_selectdb
           then do
                skip 1
                (isEncType,dbnum) <- loadLen
                objs <- loadObjs
                rest <- getDBs
                return ((RDBDatabase dbnum objs):rest)
           else return [] 

{-output = toByteString $ mconcat [redis_header, redis_eof]-}

main = do 
       testf <- B8.readFile "/usr/local/etc/dumb.rdb"
        
