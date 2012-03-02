{- LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeFamilies -}

import Blaze.ByteString.Builder
import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid
import Data.Binary
import Data.Binary.Get

redis_header = BL8.pack "REDIS0003"

-- Redis types

type_string = 0x00
type_list = 0x01
type_set  = 0x02
type_zset = 0x03
type_hash = 0x04

-- Encoded types

type_hash_zipmap = 0x09
type_list_ziplist = 0x0a
type_set_intset =  0x0b 
type_zset_ziplist = 0x0c

-- Length encodings for types

enc_int8  = 0x00 
enc_int16 = 0x01
enc_int32 = 0x02
enc_lzf   = 0x03  

-- Redis Opcodes
opcode_eof = 0xff
opcode_selectdb = 0xfe
opcode_expiretime = 0xfd
opcode_expiretimems = 0xfc

-- Redis Length Codes

len_6bit = 0x00
len_14bit = 0x01
len_32bit = 0x02
len_enc = 0x04

data RDBObj = RDBString BL8.ByteString | 
              RDBPair (BL8.ByteString,RDBObj) |
              RDBDatabase Integer [RDBObj] |
              RDB [RDBObj] deriving (Show)

instance Binary RDBObj where
  get = do
        header <- getBytes 9
        dbs <- getDBs
        eof <- getWord8
        return (RDB dbs)

getEncoding :: Word8 -> Word8
getEncoding = (flip shift (-6)) . (.&.) 0xC0

get6bitLen :: Word8 -> Integer
get6bitLen = fromIntegral . (.&.) 0x3f

get14bitLen :: Word8 -> Word8 -> Integer
get14bitLen f s = fromIntegral $ (ms .|. s) where 
                    ms = shift (f .&. 0x3f) 8

loadLen :: Get (Bool, Integer)
loadLen = do
          first <- getWord8
          case (getEncoding first) of
            0x00 -> do
              return (False, (get6bitLen first))
            0x01 -> do
              second <- getWord8
              return (False, (get14bitLen first second))
            0x02 -> do
              len <- getWord32be
              return (False, (fromIntegral len))
            0x03 -> do
              return (True, (get6bitLen first))

loadObjs :: Get [RDBObj]
loadObjs = do
           code <- lookAhead $ getWord8
           case code of
             -- Handle these correctly
             0xfd -> do
               return ([])
             0xfc -> do
               return ([])
             0xfe -> do
               return ([])
             0xff -> do
               return ([])
             _ -> do
               t <- getWord8
               key <- loadStringObj False
               obj <- loadObj t
               rest <- loadObjs
               return ((RDBPair (key,obj)):rest)

loadIntegerObj :: Integer -> Bool -> Get BL8.ByteString
loadIntegerObj len enc = do
                         case len of 
                            0x00 -> do
                              str <- getWord8
                              return $ BL8.pack $ show str
                            0x01 -> do
                              str <- getWord16le
                              return $ BL8.pack $ show str
                            0x02 -> do
                              str <- getWord32le
                              return $ BL8.pack $ show str
                     

loadStringObj :: Bool -> Get BL8.ByteString
loadStringObj enc = do 
                    (isEncType,len) <- loadLen
                    if isEncType
                    then do
                      case len of
                        0x00 -> do
                          str <- loadIntegerObj len enc
                          return str
                        0x01 -> do
                          str <- loadIntegerObj len enc
                          return str
                        0x02 -> do
                          str <- loadIntegerObj len enc
                          return str
                        0x03 -> do
                          str <- loadIntegerObj len enc
                          return str
                    else do
                      str <- getLazyByteString (fromIntegral len)
                      return str

loadObj :: Word8 -> Get RDBObj
loadObj t = do
            case t of
              type_string -> do
                obj <- loadStringObj True
                return (RDBString obj)
          

getDBs :: Get [RDBObj]
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
       testf <- BL8.readFile "/usr/local/etc/dump.rdb"
       print $ show (decode testf :: RDBObj)

        
