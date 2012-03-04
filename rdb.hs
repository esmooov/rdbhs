{- LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeFamilies -}

import Blaze.ByteString.Builder
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid
import Data.Binary
import Data.Binary.Get
import Control.Monad
import Debug.Trace

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
              RDBList [BL8.ByteString] |
              RDBSet [BL8.ByteString] |
              RDBZSet [(BL8.ByteString,Double)] |
              RDBHash [(BL8.ByteString,BL8.ByteString)] |
              RDBPair (BL8.ByteString,RDBObj) |
              RDBDatabase Integer [RDBObj] |
              RDB [RDBObj] deriving (Show)

instance Binary RDBObj where
  get = do
        header <- getBytes 9
        dbs <- getDBs
        eof <- getWord8
        return (RDB dbs)

toZsetPairs :: [BL8.ByteString] -> [(BL8.ByteString,Double)]
toZsetPairs [] = []
toZsetPairs l = (x,(read $ BL8.unpack y)):(toZsetPairs xs) where
            ((x:y:[]),xs) = splitAt 2 l

getEncoding :: Word8 -> Word8
getEncoding = (flip shift (-6)) . (.&.) 0xC0

getSecondEncoding :: Word8 -> Word8
getSecondEncoding = (flip shift (-4)) . (.&.) 0x30

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

loadDoubleValue :: Get Double
loadDoubleValue = do
                  len <- getWord8
                  case len of
                    0xff -> do
                      return $ read "-Infinity"
                    0xfe -> do
                      return $ read "Infinity"
                    0xfd -> do
                      return $ read "NaN"
                    l -> do
                      val <- getLazyByteString (fromIntegral l)
                      return $ read $ BL8.unpack val

loadStringObj :: Bool -> Get BL8.ByteString
loadStringObj enc = do 
                    (isEncType,len) <- loadLen
                    if isEncType
                    then do
                      case len of
                        0x00 -> do
                          loadIntegerObj len enc
                        0x01 -> do
                          loadIntegerObj len enc
                        0x02 -> do
                          loadIntegerObj len enc
                        0x03 -> do
                          loadIntegerObj len enc
                    else do
                      getLazyByteString (fromIntegral len)

loadListObj :: Get [BL8.ByteString]
loadListObj = do
              (isEncType,len) <- loadLen
              sequence $ replicate (fromIntegral len) (loadStringObj True)

loadZSetPair :: Get (BL8.ByteString, Double)
loadZSetPair = do
               value <- loadStringObj True
               weight <- loadDoubleValue
               return (value,weight)

loadZSetObj :: Get [(BL8.ByteString,Double)]
loadZSetObj = do
              (isEncType,len) <- loadLen
              sequence $ replicate (fromIntegral len) loadZSetPair

loadZipListObj :: Get [BL8.ByteString]
loadZipListObj = do
                 (isEncType,len) <- loadLen
                 ziplen <- getWord32le
                 offset <- getWord32le
                 num_entries <- getWord16le
                 obj <- loadZipListMembers
                 eoz <- getWord8
                 return obj

loadZipListMembers :: Get [BL8.ByteString]
loadZipListMembers = do
                     opc <- lookAhead $ getWord8
                     if opc == 0xff
                       then return []
                       else do
                            obj <- getZipListMember
                            rest <- loadZipListMembers
                            return (obj:rest)
                            
getZipListMember :: Get BL8.ByteString
getZipListMember = do
                   prevLen <- getZipLen
                   header <- getWord8
                   case ((getEncoding header),(getSecondEncoding header)) of
                     (0x00,_) -> do
                       let len = get6bitLen header
                       getLazyByteString (fromIntegral len)
                     (0x01,_) -> do
                       second_part <- getWord8
                       let len = get14bitLen header second_part
                       getLazyByteString (fromIntegral len)
                     (0x02,_) -> do
                       len <- getWord32be
                       getLazyByteString (fromIntegral len)
                     (0x03,0x00) -> do
                       obj <- getWord16le
                       return $ BL8.pack $ show (fromIntegral obj :: Int16)
                     (0x03,0x01) -> do
                       obj <- getWord32le
                       return $ BL8.pack $ show (fromIntegral obj :: Int32)
                     (0x03,0x02) -> do
                       obj <- getWord64le
                       return $ BL8.pack $ show (fromIntegral obj :: Int64)

getZipLen :: Get Integer
getZipLen = do
            prevLen <- getWord8
            case prevLen of
              0xfe -> do
                len <- getWord32le
                return (fromIntegral len)
              _ -> do
                return (fromIntegral prevLen)

loadIntSetObj :: Get [BL8.ByteString]
loadIntSetObj = do
                (isEncType,len) <- loadLen
                enc <- getWord32le
                setlen <- getWord32le
                sequence $ replicate (fromIntegral setlen) (loadIntSetMember enc)

loadIntSetMember :: Word32 -> Get BL8.ByteString
loadIntSetMember enc = do 
                       case enc of
                         0x02 -> do
                            obj <- getWord16le
                            return $ BL8.pack $ show obj
                         0x04 -> do
                            obj <- getWord32le
                            return $ BL8.pack $ show obj
                         0x08 -> do
                            obj <- getWord64le
                            return $ BL8.pack $ show obj

loadHashObj :: Get [(BL8.ByteString,BL8.ByteString)]
loadHashObj = do
              (isEncType,len) <- loadLen
              sequence $ replicate (fromIntegral len) loadHashMember

loadHashMember :: Get (BL8.ByteString,BL8.ByteString)
loadHashMember = do
                 key <- loadStringObj True
                 value <- loadStringObj True
                 return (key,value)

loadZipMapObj :: Get [(BL8.ByteString,BL8.ByteString)]
loadZipMapObj = do
                (isEncType,len) <- loadLen
                zmlen <- getWord8
                loadZipMapMembers

loadZipMapMembers :: Get [(BL8.ByteString,BL8.ByteString)]
loadZipMapMembers = do
                    is_eoz <- lookAhead $ getWord8
                    case is_eoz of
                      0xff -> do return []
                      _ -> do
                        obj <- loadZipMapMember
                        rest <- loadZipMapMembers
                        return (obj:rest)

loadZipMapMember :: Get (BL8.ByteString,BL8.ByteString)
loadZipMapMember = do
                   first_len <- getWord8
                   key_len <- getZipMapMemberLen first_len
                   key <- getLazyByteString (fromIntegral key_len)
                   first_len_v <- getWord8
                   val_len <- getZipMapMemberLen first_len_v
                   free <- getWord8
                   val <- getLazyByteString val_len
                   return (key,val)

getZipMapMemberLen :: Word8 -> Get Int64
getZipMapMemberLen first_len = do                   
                               case first_len of 
                                 0xfd -> do
                                   l <- getWord32host
                                   return (fromIntegral l)
                                 0xfe -> do
                                   return (fromIntegral 0xfe)
                                 l -> do
                                   return (fromIntegral l)

loadObj :: Word8 -> Get RDBObj
loadObj t = do
            case t of
              -- ^ Load a string value
              0x00 -> do
                obj <- loadStringObj True
                return (RDBString obj)
              -- ^ Load a list value
              0x01 -> do
                obj <- loadListObj
                return (RDBList obj)
              -- ^ Load a set value
              0x02 -> do
                obj <- loadListObj
                return (RDBSet obj)
              -- ^ Load a sorted set value
              0x03 -> do
                obj <- loadZSetObj
                return (RDBZSet $ obj)
              -- ^ Load a hash value
              0x04 -> do
                obj <- loadHashObj
                return (RDBHash $ obj)
              -- ^ Load a zipmap encoded hash
              0x09 -> do
                obj <- loadZipMapObj
                return (RDBHash obj)
              -- ^ Load a ziplist encoded list
              0x0a -> do
                obj <- loadZipListObj
                return (RDBList obj)          
              -- ^ Load an intset encoded set
              0x0b -> do
                obj <- loadIntSetObj
                return (RDBSet obj)
              -- ^ Load a ziplist encoded zset
              0x0c -> do
                obj <- loadZipListObj
                return (RDBZSet $ toZsetPairs obj)


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
       testf <- BL8.readFile "./dump.rdb"
       print $ show (decode testf :: RDBObj)

        
