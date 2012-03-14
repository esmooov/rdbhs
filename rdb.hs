{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeFamilies, BangPatterns #-}

import Blaze.ByteString.Builder
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid
import Data.Serialize
import Data.Serialize.Get
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Debug.Trace
import qualified System.IO.Unsafe as IOU
import Data.Conduit hiding (Done)
import qualified Data.Conduit.Binary as C

redisHeader = BL8.pack "REDIS0003"

-- Redis types

typeString = 0x00
typeList = 0x01
typeSet  = 0x02
typeZset = 0x03
typeHash = 0x04

-- Encoded types

typeHashZipmap = 0x09
typeListZiplist = 0x0a
typeSetIntset =  0x0b 
typeZsetZiplist = 0x0c

-- Length encodings for types

encInt8  = 0x00 
encInt16 = 0x01
encInt32 = 0x02
encLzf   = 0x03  

-- Redis Opcodes
opcodeEof = 0xff
opcodeSelectdb = 0xfe
opcodeExpiretime = 0xfd
opcodeExpiretimems = 0xfc

-- Redis Length Codes
len6bit = 0x00
len14bit = 0x01
len32bit = 0x02
len_enc = 0x04

data RDBObj = RDBString B8.ByteString | 
              RDBList [B8.ByteString] |
              RDBSet [B8.ByteString] |
              RDBZSet [(B8.ByteString,Double)] |
              RDBHash [(B8.ByteString,B8.ByteString)] |
              RDBPair (Maybe Integer,B8.ByteString,RDBObj) |
              RDBDatabase Integer [RDBObj] |
              RDBNull |
              RDB [RDBObj] deriving (Show,Eq)

toZsetPairs :: [B8.ByteString] -> [(B8.ByteString,Double)]
toZsetPairs [] = []
toZsetPairs l = (x, read $ B8.unpack y) : toZsetPairs xs where
            (x:(y:[]),xs) = splitAt 2 l

getEncoding :: Word8 -> Word8
getEncoding = flip shift (-6) . (.&.) 0xC0

getSecondEncoding :: Word8 -> Word8
getSecondEncoding = flip shift (-4) . (.&.) 0x30

get6bitLen :: Word8 -> Integer
get6bitLen = fromIntegral . (.&.) 0x3f

get14bitLen :: Word16 -> Word16 -> Integer
get14bitLen f s = fromIntegral (shift (f .&. 0x3f) 8 .|. s) 

combineDistances :: Word16 -> Word16 -> Word16
combineDistances h l = shift h 8 .|. l

loadTimeMs :: Get Integer
loadTimeMs = do 
             time <- getWord64host
             return $ fromIntegral (fromIntegral time :: Int64)

loadTime :: Get Integer
loadTime = do 
             time <- getWord32host
             return $ fromIntegral (fromIntegral time :: Int64)

loadLen :: Get (Bool, Integer)
loadLen = do
          first <- getWord8
          case getEncoding first of
            0x00 -> return (False, get6bitLen first)
            0x01 -> do
              second <- getWord8
              return (False, get14bitLen (fromIntegral first) (fromIntegral second) )
            0x02 -> do
              len <- getWord32be
              return (False, fromIntegral len)
            0x03 -> return (True, get6bitLen first)

loadIntegerObj :: Integer -> Bool -> Get B8.ByteString
loadIntegerObj len enc = case len of 
                            0x00 -> do
                              str <- getWord8
                              return $ B8.pack $ show (fromIntegral str :: Int8) 
                            0x01 -> do
                              str <- getWord16le
                              return $ B8.pack $ show (fromIntegral str :: Int16)
                            0x02 -> do
                              str <- getWord32le
                              return $ B8.pack $ show (fromIntegral str :: Int32) 

loadDoubleValue :: Get Double
loadDoubleValue = do
                  len <- getWord8
                  case len of
                    0xff -> return $ read "-Infinity"
                    0xfe -> return $ read "Infinity"
                    0xfd -> return $ read "NaN"
                    l -> do
                      val <- getByteString (fromIntegral l)
                      return $ read $ B8.unpack val

loadLzfStr :: Get B8.ByteString
loadLzfStr = do
             (clenEnc, !clen) <- loadLen
             (lenEnc, !len) <- loadLen
             !str <- getLazyByteString (fromIntegral clen)
             return $ decompressLzfStr str

decompressLzfStr :: BL8.ByteString -> B8.ByteString
decompressLzfStr s = B8.concat $ BL8.toChunks str where
                     (Right str) = runGetLazy (parseLzf BL8.empty) s
                                
parseLzf :: BL8.ByteString -> Get BL8.ByteString
parseLzf decodedString = do
                         empty <- isEmpty
                         if empty then return BL8.empty else
                           do
                             h <- getWord8
                             let len = shift (h .&. 0xe0) (-5)
                             let high_d = h .&. 0x1f
                             obj <- case len of
                                      0 -> getLazyByteString ((fromIntegral high_d :: Int64) + 1)
                                      7 -> do
                                        new_len <- getWord8
                                        let full_len = (fromIntegral new_len :: Word16) + 7 + 2
                                        low_d <- getWord8
                                        let distance = combineDistances (fromIntegral high_d) (fromIntegral low_d)
                                        return $ repCopy decodedString distance full_len 
                                      l -> do
                                        low_d <- getWord8
                                        let distance = combineDistances (fromIntegral high_d) (fromIntegral low_d)
                                        return $ repCopy decodedString distance ((fromIntegral l :: Word16) + 2)
                             rest <- parseLzf (BL8.append decodedString obj)
                             return (BL8.append obj rest)

repCopy :: (Integral a) => BL8.ByteString -> a -> a -> BL8.ByteString
repCopy str dist len = BL8.take (fromIntegral len) (BL8.cycle window) where
                       window = BL8.drop (olen - fromIntegral dist - 1) str
                       olen = BL8.length str

loadStringObj :: Bool -> Get B8.ByteString
loadStringObj enc = do 
                    (isEncType,len) <- loadLen
                    if isEncType
                      then case len of
                             0x00 -> loadIntegerObj len enc
                             0x01 -> loadIntegerObj len enc
                             0x02 -> loadIntegerObj len enc
                             0x03 -> loadLzfStr
                      else getByteString (fromIntegral len)

loadListObj :: Get [B8.ByteString]
loadListObj = do
              (isEncType,len) <- loadLen
              replicateM (fromIntegral len) (loadStringObj True)

loadZSetPair :: Get (B8.ByteString, Double)
loadZSetPair = do
               value <- loadStringObj True
               weight <- loadDoubleValue
               return (value,weight)

loadZSetObj :: Get [(B8.ByteString,Double)]
loadZSetObj = do
              (isEncType,len) <- loadLen
              replicateM (fromIntegral len) loadZSetPair

loadZipListObj :: Get [B8.ByteString]
loadZipListObj = do
                 ziplen <- getWord32le
                 offset <- getWord32le
                 num_entries <- getWord16le
                 obj <- loadZipListMembers
                 eoz <- getWord8
                 return obj

loadZipListMembers :: Get [B8.ByteString]
loadZipListMembers = do
                     opc <- lookAhead getWord8
                     if opc == 0xff
                       then return []
                       else do
                            obj <- getZipListMember
                            rest <- loadZipListMembers
                            return (obj:rest)
                            
getZipListMember :: Get B8.ByteString
getZipListMember = do
                   prevLen <- getZipLen
                   header <- getWord8
                   case (getEncoding header, getSecondEncoding header) of
                     (0x00,_) -> do
                       let len = get6bitLen header
                       getByteString (fromIntegral len)
                     (0x01,_) -> do
                       second_part <- getWord8
                       let len = get14bitLen (fromIntegral header) (fromIntegral second_part)
                       getByteString (fromIntegral len)
                     (0x02,_) -> do
                       len <- getWord32be
                       getByteString (fromIntegral len)
                     (0x03,0x00) -> do
                       obj <- getWord16le
                       return $ B8.pack $ show (fromIntegral obj :: Int16)
                     (0x03,0x01) -> do
                       obj <- getWord32le
                       return $ B8.pack $ show (fromIntegral obj :: Int32)
                     (0x03,0x02) -> do
                       obj <- getWord64le
                       return $ B8.pack $ show (fromIntegral obj :: Int64)

getZipLen :: Get Integer
getZipLen = do
            prevLen <- getWord8
            case prevLen of

              0xfe -> do
                len <- getWord32le
                return (fromIntegral len)

              _ -> return (fromIntegral prevLen)

loadIntSetObj :: Get [B8.ByteString]
loadIntSetObj = do
                enc <- getWord32le
                setlen <- getWord32le
                replicateM (fromIntegral setlen) (loadIntSetMember enc)

loadIntSetMember :: Word32 -> Get B8.ByteString
loadIntSetMember enc = case enc of
                         0x02 -> do
                            obj <- getWord16le
                            return $ B8.pack $ show (fromIntegral obj :: Int16)
                         0x04 -> do
                            obj <- getWord32le
                            return $ B8.pack $ show (fromIntegral obj :: Int32)
                         0x08 -> do
                            obj <- getWord64le
                            return $ B8.pack $ show (fromIntegral obj :: Int64)
                         l -> do
                            obj <- trace (show enc) $ getWord8
                            return "1"

loadHashObj :: Get [(B8.ByteString,B8.ByteString)]
loadHashObj = do
              (isEncType,len) <- loadLen
              replicateM (fromIntegral len) loadHashMember

loadHashMember :: Get (B8.ByteString,B8.ByteString)
loadHashMember = do
                 key <- loadStringObj True
                 value <- loadStringObj True
                 return (key,value)

loadZipMapObj :: Get [(B8.ByteString,B8.ByteString)]
loadZipMapObj = do
                zmlen <- getWord8
                loadZipMapMembers

loadZipMapMembers :: Get [(B8.ByteString,B8.ByteString)]
loadZipMapMembers = do
                    is_eoz <- lookAhead getWord8
                    case is_eoz of
                      0xff -> do
                        skip 1
                        return []
                      _ -> do
                        obj <- loadZipMapMember
                        rest <- loadZipMapMembers
                        return (obj:rest)

loadZipMapMember :: Get (B8.ByteString,B8.ByteString)
loadZipMapMember = do
                   first_len <- getWord8
                   key_len <- getZipMapMemberLen first_len
                   key <- getByteString (fromIntegral key_len)
                   first_len_v <- getWord8
                   val_len <- getZipMapMemberLen first_len_v
                   free <- getWord8
                   val <- getByteString val_len
                   extra <- getByteString (fromIntegral free)
                   return (key,val)

getZipMapMemberLen :: Word8 -> Get Int
getZipMapMemberLen first_len = case first_len of 

                                 0xfd -> do
                                   l <- getWord32host
                                   return (fromIntegral l)

                                 0xfe -> return 0xfe
                                 
                                 l -> return (fromIntegral l)

loadObj :: Word8 -> Get RDBObj
loadObj t = case t of
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
                return (RDBZSet obj)
              -- ^ Load a hash value
              0x04 -> do
                obj <- loadHashObj
                return (RDBHash obj)
              -- ^ Load a zipmap encoded hash
              0x09 -> do
                binstr <- loadStringObj True 
                let (Right obj) = runGet loadZipMapObj binstr
                return (RDBHash obj)
              -- ^ Load a ziplist encoded list
              0x0a -> do
                binstr <- loadStringObj True 
                let (Right obj) = runGet loadZipListObj binstr
                return (RDBList obj)          
              -- ^ Load an intset encoded set
              0x0b -> do
                binstr <- loadStringObj True 
                let (Right obj) = runGet loadIntSetObj binstr
                return (RDBSet obj)
              -- ^ Load a ziplist encoded zset
              0x0c -> do
                binstr <- loadStringObj True 
                let (Right obj) = runGet loadZipListObj binstr
                return (RDBZSet $ toZsetPairs obj)

loadObjs :: Get [RDBObj]
loadObjs = do
           code <- lookAhead getWord8
           case code of
             0xfd -> do
               skip 1
               expire <- loadTime
               getPairs (Just expire)
             0xfc -> do
               skip 1
               expire <- loadTimeMs
               getPairs (Just expire)
             0xfe -> return []
             0xff -> return []
             _ -> getPairs Nothing

getPairs :: Maybe Integer -> Get [RDBObj]
getPairs ex = do
              t <- getWord8
              key <- loadStringObj False
              obj <- loadObj t
              rest <- loadObjs
              return (RDBPair (ex,key,obj):rest)

getPair :: Maybe Integer -> Get RDBObj
getPair ex = do
              t <- getWord8
              key <- loadStringObj False
              obj <- loadObj t
              return $ RDBPair (ex,key,obj)

getDBs :: Get [RDBObj]
getDBs = do
         opc <- lookAhead getWord8
         if opc == opcodeSelectdb
           then do
                skip 1
                (isEncType,dbnum) <- loadLen
                objs <- loadObjs
                rest <- getDBs
                return (objs ++ rest)
           else return [] 

getObjInc :: Get RDBObj
getObjInc = do
            opc <- lookAhead getWord8
            if opc == opcodeSelectdb
              then do
                   skip 1
                   (isEncType,dbnum) <- loadLen
                   getObjInc
              else do
                 code <- lookAhead getWord8
                 case code of
                   0xfd -> do
                     skip 1
                     expire <- loadTime
                     getPair (Just expire)
                   0xfc -> do
                     skip 1
                     expire <- loadTimeMs
                     getPair (Just expire)
                   0xfe -> do
                     return RDBNull
                   0xff -> do
                     skip 1
                     return RDBNull
                   _ -> getPair Nothing

{-main = do-}
       {-testf <- BL8.readFile "./dump.rdb"-}
       {-print $ show (decode testf :: RDBObj)-}

{-main = do-}
       {-testf <- BL8.readFile "./dump.rdb"-}
       {-runGet (processRDB_ printRDBObj)  testf-}

repParse !input (!st,Nothing) = case result of
                                   (Partial parser) -> (st,Just parser)
                                   (Done !res "") -> (res:st,Just (\input -> Done RDBNull input))
                                   (Done !res !leftover) -> repParse leftover (res:st,Nothing)
                                where
                                  !result = runGetPartial getObjInc input

repParse !input (!st,Just parser) = case result of
                                   (Partial parser) -> (st,Just parser)
                                   (Done !res "") -> (res:st,Just (\input -> Done RDBNull input))
                                   (Done !res !leftover) -> repParse leftover (res:st,Nothing)
                                  where
                                   !result = parser input

printRDB :: ResourceIO m => Sink B8.ByteString m ()
printRDB =
  sinkState Nothing
  pushRDB
  (\state -> return ())

pushRDB Nothing !input = do liftIO $ mapM_ (\x -> if x == RDBNull then return () else print x) st
                            return $ StateProcessing p
                            where
                              (Done r l) = runGetPartial (getBytes 9) input
                              (!st,!p) = repParse l ([],Nothing)

pushRDB (Just parser) !input = do liftIO $ mapM_ (\x -> if x == RDBNull then return () else print x) st
                                  return $ StateProcessing p
                                   where
                                     (!st,!p) = repParse input ([],Just parser)


main = do
       runResourceT $ C.sourceFile "./dump.rdb" $$ printRDB
        
