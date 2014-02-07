module JFBTerm.Util where

import Foreign
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek, sizeOf)
import Foreign.Marshal.Array (peekArray0)
import Foreign.C.Types
import Foreign.C.String
import Data.Char (ord)
import System.Posix.User (setUserID, setEffectiveUserID)
import System.Posix.Types (CUid)

data VirtualUID = VirtualUID {
  virtualRealUID :: CUid,
  virtualEffectiveUID :: CUid
  } deriving (Show, Eq)

instance Storable VirtualUID where
  sizeOf (VirtualUID ruid euid) = (sizeOf ruid) + (sizeOf euid)
  alignment a = 4
  peek a = do
      ruid <- peek ((castPtr a) :: Ptr CUid)
      euid <- peek ((castPtr (plusPtr a 4)) :: Ptr CUid)
      return (VirtualUID ruid euid)

foreign export ccall
  util_getuid :: Ptr VirtualUID -> IO CInt

foreign export ccall
  util_privilege_drop :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_search_string :: CString -> Ptr CString -> IO CInt

foreign export ccall
  remove_quote :: CString -> IO CString

-- | システムが現在、本来のrealユーザーIDとして考えてるIDを返す
getUID :: VirtualUID -> IO CUid
getUID (VirtualUID ruid euid) = return ruid

-- | getUID c interface
util_getuid :: Ptr VirtualUID -> IO CInt
util_getuid p = (peek p) >>= getUID >>= (\i -> return ((fromIntegral i) :: CInt))

-- | real, effectiveどちらのユーザーIDも、realユーザーIDを用いる状態に設定する
privilegeDrop :: VirtualUID -> IO ()
privilegeDrop (VirtualUID ruid euid) = setUserID ruid >> setEffectiveUserID 0 >> setEffectiveUserID euid

-- | privilegeDrop c interface
util_privilege_drop :: Ptr VirtualUID -> IO ()
util_privilege_drop p = (peek p) >>= privilegeDrop

-- | (void** p) -> [CString]
-- | p の最後は Null であること
convertPtrListToCStringList :: Ptr (Ptr a) -> IO [CString]
convertPtrListToCStringList p = peekArray0 nullPtr p >>= (return . castCStringList)
  where
    castCString s = (castPtr s) :: CString 
    castCStringList = map castCString

-- | (void** p) -> IO [String]
-- | p の最後は Nullであること
convertPtrListToStringList :: Ptr (Ptr a) -> IO [String]
convertPtrListToStringList p = convertPtrListToCStringList p >>= mapM peekCString

-- | 文字列を要素とする配列 s の各要素と文字列 t を比較し、一致したインデックスを返す
-- | 一致しなかった場合は -1 を返す
searchString :: String -> [String] -> Integer
searchString t s = searchString' 0 s
  where
    searchString' _ [] = -1
    searchString' n (x:xs)
      | x == t = n
      | otherwise = searchString' (n + 1) xs

-- | searchString c interface
util_search_string :: CString -> Ptr CString -> IO CInt
util_search_string t p = do
  s <- convertPtrListToStringList p
  t' <- peekCString t
  let i = searchString t' s in return ((fromInteger i) :: CInt)

-- | 文字列が " または ' で前後を囲まれてた場合、それを取り除く
-- | "ABC" -> ABC
-- | 'ABC' -> ABC
removeQuote :: String -> String
removeQuote [] = []
removeQuote (x:xs)
  | x == '\'' || x == '"' = removeQuote xs
  | otherwise = x : (removeQuote xs)

-- | removeQuote c interface
remove_quote :: CString -> IO CString
remove_quote s = peekCString s >>= (return . removeQuote) >>= newCString

