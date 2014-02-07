module JFBTerm.Util where

import Foreign
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek, sizeOf)
import Foreign.Marshal.Array (peekArray0)
import Foreign.C.Types
import Foreign.C.String
import Data.Char (ord)
import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID, getEffectiveUserID)
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
  poke a (VirtualUID ruid euid) = do
    poke ((castPtr a) :: Ptr CUid) ruid 
    poke ((castPtr (plusPtr a 4)) :: Ptr CUid) euid

foreign import ccall safe "setreuid"
  setreuid :: Int -> Int -> IO ()

foreign export ccall
  util_privilege_init :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_privilege_on :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_privilege_off :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_getuid :: Ptr VirtualUID -> IO CInt

foreign export ccall
  util_privilege_drop :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_search_string :: CString -> Ptr CString -> IO CInt

foreign export ccall
  remove_quote :: CString -> IO CString

-- | システム本来の{Real,Effective}UIDを得る
-- |
-- | TIPS:
-- | effective ユーザーIDが、実際の効果を発揮するユーザーIDで
-- | real ユーザーIDは、（アプリなどの）プロセスを起動した時点でのユーザーID
-- | プロセスが読み書きする際の、実際に効果がある権限としては effective の権限が用いられる。
-- | たとえば effective が非ルートユーザーIDならば、たとえ real がルートユーザーIDだとしても
-- | プロセスが読み書きする際の権限は非ルートユーザーまでとなる。
-- | もしも effective の権限自体をルートユーザーへと書き換えれば、
-- | プロセスが読み書きする際の権限もルートユーザーとなる。が、その書き変えのためには real が
-- | ルートユーザーでなければならない。
-- | real はそのため（effectiveの権限を書き換えても良いかの判断）に用いられるだけのもの。
-- | あくまで実際のアクセス権限は、effective のユーザーIDによって決まる。
privilegeInit :: IO VirtualUID
privilegeInit = do
    ruid <- getRealUserID
    euid <- getEffectiveUserID
    return (VirtualUID ruid euid)

-- | privilegeInit c interface
util_privilege_init :: Ptr VirtualUID -> IO ()
util_privilege_init p = privilegeInit >>= (poke p)

-- | realユーザーIDと、effectiveユーザーIDを、本来の元の状態に設定する
privilegeOn :: VirtualUID -> IO ()
privilegeOn (VirtualUID ruid euid) = setreuid ((fromIntegral ruid) :: Int) ((fromIntegral euid) :: Int)

util_privilege_on :: Ptr VirtualUID -> IO ()
util_privilege_on p = (peek p) >>= privilegeOn

-- | realユーザーIDと、effectiveユーザーIDを、逆転した状態に設定する
privilegeOff :: VirtualUID -> IO ()
privilegeOff (VirtualUID ruid euid) = setreuid ((fromIntegral euid) :: Int) ((fromIntegral ruid) :: Int)

util_privilege_off :: Ptr VirtualUID -> IO ()
util_privilege_off p = (peek p) >>= privilegeOff

-- | システムが現在、本来のrealユーザーIDとして考えてるIDを返す
getUID :: VirtualUID -> IO CUid
getUID (VirtualUID ruid euid) = return ruid

-- | getUID c interface
util_getuid :: Ptr VirtualUID -> IO CInt
util_getuid p = (peek p) >>= getUID >>= (\i -> return ((fromIntegral i) :: CInt))

-- | real, effectiveどちらのユーザーIDも、realユーザーIDを用いる状態に設定する
privilegeDrop :: VirtualUID -> IO ()
privilegeDrop (VirtualUID ruid euid) = setreuid ((fromIntegral ruid) :: Int) ((fromIntegral ruid) :: Int)

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

