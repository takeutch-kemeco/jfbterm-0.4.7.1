module JFBTerm.Util where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Char (ord)
import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID)

foreign export ccall
  util_search_string :: CString -> Ptr CString -> IO CInt

foreign export ccall
  remove_quote :: CString -> IO CString

foreign export ccall
  util_privilege_drop :: CInt -> IO ()

-- | real, effectiveどちらのユーザーIDも、realユーザーIDを用いる状態に設定する
privilegeDrop :: IO ()
privilegeDrop = getRealUserID >>= (\ruid -> setUserID ruid >> setEffectiveUserID ruid)

-- | privilegeDrop c interface
util_privilege_drop :: CInt -> IO ()
util_privilege_drop _ = return ()

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

-- | util_search_string c interface
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

