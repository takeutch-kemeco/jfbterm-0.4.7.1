module JFBTerm.Util where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Char (ord)

foreign export ccall
  remove_quote :: CString -> IO CString

-- | 文字列が " または ' で前後を囲まれてた場合、それを取り除く。
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

