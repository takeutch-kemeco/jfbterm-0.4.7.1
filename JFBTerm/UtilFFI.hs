-- | JFBTerm/UtilFFI.hs
-- | Copyright (c) 2014 Takeutch Kemeco (takeutchkemeco@gmail.com)
-- |
-- | JFBTERM -
-- | Copyright (c) 2003 Fumitoshi UKAI <ukai@debian.or.jp>
-- | Copyright (c) 1999 Noritoshi Masuichi (nmasu@ma3.justnet.ne.jp)
-- |
-- | Redistribution and use in source and binary forms, with or without
-- | modification, are permitted provided that the following conditions
-- | are met:
-- | 1. Redistributions of source code must retain the above copyright
-- |    notice, this list of conditions and the following disclaimer.
-- | 2. Redistributions in binary form must reproduce the above copyright
-- |    notice, this list of conditions and the following disclaimer in the
-- |    documentation and/or other materials provided with the distribution.
-- |
-- | THIS SOFTWARE IS PROVIDED BY NORITOSHI MASUICHI ``AS IS'' AND ANY
-- | EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- | IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- | ARE DISCLAIMED.  IN NO EVENT SHALL THE TERRENCE R. LAMBERT BE LIABLE
-- | FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- | DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- | HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- | LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- | OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- | SUCH DAMAGE.

module JFBTerm.UtilFFI where

import JFBTerm.Util
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String (CString, newCString, peekCString)
import System.Posix.IO (openFd, defaultFileFlags, OpenMode(..))

foreign export ccall
  util_privilege_init :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_privilege_on :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_privilege_off :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_privilege_open :: Ptr VirtualUID -> CString -> CInt -> IO CInt

foreign export ccall
  util_getuid :: Ptr VirtualUID -> IO CInt

foreign export ccall
  util_privilege_drop :: Ptr VirtualUID -> IO ()

foreign export ccall
  util_search_string :: CString -> Ptr CString -> IO CInt

foreign export ccall
  remove_quote :: CString -> IO CString

foreign export ccall
  limit_inner :: CInt -> CInt -> CInt -> IO CInt

foreign export ccall
  swap_int :: Ptr CInt -> Ptr CInt -> IO ()

-- | privilegeInit c interface
util_privilege_init :: Ptr VirtualUID -> IO ()
util_privilege_init p = privilegeInit >>= (poke p)

-- | privilegeOn c interface
util_privilege_on :: Ptr VirtualUID -> IO ()
util_privilege_on p = (peek p) >>= privilegeOn

-- | privilegeOff c interface
util_privilege_off :: Ptr VirtualUID -> IO ()
util_privilege_off p = (peek p) >>= privilegeOff

-- | privilegeOpen c interface
util_privilege_open :: Ptr VirtualUID -> CString -> CInt -> IO CInt
util_privilege_open p filepath flags = do
  p' <- peek p
  filepath' <- (peekCString filepath) :: IO FilePath
  let flags' = case flags of {0 -> ReadOnly; 1 -> WriteOnly; 2 -> ReadWrite}
  i <- privilegeOpen p' filepath' flags'
  return ((fromIntegral i) :: CInt)

-- | getUID c interface
util_getuid :: Ptr VirtualUID -> IO CInt
util_getuid p = (peek p) >>= getUID >>= (\i -> return ((fromIntegral i) :: CInt))

-- | privilegeDrop c interface
util_privilege_drop :: Ptr VirtualUID -> IO ()
util_privilege_drop p = (peek p) >>= privilegeDrop

-- | searchString c interface
util_search_string :: CString -> Ptr CString -> IO CInt
util_search_string t p = do
  s <- convertPtrListToStringList p
  t' <- peekCString t
  let i = searchString t' s in return ((fromInteger i) :: CInt)

-- | removeQuote c interface
remove_quote :: CString -> IO CString
remove_quote s = peekCString s >>= (return . removeQuote) >>= newCString

-- | limitInner c interface
limit_inner :: CInt -> CInt -> CInt -> IO CInt
limit_inner a min max = return ((fromIntegral a') :: CInt)
  where
    a' = limitInner a min max

-- | swapIntegral c interface
swap_int :: Ptr CInt -> Ptr CInt -> IO ()
swap_int a b = do
    a' <- (peek a)
    b' <- (peek b)
    let t@(a'', b'') = swapIntegral (a', b')
    poke a a''
    poke b b''
