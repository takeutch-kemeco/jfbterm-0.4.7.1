-- | JFBTerm/Util.hs
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

module JFBTerm.Util (
  VirtualUID (
     virtualRealUID,      -- :: CUid
     virtualEffectiveUID  -- :: CUid
     ),
  
  privilegeInit, privilegeOn, privilegeOff, privilegeOpen, privilegeDrop,
  getUID, 
  convertPtrListToCStringList, convertPtrListToStringList, 
  searchString, removeQuote, limitInner, swapIntegral
  ) where

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Marshal.Array (peekArray0)
import Foreign.C.String (CString, newCString, peekCString)
import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID, getEffectiveUserID)
import System.Posix.Types (CUid, Fd)
import System.Posix.IO (openFd, defaultFileFlags, OpenMode(..))

data VirtualUID = VirtualUID {
  virtualRealUID      :: CUid,
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

-- | realユーザーIDと、effectiveユーザーIDを、本来の元の状態に設定する
privilegeOn :: VirtualUID -> IO ()
privilegeOn (VirtualUID ruid euid) = setreuid ((fromIntegral ruid) :: Int) ((fromIntegral euid) :: Int)

-- | realユーザーIDと、effectiveユーザーIDを、逆転した状態に設定する
privilegeOff :: VirtualUID -> IO ()
privilegeOff (VirtualUID ruid euid) = setreuid ((fromIntegral euid) :: Int) ((fromIntegral ruid) :: Int)

-- | ファイルをeffectiveユーザー権限で開く
privilegeOpen :: VirtualUID -> FilePath -> OpenMode -> IO Fd
privilegeOpen p filepath flags = do
  privilegeOn p
  fd <- openFd filepath flags Nothing defaultFileFlags
  privilegeOff p
  return fd

-- | システムが現在、本来のrealユーザーIDとして考えてるIDを返す
getUID :: VirtualUID -> IO CUid
getUID (VirtualUID ruid euid) = return ruid

-- | real, effectiveどちらのユーザーIDも、realユーザーIDを用いる状態に設定する
privilegeDrop :: VirtualUID -> IO ()
privilegeDrop (VirtualUID ruid euid) = setreuid ((fromIntegral ruid) :: Int) ((fromIntegral ruid) :: Int)

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

-- | 文字列が " または ' で前後を囲まれてた場合、それを取り除く
-- | "ABC" -> ABC
-- | 'ABC' -> ABC
removeQuote :: String -> String
removeQuote [] = []
removeQuote (x:xs)
  | x == '\'' || x == '"' = removeQuote xs
  | otherwise = x : (removeQuote xs)

-- | 数値を範囲a,b内に丸める
limitInner :: Integral a => a -> a -> a -> a
limitInner a min max
  | a < min = min
  | a > max = max
  | otherwise = a

-- | 変数の内容を交換する
swapIntegral :: Integral a => (a, a) -> (a, a)
swapIntegral (a, b) = (b, a)
