-- | pen-h.hs
-- | Copyright (C) 2014 Takeutch Kemeco (takeutchkemeco@gmail.com)
-- |
-- | JFBTERM -
-- | Copyright (C) 1999 Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
-- |
-- | KON2 - Kanji ON Console -
-- | Copyright (C) 1992-1996 Takashi MANABE (manabe@papilio.tutics.tut.ac.jp)
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

module JFBTerm.Pen where

--import Foreign
import Data.Bits
import Foreign.C.Types (CUChar, CUInt)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable
-- import Foreign.Marshal.Array (peekArray0)
--import Foreign.C.String
--import Data.Char (ord)
--import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID, getEffectiveUserID)
--import System.Posix.Types
--import System.Posix.IO (openFd, defaultFileFlags, OpenMode(..))

data TPen = TPen {
  tpenPrev :: (Ptr TPen),
  tpenX    :: CUChar,
  tpneY    :: CUChar,
  tpenBCol :: CUChar,
  tpenFCol :: CUChar,
  tpenAttr :: CUChar
  } deriving (Show, Eq)

sizePtr = sizeOf nullPtr
sizeCUChar = sizeOf (0 :: CUChar)

prevOffset = 0
xOffset    = prevOffset + sizePtr
yOffset    = xOffset    + sizeCUChar
bcolOffset = yOffset    + sizeCUChar
fcolOffset = bcolOffset + sizeCUChar
attrOffset = fcolOffset + sizeCUChar

instance Storable TPen where
  sizeOf (TPen prev x y bcol fcol attr) =
    (sizeOf prev) + (sizeOf x) + (sizeOf y) + (sizeOf bcol) + (sizeOf fcol) + (sizeOf attr)

  alignment a = maximum [sizePtr, sizeCUChar]

  peek a = do
    prev <- peek ((castPtr (plusPtr a prevOffset)) :: Ptr (Ptr TPen))
    x    <- peek ((castPtr (plusPtr a xOffset))    :: Ptr CUChar)
    y    <- peek ((castPtr (plusPtr a yOffset))    :: Ptr CUChar)
    bcol <- peek ((castPtr (plusPtr a bcolOffset)) :: Ptr CUChar)
    fcol <- peek ((castPtr (plusPtr a fcolOffset)) :: Ptr CUChar)
    attr <- peek ((castPtr (plusPtr a attrOffset)) :: Ptr CUChar)
    return (TPen prev x y bcol fcol attr)

  poke a (TPen prev x y bcol fcol attr) = do
    poke ((castPtr (plusPtr a prevOffset)) :: Ptr (Ptr TPen)) prev
    poke ((castPtr (plusPtr a xOffset))    :: Ptr CUChar)     x
    poke ((castPtr (plusPtr a yOffset))    :: Ptr CUChar)     y
    poke ((castPtr (plusPtr a bcolOffset)) :: Ptr CUChar)     bcol
    poke ((castPtr (plusPtr a fcolOffset)) :: Ptr CUChar)     fcol
    poke ((castPtr (plusPtr a attrOffset)) :: Ptr CUChar)     attr

class TerminalPen a where
  init            :: a
  offAllAttribute :: a -> a
  onHiLight       :: a -> a
  offHiLight      :: a -> a
  onUnderLine     :: a -> a
  offUnderLine    :: a -> a
  swapAttribute   :: a -> a
  onReverse       :: a -> a
  offReverse      :: a -> a
  setColor        :: a -> CUInt -> a

tpenAttrULine   = 0x80 -- ^ under line
tpenAttrReverse = 0x40 -- ^ reverse
tpenAttrHigh    = 0x20 -- ^ high

tpenLatchS = 0x00 -- ^ single byte char
tpenLatch1 = 0x20 -- ^ double byte char 1st byte
tpenLatch2 = 0x40 -- ^ double byte char 2nd byte

tpenCleanS   = 0x80
tpenCodeis1  = tpenLatch1
tpenCodeis2  = tpenLatch2
tpenLangCode = 0x0f

instance TerminalPen TPen where
  init = (TPen nullPtr 0 0 7 0 0)

  offAllAttribute a = a {tpenBCol = 0, tpenFCol = 7, tpenAttr = 0}

  onHiLight a = a {tpenAttr = attr', tpenFCol = fcol'}
    where
      attr' = (tpenAttr a) .|. tpenAttrHigh
      fcol' = (tpenFCol a) .|. 0x08

  offHiLight a = a {tpenAttr = attr', tpenFCol = fcol'}
    where
      attr' = (tpenAttr a) .&. (complement tpenAttrHigh)
      fcol' = (tpenFCol a) .&. (complement 0x08)

  onUnderLine a = a {tpenAttr = attr', tpenBCol = bcol'}
    where
      attr' = (tpenAttr a) .|. tpenAttrULine
      bcol' = (tpenBCol a) .|. 0x08

  offUnderLine a = a {tpenAttr = attr', tpenBCol = bcol'}
    where
      attr' = (tpenAttr a) .&. (complement tpenAttrULine)
      bcol' = (tpenAttr a) .&. (complement 0x08)

  swapAttribute a = a {tpenFCol = fcol'', tpenBCol = bcol''}
    where
      fcol' = (tpenBCol a) .&. 0x07
      fcol''
        | (((tpenAttr a) .&. tpenAttrHigh) /= 0) && (fcol' /= 0) = fcol' .|. 0x08
        | otherwise = fcol'
      bcol' = (tpenFCol a) .&. 0x07
      bcol''
        | (((tpenAttr a) .&. tpenAttrULine) /= 0) = bcol' .|. 0x08
        | otherwise = bcol'

  onReverse a = swapAttribute (a {tpenAttr = attr'})
    where
      attr = (tpenAttr a)
      attr'
        | ((attr .&. tpenAttrReverse) == 0) = attr .|. tpenAttrReverse
        | otherwise = attr

  offReverse a = swapAttribute (a {tpenAttr = attr'})
    where
      attr = (tpenAttr a)
      attr'
        | ((attr .&. tpenAttrReverse) /= 0) = attr .&. (complement tpenAttrReverse)
        | otherwise = attr

  setColor a col = f a (fromIntegral col)
    where
      tcol = [0, 4, 2, 6, 1, 5, 3, 7] :: [CUChar]
      f a col
        | (col >= 30) && (col <= 37) = setColor' a (tcol !! col) tpenAttrHigh  tpenAttrULine
        | (col >= 40) && (col <= 47) = setColor' a (tcol !! col) tpenAttrULine tpenAttrHigh
      attr = (tpenAttr a)
      setColor' a col normalAttr reverseAttr
        | ((attr .&. tpenAttrReverse) /= 0) = case ((attr .&. reverseAttr) /= 0) of
          True ->  a {tpenBCol = (col .|. 0x08)}
          False -> a {tpenBCol = col}
