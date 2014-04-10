-- | JFBTerm/FBCommon.hs
-- | Copyright (C) 2014 Takeutch Kemeco (takeutchkemeco@gmail.com)
-- |
-- | JFBTERM -
-- | Copyright (C) 1999  Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
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

module JFBTerm.FBCommon (
  JFBTerm.FBCommon.FrameBufferCapability (
     fill,    -- :: a -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO a
     overlay, -- :: a -> CUInt -> CUInt -> (Ptr CUChar) -> CUInt -> CUInt -> CUInt -> CUInt -> IO a
     clear,   -- :: a -> CUInt -> IO a
     reverse  -- :: a -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO a
     ),
  
  TFrameBufferMemory (
    tfbmHeight,       -- CUInt
    tfbmWidth,        -- CUInt
    tfbmBytePerLine,  -- CUInt
    tfbmFileHandle,   -- CInt
    tfbmSourceStart,  -- CUInt
    tfbmSourceOffset, -- CUInt
    tfbmSourceLength, -- CUInt
    tfbmMemoryStart,  -- CUInt
    tfbmMemoryOffset, -- CUInt
    tfbmMemoryLength, -- CUint
    tfbmSourceMemory, -- (Ptr CUChar)
    tfbmMemoryMapIO,  -- (Ptr CUChar)
    tfbmTTYFileHandle -- CInt
    )
  ) where

import Foreign.Storable (Storable(..))
import Data.Bits ((.|.), (.&.), complement)
import Foreign.C.Types (CInt, CUInt, CUChar)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)

data TFrameBufferMemory = TFrameBufferMemory {
  tfbmHeight            :: CUInt,
  tfbmWidth             :: CUInt,
  tfbmBytePerLine       :: CUInt,
  tfbmFileHandle        :: CInt,
  tfbmSourceStart       :: CUInt,
  tfbmSourceOffset      :: CUInt,
  tfbmSourceLength      :: CUInt,
  tfbmMemoryStart       :: CUInt,
  tfbmMemoryOffset      :: CUInt,
  tfbmMemoryLength      :: CUInt,
  tfbmSourceMemory      :: (Ptr CUChar),
  tfbmMemoryMapIO       :: (Ptr CUChar),
  tfbmTTYFileHandle     :: CInt
  } deriving (Show, Eq)

class FrameBufferCapability a where
  fill    :: a -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO a
  overlay :: a -> CUInt -> CUInt -> (Ptr CUChar) -> CUInt -> CUInt -> CUInt -> CUInt -> IO a
  clear   :: a -> CUInt -> IO a
  reverse :: a -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO a

instance FrameBufferCapability TFrameBufferMemory where
  fill a sx sy lx ly color = return a
  overlay a xd yd ps lx ly gap color = return a
  clear a color = return a
  reverse a sx sy lx ly color = return a

sizeCInt   = sizeOf (0 :: CInt)
sizeCUInt  = sizeOf (0 :: CUInt)
sizeCUChar = sizeOf (0 :: CUChar)
sizePtr    = sizeOf nullPtr

tfbmHeightOffset        = 0
tfbmWidthOffset         = tfbmHeightOffset       + sizeCUInt
tfbmBytePerLineOffset   = tfbmWidthOffset        + sizeCUInt
tfbmFileHandleOffset    = tfbmBytePerLineOffset  + sizeCUInt
tfbmSourceStartOffset   = tfbmFileHandleOffset   + sizeCInt
tfbmSourceOffsetOffset  = tfbmSourceStartOffset  + sizeCUInt      
tfbmSourceLengthOffset  = tfbmSourceOffsetOffset + sizeCUInt
tfbmMemoryStartOffset   = tfbmSourceLengthOffset + sizeCUInt       
tfbmMemoryOffsetOffset  = tfbmMemoryStartOffset  + sizeCUInt      
tfbmMemoryLengthOffset  = tfbmMemoryOffsetOffset + sizeCUInt      
tfbmSourceMemoryOffset  = tfbmMemoryLengthOffset + sizeCUInt      
tfbmMemoryMapIOOffset   = tfbmSourceMemoryOffset + sizePtr       
tfbmTTYFileHandleOffset = tfbmMemoryMapIOOffset  + sizePtr

instance Storable TFrameBufferMemory where
  sizeOf (TFrameBufferMemory h w bp fh ss so sl ms mo ml sm mm tf) =
    (sizeOf w) + (sizeOf h) + (sizeOf bp) + (sizeOf fh) +
    (sizeOf ss) + (sizeOf so) + (sizeOf sl) + 
    (sizeOf ms) + (sizeOf mo) + (sizeOf ml) + 
    (sizeOf sm) + (sizeOf mm) + (sizeOf tf)

  alignment a = maximum [sizePtr, sizeCInt, sizeCUInt, sizeCUChar]

  peek a = do
    h  <- peek ((castPtr (plusPtr a tfbmHeightOffset))        :: Ptr CUInt)
    w  <- peek ((castPtr (plusPtr a tfbmWidthOffset))         :: Ptr CUInt)
    bp <- peek ((castPtr (plusPtr a tfbmBytePerLineOffset))   :: Ptr CUInt)
    fh <- peek ((castPtr (plusPtr a tfbmFileHandleOffset))    :: Ptr CInt)
    ss <- peek ((castPtr (plusPtr a tfbmSourceStartOffset))   :: Ptr CUInt)
    so <- peek ((castPtr (plusPtr a tfbmSourceOffsetOffset))  :: Ptr CUInt)
    sl <- peek ((castPtr (plusPtr a tfbmSourceLengthOffset))  :: Ptr CUInt)
    ms <- peek ((castPtr (plusPtr a tfbmMemoryStartOffset))   :: Ptr CUInt)
    mo <- peek ((castPtr (plusPtr a tfbmMemoryOffsetOffset))  :: Ptr CUInt)
    ml <- peek ((castPtr (plusPtr a tfbmMemoryLengthOffset))  :: Ptr CUInt)
    sm <- peek ((castPtr (plusPtr a tfbmSourceMemoryOffset))  :: Ptr (Ptr CUChar))
    mm <- peek ((castPtr (plusPtr a tfbmMemoryMapIOOffset))   :: Ptr (Ptr CUChar))
    tf <- peek ((castPtr (plusPtr a tfbmTTYFileHandleOffset)) :: Ptr CInt)
    return (TFrameBufferMemory h w bp fh ss so sl ms mo ml sm mm tf)
  
  poke a (TFrameBufferMemory h w bp fh ss so sl ms mo ml sm mm tf) = do
    poke ((castPtr (plusPtr a tfbmHeightOffset))        :: Ptr CUInt)        h
    poke ((castPtr (plusPtr a tfbmWidthOffset))         :: Ptr CUInt)        w
    poke ((castPtr (plusPtr a tfbmBytePerLineOffset))   :: Ptr CUInt)        bp
    poke ((castPtr (plusPtr a tfbmFileHandleOffset))    :: Ptr CInt)         fh
    poke ((castPtr (plusPtr a tfbmSourceStartOffset))   :: Ptr CUInt)        ss
    poke ((castPtr (plusPtr a tfbmSourceOffsetOffset))  :: Ptr CUInt)        so
    poke ((castPtr (plusPtr a tfbmSourceLengthOffset))  :: Ptr CUInt)        sl
    poke ((castPtr (plusPtr a tfbmMemoryStartOffset))   :: Ptr CUInt)        ms
    poke ((castPtr (plusPtr a tfbmMemoryOffsetOffset))  :: Ptr CUInt)        mo
    poke ((castPtr (plusPtr a tfbmMemoryLengthOffset))  :: Ptr CUInt)        ml
    poke ((castPtr (plusPtr a tfbmSourceMemoryOffset))  :: Ptr (Ptr CUChar)) sm
    poke ((castPtr (plusPtr a tfbmMemoryMapIOOffset))   :: Ptr (Ptr CUChar)) mm
    poke ((castPtr (plusPtr a tfbmTTYFileHandleOffset)) :: Ptr CInt)         tf
