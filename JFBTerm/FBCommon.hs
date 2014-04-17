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
import Data.Bits ((.|.), (.&.), xor, complement)
import Foreign.C.Types -- (CInt, CUInt, CUChar)
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

tfbmTrueColor32Table = a ++ a
  where
    a = [0x000000, 0x8080ff, 0x80ff80, 0x80ffff, 0xff8080, 0xff80ff, 0xffff80, 0xffffff] :: [CUInt]

-- | xとyをスクリーンの回転設定に合わせて、右か左に回転移動。
-- | CW（クロックワイズ）が、首を時計回りに傾けて見るのに適した状態。
-- | CCW（カウンターCW）が、首を反時計回りに傾けて見るのに適した状態。
tfbmRotXY32BppPacked :: CUInt -> CUInt -> CUInt -> CUInt -> (CUInt, CUInt)
tfbmRotXY32BppPacked x y w h = (x, y)

tfbmSeekPixAdrs32BppPacked :: TFrameBufferMemory -> CUInt -> CUInt -> Ptr CUInt
tfbmSeekPixAdrs32BppPacked fbm x y = castPtr smem'
  where
    (x', y') = tfbmRotXY32BppPacked x y (tfbmWidth fbm) (tfbmHeight fbm) 
    i = fromIntegral ((y' * (tfbmBytePerLine fbm)) + (x' * 4)) :: Int
    smem' = plusPtr (tfbmSourceMemory fbm) i
  
tfbmSetPixel32BppPacked :: TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> IO ()
tfbmSetPixel32BppPacked fbm x y icol = do
  case x >= 0 && x < (tfbmWidth fbm) && y >= 0 && y < (tfbmHeight fbm) of
    True -> poke (tfbmSeekPixAdrs32BppPacked fbm x y) icol
    otherwise -> return ()

tfbmXOrPixel32BppPacked :: TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> IO ()
tfbmXOrPixel32BppPacked fbm x y icol = do
  let d = tfbmSeekPixAdrs32BppPacked fbm x y
  case x >= 0 && x < (tfbmWidth fbm) && y >= 0 && y < (tfbmHeight fbm) of
    True -> peek d >>= (\v -> poke d (xor v icol))
    otherwise -> return ()

tfbmFillRect32BppPacked :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
tfbmFillRect32BppPacked p xHead yHead xLen yLen colorIndex = do
  fbm <- peek p
  let xs = [xHead .. (xHead + xLen - 1)]
      ys = [yHead .. (yHead + yLen - 1)]
      ps = [(x, y) | y <- ys, x <- xs]
      icol = tfbmTrueColor32Table !! (fromIntegral colorIndex)
  mapM_ (\(x, y) -> tfbmSetPixel32BppPacked fbm x y icol) ps
  return ()

tfbmClearAll32BppPacked :: Ptr TFrameBufferMemory -> CUInt -> IO ()
tfbmClearAll32BppPacked p colorIndex = do
  fbm <- peek p
  let width  = tfbmWidth  fbm
      height = tfbmHeight fbm
  tfbmFillRect32BppPacked p 0 0 width height colorIndex

tfbmByteTo8Pix32BppPacked :: TFrameBufferMemory -> CUInt -> CUInt -> CUChar -> CUInt -> IO ()
tfbmByteTo8Pix32BppPacked fbm x y sb icol = do
  case (sb .&. 0x80) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 0) y icol
    otherwise -> return ()
  
  case (sb .&. 0x40) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 1) y icol
    otherwise -> return ()

  case (sb .&. 0x20) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 2) y icol
    otherwise -> return ()

  case (sb .&. 0x10) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 3) y icol
    otherwise -> return ()

  case (sb .&. 0x08) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 4) y icol
    otherwise -> return ()

  case (sb .&. 0x04) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 5) y icol
    otherwise -> return ()

  case (sb .&. 0x02) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 6) y icol
    otherwise -> return ()

  case (sb .&. 0x01) /= 0 of
    True -> tfbmSetPixel32BppPacked fbm (x + 7) y icol
    otherwise -> return ()

tfbmByteToXPix32BppPacked :: TFrameBufferMemory -> CUInt -> CUInt -> CUChar -> CUInt -> CUInt -> IO ()
tfbmByteToXPix32BppPacked fbm x y sb icol index = do
  case (index >= 7) && ((sb .&. 0x02) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 6) y icol
    otherwise -> return ()

  case (index >= 6) && ((sb .&. 0x04) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 5) y icol
    otherwise -> return ()
                              
  case (index >= 5) && ((sb .&. 0x08) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 4) y icol
    otherwise -> return ()
  
  case (index >= 4) && ((sb .&. 0x10) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 3) y icol
    otherwise -> return ()
  
  case (index >= 3) && ((sb .&. 0x20) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 2) y icol
    otherwise -> return ()

  case (index >= 2) && ((sb .&. 0x40) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 1) y icol
    otherwise -> return ()

  case (index >= 1) && ((sb .&. 0x80) /= 0) of
    True -> tfbmSetPixel32BppPacked fbm (x + 0) y icol
    otherwise -> return ()

tfbmOverlay32BppPacked :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
tfbmOverlay32BppPacked p xd yd ps lx ly gap colorIndex = do
  fbm <- peek p
  let ys = zip [yd .. ((yd + ly) - 1)] [0, (fromIntegral gap) ..]
      icol = tfbmTrueColor32Table !! (fromIntegral colorIndex)
  mapM_ (\(y, g) -> drawLine fbm xd lx y ps g icol) ys
  return ()
  where
    drawLine fbm xd lx y ps gap icol = do
      let xs = zip [xd, (xd + 8) .. (xd + lx)] [0..]
          lastLen = mod (xd + lx) 8
          lastPos = fst (last xs)
          lastPtr = snd (last xs)
          ff = 0xff :: CUChar
      mapM_ (\(x, i) -> peek (plusPtr ps (i + gap)) >>= (\v -> tfbmByteTo8Pix32BppPacked fbm x y v icol)) xs
      case lastLen /= 0 of
        True -> peek (plusPtr ps (lastPtr + gap)) >>= (\v -> tfbmByteToXPix32BppPacked fbm lastPos y v icol lastLen)
        otherwise -> return ()
      return ()

tfbmReverse32BppPacked :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
tfbmReverse32BppPacked p xHead yHead xLen yLen colorIndex = do
  fbm <- peek p
  let xs = [xHead .. (xHead + xLen)]
      ys = [yHead .. (yHead + yLen)]
      ps = [(x, y) | y <- ys, x <- xs]
      icol = tfbmTrueColor32Table !! (fromIntegral colorIndex)
  mapM_ (\(x, y) -> tfbmXOrPixel32BppPacked fbm x y icol) ps
  return ()

foreign export ccall
  tfbm_fill_rect_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt ->CUInt -> CUInt -> IO ()

foreign export ccall
  tfbm_overlay_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

foreign export ccall
  tfbm_clear_all_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> IO ()

foreign export ccall
  tfbm_reverse_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

tfbm_fill_rect_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt ->CUInt -> CUInt -> IO ()
tfbm_fill_rect_32bpp_packed p sx sy lx ly color = tfbmFillRect32BppPacked p sx sy lx ly color

tfbm_overlay_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> Ptr CUChar -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
tfbm_overlay_32bpp_packed p xd yd ps lx ly gap color = tfbmOverlay32BppPacked p xd yd ps lx ly gap color

tfbm_clear_all_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> IO ()
tfbm_clear_all_32bpp_packed p color = tfbmClearAll32BppPacked p color

tfbm_reverse_32bpp_packed :: Ptr TFrameBufferMemory -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
tfbm_reverse_32bpp_packed p sx sy lx ly color = tfbmReverse32BppPacked p sx sy lx ly color

