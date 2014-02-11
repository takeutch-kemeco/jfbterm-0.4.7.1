-- | JFBTERM - skipagent-h.hs
-- | Copyright (c) 2014 Takeutch Kemeco (takeutchkemeco@gmail.com)
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

module JFBTerm.SkipAgent where

import Foreign
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek, sizeOf)
import Foreign.Marshal.Array (peekArray0)
import Foreign.C.Types
import Control.Concurrent
import Foreign.Ptr -- (FunPtr Ptr)
import Foreign.C.Types
import System.CPUTime.Rdtsc(rdtsc)

{--
foreign export ccall
  init_skip_agent :: Ptr CSkipAgentContext -> IO ()

foreign export ccall
  throw_skip_agent :: Ptr CSkipAgentContext -> (FunPtr CInt) -> (Ptr CChar) -> IO () 

foreign export ccall
  close_skip_agent :: Ptr CSkipAgentContext -> IO ()
--}

-- | Cコード側メモリー上に置かれてる SkipAgentContext のフォーマット
data SkipAgentContext = SkipAgentContext {
  useFlag      :: CInt,
  closeFlag    :: CInt,
  mutexFlag    :: CInt,
  orderFlag    :: CInt,
  startTime    :: CULLong,
  curTime      :: CULLong,
  prevTime     :: CULLong,
  functionPtr  :: FunPtr CInt,
  parameterPtr :: Ptr CChar
  } deriving (Show, Eq)

sizeCInt    = sizeOf (0 :: CInt)
sizeCULLong = sizeOf (0 :: CULLong)
sizeFunPtr  = sizeOf nullFunPtr
sizePtr     = sizeOf nullPtr

useFOffset       = 0
closeFOffset     = useFOffset      + sizeCInt
mutexFOffset     = closeFOffset    + sizeCInt
orderFOffset     = mutexFOffset    + sizeCInt
startTOffset     = orderFOffset    + sizeCInt
curTOffset       = startTOffset    + sizeCULLong
prevTOffset      = curTOffset      + sizeCULLong
functionPOffset  = prevTOffset     + sizeCULLong
parameterPOffset = functionPOffset + sizeFunPtr

instance Storable SkipAgentContext where
  sizeOf (SkipAgentContext uF cF mF oF sT cT pT fP pP) =
    (sizeOf uF) + (sizeOf cF) + (sizeOf mF) + (sizeOf oF) + 
    (sizeOf sT) + (sizeOf cT) + (sizeOf pT) + 
    (sizeOf fP) + (sizeOf pP)
  
  alignment a = maximum [sizeCInt, sizeCULLong, sizeFunPtr, sizePtr]
  
  peek a = do
    uF <- peek ((castPtr (plusPtr a useFOffset))       :: Ptr CInt)
    cF <- peek ((castPtr (plusPtr a closeFOffset))     :: Ptr CInt)
    mF <- peek ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt)
    oF <- peek ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt)
    sT <- peek ((castPtr (plusPtr a startTOffset))     :: Ptr CULLong)
    cT <- peek ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong)
    pT <- peek ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong)
    fP <- peek ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt))
    pP <- peek ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar))
    return (SkipAgentContext uF cF mF oF sT cT pT fP pP)
    
  poke a (SkipAgentContext uF cF mF oF sT cT pT fP pP) = do
    poke ((castPtr (plusPtr a useFOffset))       :: Ptr CInt) uF
    poke ((castPtr (plusPtr a closeFOffset))     :: Ptr CInt) cF
    poke ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt) mF
    poke ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt) oF    
    poke ((castPtr (plusPtr a startTOffset))     :: Ptr CULLong) sT
    poke ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong) cT
    poke ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong) pT    
    poke ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt)) fP
    poke ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar)) pP

-- | initSkipAgent c interface
init_skip_agent :: Ptr SkipAgentContext -> IO ()
init_skip_agent p = return ()

-- | throwSkipAgent c interface
throw_skip_agent :: Ptr SkipAgentContext -> (FunPtr CInt) -> (Ptr CChar) -> IO ()
throw_skip_agent p func param = peek p >>= (\c -> poke p (c {functionPtr = func, parameterPtr = param}))

close_skip_agent :: Ptr SkipAgentContext -> IO ()
close_skip_agent p = peek p >>= (\c -> poke p (c {closeFlag = 1}))

-- | unsigned long long 型を返す rdtsc
rdtscCULLong :: IO CULLong
rdtscCULLong = rdtsc >>= (return . fromIntegral)

-- | 使用中の CPU のクロック周波数の近似値を測定する
getCPUHz :: IO CULLong
getCPUHz = rdtscCULLong >>= (\a -> threadDelay 100000 >> rdtscCULLong >>= (\b -> return ((b - a) * 10)))

