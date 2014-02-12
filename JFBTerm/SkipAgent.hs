-- | JFBTerm/SkipAgent.hs
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

import Foreign.Ptr (Ptr, FunPtr, nullPtr, nullFunPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types
import Control.Concurrent (forkIO, threadDelay)
import System.CPUTime.Rdtsc(rdtsc)

foreign import ccall safe "exec_c_function"
  exec_c_function :: (FunPtr CInt) -> (Ptr CChar) -> IO ()

foreign import ccall safe "atomic_compare_swap"
  atomic_compare_swap :: (Ptr CInt) -> CInt -> CInt -> IO ()

foreign export ccall
  init_skip_agent :: (Ptr SkipAgentContext) -> IO ()

foreign export ccall
  throw_skip_agent :: (Ptr SkipAgentContext) -> (FunPtr CInt) -> (Ptr CChar) -> IO ()

foreign export ccall
  close_skip_agent :: (Ptr SkipAgentContext) -> IO ()

-- | Cコード側メモリー上に置かれてる SkipAgentContext のフォーマット
data SkipAgentContext = SkipAgentContext {
  useFlag      :: CInt,
  runFlag      :: CInt,
  orderFlag    :: CInt,
  mutexFlag    :: CInt,
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
runFOffset       = useFOffset      + sizeCInt
orderFOffset     = runFOffset      + sizeCInt
mutexFOffset     = orderFOffset    + sizeCInt
curTOffset       = mutexFOffset    + sizeCInt
prevTOffset      = curTOffset      + sizeCULLong
functionPOffset  = prevTOffset     + sizeCULLong
parameterPOffset = functionPOffset + sizeFunPtr

instance Storable SkipAgentContext where
  sizeOf (SkipAgentContext uF rF oF mF cT pT fP pP) =
    (sizeOf uF) + (sizeOf rF) + (sizeOf oF) + (sizeOf mF) + (sizeOf cT) + (sizeOf pT) + (sizeOf fP) + (sizeOf pP)

  alignment a = maximum [sizeCInt, sizeCULLong, sizeFunPtr, sizePtr]

  peek a = do
    uF <- peek ((castPtr (plusPtr a useFOffset))       :: Ptr CInt)
    rF <- peek ((castPtr (plusPtr a runFOffset))       :: Ptr CInt)
    oF <- peek ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt)
    mF <- peek ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt)
    cT <- peek ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong)
    pT <- peek ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong)
    fP <- peek ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt))
    pP <- peek ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar))
    return (SkipAgentContext uF rF oF mF cT pT fP pP)

  poke a (SkipAgentContext uF rF oF mF cT pT fP pP) = do
    poke ((castPtr (plusPtr a useFOffset))       :: Ptr CInt)          uF
    poke ((castPtr (plusPtr a runFOffset))       :: Ptr CInt)          rF
    poke ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt)          oF
    poke ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt)          mF
    poke ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong)       cT
    poke ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong)       pT
    poke ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt)) fP
    poke ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar))   pP

init_skip_agent :: (Ptr SkipAgentContext) -> IO ()
init_skip_agent p = peek p >>= (\c -> poke p (c {runFlag = 1})) >> forkIO (mainLoop p) >> return ()

close_skip_agent :: (Ptr SkipAgentContext) -> IO ()
close_skip_agent p = peek p >>= (\c -> poke p (c {runFlag = 0}))

throw_skip_agent :: (Ptr SkipAgentContext) -> (FunPtr CInt) -> (Ptr CChar) -> IO ()
throw_skip_agent p func param = do
  c <- peek p
  case (useFlag c) of
    0 -> exec_c_function func param
    otherwise -> do
      lockMutex p mutexID
      poke p (c {functionPtr = func, parameterPtr = param, orderFlag = 1})
      unlockMutex p mutexID
  where
    mutexID = 2

loopTime = 4 * 1000
boundTime = 16 * 1000 * 1000

mainLoop :: (Ptr SkipAgentContext) -> IO ()
mainLoop p = do
  c <- peek p 
  case (useFlag c) of
    0 -> return ()
    otherwise -> transientLoop p

transientLoop :: (Ptr SkipAgentContext) -> IO ()
transientLoop p = do
  c <- peek p
  case (runFlag c) of
    0 -> return ()
    otherwise -> transientLoopB p
  threadDelay loopTime
  transientLoop p

transientLoopB :: (Ptr SkipAgentContext) -> IO ()
transientLoopB p = do
  c <- peek p
  t <- rdtscCULLong
  poke p (c {curTime = t})
  case (orderFlag c) of
    0 -> return ()
    otherwise -> transientLoopC p

transientLoopC :: (Ptr SkipAgentContext) -> IO ()
transientLoopC p = do
  c <- peek p
  case ((curTime c) - (prevTime c)) > boundTime of
    True -> transientLoopD p
    False -> return ()

transientLoopD :: (Ptr SkipAgentContext) -> IO ()
transientLoopD p = do
  lockMutex p mutexID
  c <- peek p
  exec_c_function (functionPtr c) (parameterPtr c)
  poke p (c {orderFlag = 0})
  unlockMutex p mutexID
  peaceLoop p
  c' <- peek p
  poke p (c' {prevTime = (curTime c')})
  where
    mutexID = 1

peaceLoop :: (Ptr SkipAgentContext) -> IO ()
peaceLoop p = do
  c <- peek p
  case ((runFlag c) /= 0) && ((orderFlag c) == 0) of
    True -> threadDelay loopTime >> peaceLoop p
    False -> return ()

-- | ユニークIDによってミューテックスを取得する
-- | ミューテックスを取得したフID主は、責任を持って必ず unlockMutex で抜ける必要がある
-- | これらMutex関数内のマジックナンバー 3 は、Cソース側の構造体メンバーアクセスに名前による抽象的な指定を行えない為
lockMutex :: (Ptr SkipAgentContext) -> CInt -> IO ()
lockMutex p id = do
  atomic_compare_swap (plusPtr p (sizeCInt * 3)) 0 id
  c <- peek p
  case (mutexFlag c) == id of
    True -> return ()
    False -> lockMutex p id

unlockMutex :: (Ptr SkipAgentContext) -> CInt -> IO ()
unlockMutex p id = do
  atomic_compare_swap (plusPtr p (sizeCInt * 3)) id 0
  c <- peek p
  case (mutexFlag c) == 0 of
    True -> return ()
    False -> unlockMutex p id

-- | unsigned long long 型を返す rdtsc
rdtscCULLong :: IO CULLong
rdtscCULLong = rdtsc >>= (return . fromIntegral)

-- | 使用中の CPU のクロック周波数の近似値を測定する
getCPUHz :: IO CULLong
getCPUHz = rdtscCULLong >>= (\a -> threadDelay 100000 >> rdtscCULLong >>= (\b -> return ((b - a) * 10)))
