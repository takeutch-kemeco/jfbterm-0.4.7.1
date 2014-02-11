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

foreign import ccall safe "exec_function"
  exec_function :: (FunPtr CInt) -> (Ptr CChar) -> IO ()

foreign export ccall
  init_skip_agent :: (Ptr SkipAgentContext) -> IO ()

foreign export ccall
  throw_skip_agent :: (Ptr SkipAgentContext) -> (FunPtr CInt) -> (Ptr CChar) -> IO ()

foreign export ccall
  close_skip_agent :: (Ptr SkipAgentContext) -> IO ()

-- | Cコード側メモリー上に置かれてる SkipAgentContext のフォーマット
data SkipAgentContext = SkipAgentContext {
  useFlag      :: CInt,
  closeFlag    :: CInt,
  mutexFlag    :: CInt,
  orderFlag    :: CInt,
  refreshFlag  :: CInt,
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
refreshFOffset   = orderFOffset    + sizeCInt
startTOffset     = refreshFOffset  + sizeCInt
curTOffset       = startTOffset    + sizeCULLong
prevTOffset      = curTOffset      + sizeCULLong
functionPOffset  = prevTOffset     + sizeCULLong
parameterPOffset = functionPOffset + sizeFunPtr

instance Storable SkipAgentContext where
  sizeOf (SkipAgentContext uF cF mF oF rF sT cT pT fP pP) =
    (sizeOf uF) + (sizeOf cF) + (sizeOf mF) + (sizeOf oF) + (sizeOf rF) +
    (sizeOf sT) + (sizeOf cT) + (sizeOf pT) +
    (sizeOf fP) + (sizeOf pP)

  alignment a = maximum [sizeCInt, sizeCULLong, sizeFunPtr, sizePtr]

  peek a = do
    uF <- peek ((castPtr (plusPtr a useFOffset))       :: Ptr CInt)
    cF <- peek ((castPtr (plusPtr a closeFOffset))     :: Ptr CInt)
    mF <- peek ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt)
    oF <- peek ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt)
    rF <- peek ((castPtr (plusPtr a refreshFOffset))   :: Ptr CInt)
    sT <- peek ((castPtr (plusPtr a startTOffset))     :: Ptr CULLong)
    cT <- peek ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong)
    pT <- peek ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong)
    fP <- peek ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt))
    pP <- peek ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar))
    return (SkipAgentContext uF cF mF oF rF sT cT pT fP pP)

  poke a (SkipAgentContext uF cF mF oF rF sT cT pT fP pP) = do
    poke ((castPtr (plusPtr a useFOffset))       :: Ptr CInt) uF
    poke ((castPtr (plusPtr a closeFOffset))     :: Ptr CInt) cF
    poke ((castPtr (plusPtr a mutexFOffset))     :: Ptr CInt) mF
    poke ((castPtr (plusPtr a orderFOffset))     :: Ptr CInt) oF
    poke ((castPtr (plusPtr a refreshFOffset))   :: Ptr CInt) rF
    poke ((castPtr (plusPtr a startTOffset))     :: Ptr CULLong) sT
    poke ((castPtr (plusPtr a curTOffset))       :: Ptr CULLong) cT
    poke ((castPtr (plusPtr a prevTOffset))      :: Ptr CULLong) pT
    poke ((castPtr (plusPtr a functionPOffset))  :: Ptr (FunPtr CInt)) fP
    poke ((castPtr (plusPtr a parameterPOffset)) :: Ptr (Ptr CChar)) pP

-- | initSkipAgent c interface
init_skip_agent :: (Ptr SkipAgentContext) -> IO ()
init_skip_agent p = forkIO (mainLoop p) >> return ()

throw_skip_agent :: (Ptr SkipAgentContext) -> (FunPtr CInt) -> (Ptr CChar) -> IO ()
throw_skip_agent p func param = do
  c <- peek p
  case (useFlag c) of
    0 -> exec_function func param
    otherwise -> case (closeFlag c) of
      0 -> poke p (c {orderFlag = 1, functionPtr = func, parameterPtr = param})
      otherwise -> return ()

close_skip_agent :: (Ptr SkipAgentContext) -> IO ()
close_skip_agent p = peek p >>= (\c -> poke p (c {closeFlag = 1, functionPtr = nullFunPtr}))

mainLoop :: (Ptr SkipAgentContext) -> IO ()
mainLoop p = do
  c <- peek p
  case (closeFlag c) of
    0 -> tryOrder c >>= tryRefresh >>= poke p >> wait >> mainLoop p
    otherwise -> return ()
  where
    loopTime = 4 * 1000
    wait = threadDelay loopTime

-- | 注文を試みる
tryOrder :: SkipAgentContext -> IO SkipAgentContext
tryOrder c
  | (orderFlag c) == 0 = return c
  | otherwise = do
    c' <- rdtscCULLong >>= (\t -> return c {curTime = t})
    c'' <- case isSkip c' of
      True -> return c'
      False -> return (updatePrevTime (c' {orderFlag = 0, refreshFlag = 1}))
    return (updatePrevTime c'')

-- | 更新を試みる
tryRefresh :: SkipAgentContext -> IO SkipAgentContext
tryRefresh c
  | (refreshFlag c) == 0 = return c
  | otherwise = do
    c' <- return c {refreshFlag = 0}
    wait
    case ((functionPtr c') /= nullFunPtr) && ((parameterPtr c') /= nullPtr) of
      True -> exec_function (functionPtr c') (parameterPtr c')
      False -> return ()
    return c'
  where
    refreshTime = 8 * 1000
    wait = threadDelay refreshTime

-- | スキップ条件判定
isSkip :: SkipAgentContext -> Bool
isSkip c
  | ((curTime c) - (prevTime c)) < boundTime =
    case ((curTime c) - (startTime c)) >= limitTime of
      True -> False
      False -> True
  | otherwise = False
  where
    boundTime = 8 * 1000 * 1000
    limitTime = 16 * 1000 * 1000

-- | prevTime の更新
updatePrevTime :: SkipAgentContext -> SkipAgentContext
updatePrevTime c = c {prevTime = (curTime c)}

-- | unsigned long long 型を返す rdtsc
rdtscCULLong :: IO CULLong
rdtscCULLong = rdtsc >>= (return . fromIntegral)

-- | 使用中の CPU のクロック周波数の近似値を測定する
getCPUHz :: IO CULLong
getCPUHz = rdtscCULLong >>= (\a -> threadDelay 100000 >> rdtscCULLong >>= (\b -> return ((b - a) * 10)))

