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
import Control.Concurrent.STM
import Foreign.Ptr -- (FunPtr Ptr)
import Foreign.C.Types
import System.CPUTime.Rdtsc(rdtsc)

foreign export ccall
  init_skip_agent :: Ptr CSkipAgentContext -> IO ()

foreign export ccall
  throw_skip_agent :: Ptr CSkipAgentContext -> (FunPtr CInt) -> (Ptr CChar) -> IO () 

foreign export ccall
  close_skip_agent :: Ptr CSkipAgentContext -> IO ()

-- | Cコード側メモリー上に置かれてる SkipAgentContext のフォーマット
data CSkipAgentContext = CSkipAgentContext {
  c_useFlag      :: CInt,
  c_closeFlag    :: CInt,
  c_mutexFlag    :: CInt,
  c_functionPtr  :: FunPtr CInt,
  c_parameterPtr :: Ptr CChar
  } deriving (Show, Eq)

sizeCInt    = sizeOf (0 :: CInt)
sizeFunPtr  = sizeOf nullFunPtr
sizePtr     = sizeOf nullPtr

c_useOffset       = 0
c_closeOffset     = c_useOffset       + sizeCInt
c_mutexOffset     = c_closeOffset     + sizeCInt
c_functionOffset  = c_mutexOffset     + sizeCInt
c_parameterOffset = c_functionOffset  + sizeFunPtr

instance Storable CSkipAgentContext where
  sizeOf (CSkipAgentContext use close mutex func param) =
    (sizeOf use) + (sizeOf close) + (sizeOf mutex) + (sizeOf func) + (sizeOf param)
  alignment a = sizeFunPtr
  peek a = do
    use   <- peek ((castPtr (plusPtr a c_useOffset))       :: Ptr CInt)
    close <- peek ((castPtr (plusPtr a c_closeOffset))     :: Ptr CInt)
    mutex <- peek ((castPtr (plusPtr a c_mutexOffset))     :: Ptr CInt)
    func  <- peek ((castPtr (plusPtr a c_functionOffset))  :: Ptr (FunPtr CInt))
    param <- peek ((castPtr (plusPtr a c_parameterOffset)) :: Ptr (Ptr CChar))   
    return (CSkipAgentContext use close mutex func param)
  poke a (CSkipAgentContext use close mutex func param) = do
    poke ((castPtr (plusPtr a c_useOffset))       :: Ptr CInt) use
    poke ((castPtr (plusPtr a c_closeOffset))     :: Ptr CInt) close
    poke ((castPtr (plusPtr a c_mutexOffset))     :: Ptr CInt) mutex
    poke ((castPtr (plusPtr a c_functionOffset))  :: Ptr (FunPtr CInt)) func
    poke ((castPtr (plusPtr a c_parameterOffset)) :: Ptr (Ptr CChar)) param

-- | initSkipAgent c interface
init_skip_agent :: Ptr CSkipAgentContext -> IO ()
init_skip_agent p = initSkipAgent c
  where
    c = SkipAgentContext {
      useFlag = False,
      closeFlag = False,
      mutexFlag = False,
      orderFlag = False,    
      startTime = 0,   
      curTime = 0,      
      prevTime = 0,
      functionPtr = nullFunPtr,
      parameterPtr = nullPtr,
      cContext = p
      }

-- | throwSkipAgent c interface
throw_skip_agent :: Ptr CSkipAgentContext -> (FunPtr CInt) -> (Ptr CChar) -> IO ()
throw_skip_agent p func param = peek p >>= (\c -> poke p (c {c_functionPtr = func, c_parameterPtr = param}))

close_skip_agent :: Ptr CSkipAgentContext -> IO ()
close_skip_agent p = peek p >>= (\c -> poke p (c {c_closeFlag = 1}))

initSkipAgent _ = return ()

-- | Haskellコード側での SkipAgent 処理のコンテキスト
data SkipAgentContext = SkipAgentContext {
  useFlag      :: Bool,
  closeFlag    :: Bool,
  mutexFlag    :: Bool,
  orderFlag    :: Bool,
  startTime    :: Integer,
  curTime      :: Integer,
  prevTime     :: Integer,
  functionPtr  :: FunPtr CInt,
  parameterPtr :: Ptr CChar,
  cContext     :: Ptr CSkipAgentContext
  } deriving (Show, Eq)

-- | cContext に対応する要素を更新する
updateSkipAgentContext :: SkipAgentContext -> IO SkipAgentContext 
updateSkipAgentContext c = do
  cc <- peek (cContext c)
  return c {
    useFlag      = (c_useFlag cc)   /= 0,
    closeFlag    = (c_closeFlag cc) /= 0,
    mutexFlag    = (c_mutexFlag cc) /= 0,
    functionPtr  = (c_functionPtr cc),
    parameterPtr = (c_parameterPtr cc)
    }

-- | 対応する要素を cContext へ書き込む
synchronizeSkipAgentContext :: SkipAgentContext -> IO ()
synchronizeSkipAgentContext c = do
  let use   = case (useFlag c)   of {True -> 1; False -> 0}
      close = case (closeFlag c) of {True -> 1; False -> 0}
      mutex = case (mutexFlag c) of {True -> 1; False -> 0}
      func  = (functionPtr c)
      param = (parameterPtr c)
  poke (cContext c) (CSkipAgentContext use close mutex func param)

-- | Integer 型を返す rdtsc
rdtscInteger :: IO Integer
rdtscInteger = rdtsc >>= return . toInteger

-- | 使用中の CPU のクロック周波数の近似値を測定する
getCPUHz :: IO Integer
getCPUHz = rdtscInteger >>= (\a -> threadDelay 10000 >> rdtscInteger >>= (\b -> return ((b - a) * 10)))

