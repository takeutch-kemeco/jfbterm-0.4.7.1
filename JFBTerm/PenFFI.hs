-- | JFBTerm/PenFFI.hs
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

module JFBTerm.PenFFI where

import JFBTerm.Pen
import Foreign.Storable (Storable(..))
import Foreign.C.Types -- (CUInt)
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall safe "free"
  c_free :: Ptr TPen -> IO ()

foreign export ccall
  tpen_init :: Ptr TPen -> IO ()

foreign export ccall
  tpen_final :: Ptr TPen -> IO ()

foreign export ccall
  tpen_copy :: Ptr TPen -> Ptr TPen -> IO ()

foreign export ccall
  tpen_off_all_attribute :: Ptr TPen -> IO ()

foreign export ccall
  tpen_higlight :: Ptr TPen -> IO ()

foreign export ccall
  tpen_dehiglight :: Ptr TPen -> IO ()

foreign export ccall
  tpen_underline :: Ptr TPen -> IO () 

foreign export ccall
  tpen_no_underline :: Ptr TPen -> IO ()

foreign export ccall
  tpen_swp_attr :: Ptr TPen -> IO ()

foreign export ccall
  tpen_reverse :: Ptr TPen -> IO ()

foreign export ccall
  tpen_no_reverse :: Ptr TPen -> IO ()

foreign export ccall
  tpen_set_color :: Ptr TPen -> CUInt -> IO ()

-- | init Tpen, c interface
tpen_init :: Ptr TPen -> IO ()
tpen_init p = peek p >>= (\tpen -> poke p JFBTerm.Pen.init)

-- | Free the memory of TPen
tpen_final :: Ptr TPen -> IO ()
tpen_final p = do
  tpen <- peek p
  let prev = (tpenPrev tpen)
  case prev == nullPtr of
    True ->  return ()
    False -> tpen_final prev 
  c_free prev
  poke p (tpen {tpenPrev = nullPtr})

-- | Copy from src to dst the TPen
tpen_copy :: Ptr TPen -> Ptr TPen -> IO ()
tpen_copy dst src = peek src >>= (\tpen -> poke dst tpen)

-- | offAllAttribute TPen c interface
tpen_off_all_attribute :: Ptr TPen -> IO ()
tpen_off_all_attribute p = peek p >>= (\tpen -> poke p (offAllAttribute tpen))

-- | onHiLight TPen, c interface
tpen_higlight :: Ptr TPen -> IO ()
tpen_higlight p = peek p >>= (\tpen -> poke p (onHiLight tpen))

-- | offHiLight TPen c interface
tpen_dehiglight :: Ptr TPen -> IO ()
tpen_dehiglight p = peek p >>= (\tpen -> poke p (offHiLight tpen))

-- | onUnderLine TPen, c interface
tpen_underline :: Ptr TPen -> IO ()
tpen_underline p = peek p >>= (\tpen -> poke p (onUnderLine tpen))

-- | offUnderLine TPen, c interface
tpen_no_underline :: Ptr TPen -> IO ()
tpen_no_underline p = peek p >>= (\tpen -> poke p (offUnderLine tpen))

-- | onReverse TPen, c interface
tpen_reverse :: Ptr TPen -> IO ()
tpen_reverse p = peek p >>= (\tpen -> poke p (onReverse tpen))

-- | swapAttribute TPen, c interface
tpen_swp_attr :: Ptr TPen -> IO ()
tpen_swp_attr p = peek p >>= (\tpen -> poke p (swapAttribute tpen))

-- | offReverse TPen, c interface
tpen_no_reverse :: Ptr TPen -> IO ()
tpen_no_reverse p = peek p >>= (\tpen -> poke p (offReverse tpen))

-- | setColor TPen, c interface
tpen_set_color :: Ptr TPen -> CUInt -> IO ()
tpen_set_color p col = peek p >>= (\tpen -> poke p (setColor tpen col))
