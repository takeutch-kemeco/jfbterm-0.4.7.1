diff --git a/util-h.hs b/util-h.hs
index b43872d..b4e5f7d 100644
--- a/util-h.hs
+++ b/util-h.hs
@@ -2,11 +2,16 @@ module JFBTerm.Util where
 
 import Foreign
 import Foreign.Ptr
-import Foreign.C.Types
+import Foreign.C.Types (CUid)
 import Foreign.C.String
 import Data.Char (ord)
 import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID)
 
+data VirtualUID = {virtualUID, virtualEffectiveUID :: CUid} deribing (Show, Eq)
+
+foreign export ccall
+  util_getuid :: VirtualUID -> IO CUid
+
 foreign export ccall
   util_search_string :: CString -> Ptr CString -> IO CInt
 
@@ -16,6 +21,10 @@ foreign export ccall
 foreign export ccall
   util_privilege_drop :: CInt -> IO ()
 
+-- | システムが現在realユーザーIDとして考えてるIDを返す
+getUID :: VirtualUID -> IO CUid
+util_getuid vuid = return (virtualUID)
+
 -- | real, effectiveどちらのユーザーIDも、realユーザーIDを用いる状態に設定する
 privilegeDrop :: IO ()
 privilegeDrop = getRealUserID >>= (\ruid -> setUserID ruid >> setEffectiveUserID ruid)
diff --git a/util.c b/util.c
index f3970ea..5fc5129 100644
--- a/util.c
+++ b/util.c
@@ -105,6 +105,7 @@ inline int util_privilege_ioperm(u_int from, u_int num, int turn_on)
 }
 #endif
 
+#if 0
 /* 現在の setreuid() された設定状態に関わらず、
  * システムが現在、本来のrealユーザーIDとして考えてるIDを返す。
  *
@@ -114,4 +115,5 @@ uid_t util_getuid(void)
 {
 	return real_uid;
 }
+#endif
 
