module CFilesTest where

import Foreign
import Foreign.C.Types

foreign import ccall "print_kek.h print_kek" c_print_kek :: CInt -> IO CInt

tstCFiles = do
  print "Test C files in Hs project"
  c_print_kek (CInt 55)