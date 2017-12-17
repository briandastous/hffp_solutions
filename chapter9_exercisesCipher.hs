module Cipher where

import Data.Char

caesar n s = map (shiftChar n) s
  where
    shiftChar n c = shift n c (if isUpper c then 65 else 97)
      where
        shift n c offset = chr ((mod (ord c - offset + n) 26) + offset)

uncaesar n s = caesar (-n) s
