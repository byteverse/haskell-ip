{-# LANGUAGE CPP #-}
{- | Compatibility module allowing us to support UTF-16 & UTF-8 versions of
the 'text' package.
-}
module Data.Text.Builder.Common.Compat (Codepoint) where

import Data.Word

#if MIN_VERSION_text(2, 0, 0)
type Codepoint = Word8
#else
type Codepoint = Word16
#endif
