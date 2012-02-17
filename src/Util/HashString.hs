{-# LANGUAGE TemplateHaskell, OverloadedStrings, BangPatterns #-}
-- | A 'HashString' represents a string, annotated with its hash. This allows
--   us to avoid frequent rehashing.
--
--   The recommended way of creating a 'HashString' is either statically, using
--   quasiquotes:
--
--   > [hashed|hello, world!|] -- Generates the HashString "hello, world!"
--
--   or dynamically, with 'fromString':
--
--   > name <- loadNameFromFile "some-text-file"
--   > fromString name -- Generates the hashString equivalent to whatever was
--                     -- in 'name'.
--
--   Use the quasiquoter as much as possible, as it makes the hashing run at
--   compile time, saving us from having to do _any_ hashing at runtime.
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
module Util.HashString ( -- * Normal Haskell Interface
                         HashString(..)
                       , fromHashString
                       -- * Template Haskell Helpers
                       , hashed
                       ) where

import Control.DeepSeq
import Data.Hashable
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

-- | The HashString constructor is exposed so it can be used with the 'hashed'
--   quasiquoter. Please don't call it manually. If you want to construct a
--   HashString, please use 'fromString'.
--
--   The best part about HashStrings? If you compare two HashStrings with
--   unique hashes, the text will never even have to be evaluated - it will
--   remain a thunk, saving needless computation and memory waste.
data HashString = HashString !Int
                              T.Text

-- | The usual use case for HashString is in a HashMap. We'd frequently like
--   to force the entire tree to prevent deleted elements from being leaked.
--
--   Unfortunately, when we do this, we don't want the hashstring's text to
--   be needlessly forced. Therefore, we only keep the hash strict, and leave
--   the text itself lazy. This lets us deepseq the hashmap without walking
--   through text every time.
instance NFData HashString where
    rnf (HashString _ _) = ()

instance IsString HashString where
    fromString s = HashString (hash asText) asText
        where
            asText = T.pack s

instance Ord HashString where
    compare (HashString ha a) (HashString hb b) = case compare ha hb of
                                                    EQ -> compare a b
                                                    x  -> x

instance Eq HashString where
    (HashString ha sa) == (HashString hb sb) = ha == hb
                                           && sa == sb

-- | Converts a 'HashString' to text.
fromHashString :: HashString -> T.Text
fromHashString (HashString _ x) = x
{-# INLINE fromHashString #-}

instance Hashable HashString where
    hash (HashString h _) = h

-- | The template haskell splice which lets us generate 'HashString's at
--   compile-time. Use this like so:
--
--   > $(hString "hello, world")
--
--   Which would be the same as you saying:
--
--   > "hello, world" :: HashString
--
--   except it's evaluated at compile time. Please use this function wherever
--   possible to save a lot of wasted cycles.
hString :: String -> Q Exp
hString s = [| HashString $(lift . hash $ T.pack s) (fromString $(liftString s)) |]

-- | The 'hashed' quasiquoter lets us automatically create a 'HashString' at
--   compile time. This saves us from having to ever has this string at
--   runtime. Use this whenever you want to construct a 'HashString' from a
--   compile-time string.
--
--   Usage:
--
--   > [hashed|what is this witchcraft|] -- == (fromString "what is this witchcraft") :: HashString
hashed :: QuasiQuoter
hashed = QuasiQuoter { quoteExp  = hString
                     , quotePat  = undefined
                     , quoteType = undefined
                     , quoteDec  = undefined
                     }
