{-# LANGUAGE TemplateHaskell #-}
-- | This is a temporary auxilary import for JuicyPixels until my patches are
--   merged into a released version.
--
--   At the moment, it fixes a bug with file reading, where it not only threw
--   exceptions instead of returning Left, it also read the file lazily.
module Codec.PicturePrime ( readImage'
                          ) where

import Codec.Picture hiding ( readImage )
import Codec.Picture.Types
import Control.Applicative ( (<$>) )
import Control.DeepSeq
import Control.DeepSeq.TH
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
import Prelude hiding (catch)
import System.IO

instance NFData (Image a) where
    rnf (Image width height dat) = rnf width   `seq`
                                   rnf height  `seq`
                                   V.force dat `seq`
                                   ()

instance NFData (MutableImage s a) where
    rnf (MutableImage width height dat) = rnf width  `seq`
                                          rnf height `seq`
                                          dat        `seq`
                                          ()

$(deriveNFData ''DynamicImage)

readImage' :: FilePath -> IO (Either String DynamicImage)
readImage' path = catch doit
                    (\e -> return . Left $ show (e :: IOException))
    where
        doit = withFile path ReadMode $ \h ->
                  force . decodeImage <$> B.hGetContents h
