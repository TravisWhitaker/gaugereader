{-# LANGUAGE BangPatterns #-}

module GaugeReader.FrameIO where

import Codec.FFmpeg

import Codec.Picture

import qualified Data.Array.Accelerate    as A
import qualified Data.Array.Accelerate.IO as A

import Data.Bifunctor

import qualified Data.Vector.Storable as V

-- | Returns an RGB-ordered row-major array.
imgRGB8ToAccRGB8 :: Image PixelRGB8 -> A.Array A.DIM2 A.Word8
imgRGB8ToAccRGB8 (Image w h v) = A.fromVectors (A.Z A.:. h A.:. (3 * w)) v

-- | From an RGB-ordered row-major array.
accRGB8ToImgRGB8 :: A.Array A.DIM2 A.Word8 -> Image PixelRGB8
accRGB8ToImgRGB8 arr = let (A.Z A.:. h A.:. w) = A.arrayShape arr
                       in Image (div w 3) h (A.toVectors arr)

-- | Need to call Codec.FFmpeg.initFFmpeg before using this.
getFrameGetter :: FilePath -> IO (IO (Maybe (A.Array A.DIM2 A.Word8)), IO ())
getFrameGetter p = do
    (gf, cl) <- imageReader (File p)
    pure ((imgRGB8ToAccRGB8 <$>) <$> gf, cl)

-- | Need to call Codec.FFmpeg.initFFmpeg before using this.
getFrameTimeGetter :: FilePath
                   -> IO (IO (Maybe (A.Array A.DIM2 A.Word8, Double)), IO ())
getFrameTimeGetter p = do
    (gf, cl) <- imageReaderTime (File p)
    pure ((first imgRGB8ToAccRGB8 <$>) <$> gf, cl)

mkVidEncParams :: Int -- ^ Width
               -> Int -- ^ Height
               -> Int -- ^ Frames per second.
               -> EncodingParams
mkVidEncParams w h fps = EncodingParams {
    epWidth       = fromIntegral w
  , epHeight      = fromIntegral h
  , epFps         = fps
  , epCodec       = Nothing
  , epPixelFormat = Nothing
  , epPreset      = ""
  , epFormatName  = Nothing
  }
