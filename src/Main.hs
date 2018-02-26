{-# LANGUAGE BangPatterns
           , CPP
           , DeriveGeneric
           , DeriveAnyClass
           , FlexibleContexts
           , OverloadedStrings
           #-}

module Main where

import Codec.FFmpeg

import qualified Data.Array.Accelerate as A

#ifdef CUDA
import Data.Array.Accelerate.LLVM.PTX
#else
import Data.Array.Accelerate.LLVM.Native
#endif

import qualified Data.ByteString.Lazy as BL

import Data.Csv

import Data.Fixed

import Data.Monoid

import Options.Generic

import System.Environment
import System.IO

import GaugeReader.Color
import GaugeReader.FrameIO
import GaugeReader.Hough

data Needle = Needle {
    needleSeconds  :: !Double
  , needleAngle    :: !Double
  , needleRad      :: !Double
  , needleGrams    :: !Double
  } deriving ( Show
             , Generic
             , ToRecord
             )

tableHeader :: BL.ByteString
tableHeader = "time(seconds),angle(radians),radius(pixels),gauge(grams?)\n"

makeCrop :: Double -- ^ Top crop frac
         -> Double -- ^ Bottom crop frac
         -> Double -- ^ Left crop frac
         -> Double -- ^ Right crop frac
         -> Int    -- ^ Circle mask radius in pixels
         -> A.Array A.DIM2 A.Word8
         -> A.Array A.DIM2 A.Word8
makeCrop t b l r rad = run1 ( bwToRGB
                            . twoValToBW
                            . circleMask (A.constant rad)
                            . crop (A.constant t)
                                   (A.constant b)
                                   (A.constant l)
                                   (A.constant r)
                            . toTwoVal
                            . makeBW
                            )

makeTrans :: Double -- ^ Top crop frac
          -> Double -- ^ Bottom crop frac
          -> Double -- ^ Left crop frac
          -> Double -- ^ Right crop frac
          -> Int    -- ^ Circle mask radius in pixels
          -> Int    -- ^ Theta columns
          -> Int    -- ^ Radius rows
          -> A.Array A.DIM2 A.Word8
          -> A.Array A.DIM2 A.Word8
makeTrans t b l r rad thCols rRows = run1 ( hValDebugFrame
                                          . hVals (A.constant thCols)
                                                  (A.constant rRows)
                                          . circleMask (A.constant rad)
                                          . crop (A.constant t)
                                                 (A.constant b)
                                                 (A.constant l)
                                                 (A.constant r)
                                          . toTwoVal
                                          . makeBW
                                          )

makeRec :: Double -- ^ Top crop frac
        -> Double -- ^ Bottom crop frac
        -> Double -- ^ Left crop frac
        -> Double -- ^ Right crop frac
        -> Int    -- ^ Circle mask radius in pixels
        -> Int    -- ^ Theta columns
        -> Int    -- ^ Radius rows
        -> A.Array A.DIM2 A.Word8
        -> (A.Scalar A.DIM2, A.Scalar Bool)
makeRec t b l r rad thCols rRows =
    let f arr = let circd = ( circleMask (A.constant rad)
                            . crop (A.constant t)
                                   (A.constant b)
                                   (A.constant l)
                                   (A.constant r)
                            . toTwoVal
                            . makeBW
                            ) arr
                    mp = ( maxPoint
                         . hVals (A.constant thCols)
                                 (A.constant rRows)
                         ) circd
                    lr = lorR circd
                in A.lift (mp, lr)
    in run1 f

cropDebugPipeline :: FilePath -- ^ Input
                  -> FilePath -- ^ Output
                  -> Int      -- ^ Width in pixels
                  -> Int      -- ^ Height in pixels
                  -> Int      -- ^ Frames per second
                  -> Double   -- ^ Top crop frac.
                  -> Double   -- ^ Bottom crop frac.
                  -> Double   -- ^ Left crop frac.
                  -> Double   -- ^ Right crop frac.
                  -> Int      -- ^ Circle mask radius in pixels
                  -> IO ()
cropDebugPipeline i o w h fps t b l r rad = do
    (gf, incpl) <- getFrameGetter i
    wf          <- imageWriter (mkVidEncParams w h fps) o
    let f  = makeCrop t b l r rad
        f' = accRGB8ToImgRGB8 . f
        pipe :: Int -> IO ()
        pipe !i = do
            putStrLn ("Processing frame " ++ (show i))
            mfr <- gf
            case mfr of Nothing -> wf Nothing
                        Just fr -> wf (Just (f' fr)) *> pipe (i+1)
    pipe 1
    incpl

transDebugPipeline :: FilePath -- ^ Input
                   -> FilePath -- ^ Output
                   -> Int      -- ^ Width in pixels
                   -> Int      -- ^ Height in pixels
                   -> Int      -- ^ Frames per second
                   -> Double   -- ^ Top crop frac.
                   -> Double   -- ^ Bottom crop frac.
                   -> Double   -- ^ Left crop frac.
                   -> Double   -- ^ Right crop frac.
                   -> Int      -- ^ Circle mask radius in pixels
                   -> Int      -- ^ Theta columns.
                   -> Int      -- ^ Radius rows.
                   -> IO ()
transDebugPipeline i o w h fps t b l r rad tcols rrows = do
    (gf, incpl) <- getFrameGetter i
    wf          <- imageWriter (mkVidEncParams w h fps) o
    let f  = makeTrans t b l r rad tcols rrows
        f' = accRGB8ToImgRGB8 . f
        pipe :: Int -> IO ()
        pipe !i = do
            putStrLn ("Processing frame " ++ (show i))
            mfr <- gf
            case mfr of Nothing -> wf Nothing
                        Just fr -> wf (Just (f' fr)) *> pipe (i+1)
    pipe 1
    incpl

resToNeedle :: Int        -- ^ Theta columns
            -> Double     -- ^ Multiplicative calibration factor
            -> Double     -- ^ Exponential calibration factor
            -> ( (A.Scalar A.DIM2, A.Scalar Bool)
               , Double)  -- ^ Result.
            -> Needle
resToNeedle thCols af bf ((resp, reslr), t) =
    let (A.Z A.:. rad A.:. thInd) = A.indexArray resp A.Z
        ang = indToRad thCols thInd + (if (A.indexArray reslr A.Z)
                                       then pi
                                       else 0
                                      )
    in Needle t
              ang
              (fromIntegral rad)
              (af * (exp (ang * bf)))

tablePipeline :: FilePath -- ^ Input
              -> FilePath -- ^ Output
              -> Double   -- ^ Top crop frac
              -> Double   -- ^ Bottom crop frac
              -> Double   -- ^ Left crop frac
              -> Double   -- ^ Right crop frac
              -> Int      -- ^ Circle mask radius in pixels
              -> Int      -- ^ Theta columns.
              -> Int      -- ^ Radius rows.
              -> Double   -- ^ Multiplicative factor
              -> Double   -- ^ Exponential factor
              -> IO ()
tablePipeline i o t b l r rad tcols rrows af bf = do
    (gft, incpl) <- getFrameTimeGetter i
    h <- openFile o WriteMode
    BL.hPut h tableHeader
    let f = makeRec t b l r rad tcols rrows
        pipe :: Int -> IO [(A.Scalar A.DIM2, Double)]
        pipe !i = do
            putStrLn ("Processing frame " ++ (show i))
            mfrt <- gft
            case mfrt of Nothing      -> pure []
                         Just (fr, t) -> do
                             let !n = resToNeedle tcols af bf (f fr, t)
                             BL.hPut h (encode [n])
                             pipe (i+1)
    pipe 1
    hClose h

valFrac :: Double -> Bool
valFrac x = (x <= 0.5) && (x >= 0)

valTheta :: Int -> Bool
valTheta t = mod t 45 == 0

badFrac :: IO ()
badFrac = do
    putStrLn "Each crop fraction is expressed as a fraction of the"
    putStrLn "corresponding dimension, i.e. --top 0.25 --bottom 0.25 leaves one"
    putStrLn "with the middle 50% of the image's pixels."
    pure ()

badTheta :: IO ()
badTheta = do
    putStrLn "Theta resolution must be a multiple of 45. Although the angle is"
    putStrLn "reported in radians, the angular resolution is provided by the"
    putStrLn "user in degrees."

data Action = CropDebug { input  :: FilePath
                        , output :: FilePath
                        , width  :: Int
                        , height :: Int
                        , fps    :: Int
                        , top    :: Double
                        , bottom :: Double
                        , left   :: Double
                        , right  :: Double
                        , radius :: Int
                        }
            | TransDebug { input     :: FilePath
                         , output    :: FilePath
                         , width     :: Int
                         , height    :: Int
                         , fps       :: Int
                         , top       :: Double
                         , bottom    :: Double
                         , left      :: Double
                         , right     :: Double
                         , radius    :: Int
                         , angleRes  :: Int
                         , radiusRes :: Int
                         }
            | MakeTable { input      :: FilePath
                        , output     :: FilePath
                        , top        :: Double
                        , bottom     :: Double
                        , left       :: Double
                        , right      :: Double
                        , radius     :: Int
                        , angleRes   :: Int
                        , radiusRes  :: Int
                        , multfactor :: Double
                        , expfactor  :: Double
                        }
            deriving ( Show
                     , Generic
                     , ParseRecord
                     )

doAction :: Action -> IO ()
doAction (CropDebug i o w h fps t b l r rad)
    | not (valFrac t && valFrac b && valFrac l && valFrac r) = badFrac
    | otherwise = cropDebugPipeline i o w h fps t b l r rad
doAction (TransDebug i o w h fps t b l r rad tcols rrows)
    | not (valFrac t && valFrac b && valFrac l && valFrac r) = badFrac
    | not (valTheta tcols) = badTheta
    | otherwise = transDebugPipeline i o w h fps t b l r rad tcols rrows
doAction (MakeTable i o t b l r rad tcols rrows af bf)
    | not (valFrac t && valFrac b && valFrac l && valFrac r) = badFrac
    | not (valTheta tcols) = badTheta
    | otherwise = tablePipeline i o t b l r rad tcols rrows af bf

main :: IO ()
main = do
    initFFmpeg
    getRecord "Hough Transform Gauge Video Reader" >>= doAction
