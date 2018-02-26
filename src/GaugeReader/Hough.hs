{-# LANGUAGE FlexibleContexts
           , RebindableSyntax
           , ScopedTypeVariables
           , TypeOperators
           , ViewPatterns
           #-}

module GaugeReader.Hough where

import Data.Array.Accelerate as A

import qualified Prelude

-- 0 - 360
--indToRad :: Int -> Int -> Double
--indToRad m d = let fI = Prelude.fromIntegral
--               in ((fI d) / (fI m)) * 2 * pi
--
--indToRadExp :: Exp Int -> Exp Int -> Exp Double
--indToRadExp m d = ((fromIntegral d) / (fromIntegral m)) * 2 * pi

-- 0 - 180
indToRad :: Int -> Int -> Double
indToRad m d = let fI = Prelude.fromIntegral
               in ((fI d) / (fI m)) * pi

indToRadExp :: Exp Int -> Exp Int -> Exp Double
indToRadExp m d = ((fromIntegral d) / (fromIntegral m)) * pi

crop :: Exp Double  -- ^ Top crop frac
     -> Exp Double  -- ^ Bottom crop frac
     -> Exp Double  -- ^ Left crop frac
     -> Exp Double  -- ^ Right crop frac
     -> Acc (Array DIM2 Bool)
     -> Acc (Array DIM2 Bool)
crop t b l r occ =
    let (Z :. occh :. occw) = unlift (shape occ) :: Z :. Exp Int :. Exp Int
        tpix = round (t * (fromIntegral occh)) :: Exp Int
        bpix = round (b * (fromIntegral occh)) :: Exp Int
        lpix = round (l * (fromIntegral occw)) :: Exp Int
        rpix = round (r * (fromIntegral occw)) :: Exp Int
        res = lift (Z :. ((occh - tpix) - bpix) :. ((occw - lpix) - rpix)) :: Exp (Z :. Int :. Int)
        per oi =
            let (Z :. yi :. xi) = unlift oi :: Z :. Exp Int :. Exp Int
            in lift (Z :. (yi + tpix) :. (xi + lpix)) :: Exp (Z :. Int :. Int)
    in backpermute res per occ

circleMask :: Exp Int -> Acc (Array DIM2 Bool) -> Acc (Array DIM2 Bool)
circleMask rad occ =
    let (Z :. h :. w) = unlift (shape occ) :: Z :. Exp Int :. Exp Int
        circ :: Exp Double -> Exp (Z :. Int :. Int) -> Exp Bool
        circ r sh =
            let (Z :. y :. x) = unlift sh :: Z :. Exp Int :. Exp Int
                x' = (fromIntegral x) - (fromIntegral w / 2)
                y' = (fromIntegral y) - (fromIntegral h / 2)
            in (sqrt (x' * x' + y' * y')) <= r
        per :: Exp (Z :. Int :. Int) -> Exp (Z :. Int :. Int)
        per sh = if circ (fromIntegral rad) sh then sh else ignore
        def :: Acc (Array DIM2 Bool)
        def = fill (shape occ) (lift False)
    in permute (||) def per occ

hVals :: Exp Int -> Exp Int -> Acc (Array DIM2 Bool) -> Acc (Array DIM2 Word32)
hVals thetaCols rRows occ =
    let (Z :. occh :. occw) = unlift (shape occ) :: Z :. Exp Int :. Exp Int
        trep :: Acc (Array DIM3 Bool)
        trep = replicate (lift (Z :. thetaCols :. All :. All)) occ
        tw32 :: Acc (Array DIM3 Word32)
        tw32 = map (\b -> if b then 1 else 0) trep
        per :: Exp (Z :. Int :. Int :. Int) -> Exp (Z :. Int :. Int)
        per tri =
            let (Z :. trt :. try :. trx) = unlift tri :: Z :. Exp Int :. Exp Int :. Exp Int
                x     = ((fromIntegral trx) - ((fromIntegral occw) / 2)) :: Exp Double
                y     = -((fromIntegral try) - ((fromIntegral occh) / 2)) :: Exp Double
                theta = indToRadExp thetaCols trt
                r = (x * (cos theta) + y * (sin theta)) :: Exp Double
                rind = round r + (rRows `div` 2)
                resind = if (rind < rRows) && (rind >= 0)
                         then lift (Z :. rind :. trt)
                         else ignore
            in if (trep ! tri) then resind
                               else ignore
        def :: Acc (Array DIM2 Word32)
        def = fill (lift (Z :. rRows :. thetaCols)) 0
    in permute (+) def per tw32

hValDebugFrame :: Acc (Array DIM2 Word32) -> Acc (Array DIM2 Word8)
hValDebugFrame hvs =
    let flt = flatten hvs
        mx = fromIntegral (the (maximum flt))
        mn = fromIntegral (the (minimum flt))
        toFltRange :: Exp Word32 -> Exp Double
        toFltRange v32 = (fromIntegral v32 - mn) / (mx - mn)
        toByteRange :: Exp Double -> Exp Word8
        toByteRange vd = round (vd * 255)
    in map (toByteRange . toFltRange) hvs

maxPoint :: Acc (Array DIM2 Word32) -> Acc (Scalar DIM2)
maxPoint hvs =
    let hvsind :: Acc (Array DIM1 (DIM2, Word32))
        hvsind = flatten (indexed hvs)
        fight :: Exp (DIM2, Word32) -> Exp (DIM2, Word32) -> Exp (DIM2, Word32)
        fight x y  =
            let (xind, xval) = unlift x :: (Exp DIM2, Exp Word32)
                (yind, yval) = unlift y :: (Exp DIM2, Exp Word32)
            in if xval >= yval then x else y
        maxScalar :: Acc (Scalar (DIM2, Word32))
        maxScalar = fold1 fight hvsind
    in map fst maxScalar

-- | True if right, false if left.
lorR :: Acc (Array DIM2 Bool) -> Acc (Scalar Bool)
lorR occ =
    let (Z :. occh :. occw) = unlift (shape occ) :: Z :. Exp Int :. Exp Int
        lor :: Exp DIM2 -> Exp Bool -> Exp (Word32, Word32)
        lor i b = let (Z :. occy :. occx) = unlift i :: Z :. Exp Int :. Exp Int
                      v = if b then 1 else 0 :: Exp Word32
                  in if (occx > (occw `div` 2))
                     then lift (0 :: Exp Word32, v) :: Exp (Word32, Word32)
                     else lift (v, 0 :: Exp Word32) :: Exp (Word32, Word32)
        lors = imap lor occ
        lrsum lrx lry = let (lx, rx) = unlift lrx :: (Exp Word32, Exp Word32)
                            (ly, ry) = unlift lry :: (Exp Word32, Exp Word32)
                        in lift (lx + ly, rx + ry)
        (l, r) = unlift (the (fold1All lrsum  lors))
    in unit (r > l)
