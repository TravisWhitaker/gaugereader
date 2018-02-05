{-# LANGUAGE FlexibleContexts
           , RebindableSyntax
           , ScopedTypeVariables
           , TypeOperators
           , ViewPatterns
           #-}

module GaugeReader.Color where

import Data.Array.Accelerate as A

rgbP :: Acc (Array DIM2 Word8)
     -> Acc (Array DIM2 Word8, Array DIM2 Word8, Array DIM2 Word8)
rgbP rgb =
    let (Z :. rgbh :. rgbw) = unlift (shape rgb) :: Z :. Exp Int :. Exp Int
        bwsh = lift (Z :. rgbh :. (div rgbw 3))
        rind :: Exp DIM2 -> Exp DIM2
        rind sh = let (Z :. bwh :. bww) = unlift sh :: Z :. Exp Int :. Exp Int
                  in lift (Z :. bwh :. (bww * 3))
        gind :: Exp DIM2 -> Exp DIM2
        gind sh = let (Z :. bwh :. bww) = unlift sh :: Z :. Exp Int :. Exp Int
                  in lift (Z :. bwh :. ((bww * 3) + 1))
        bind :: Exp DIM2 -> Exp DIM2
        bind sh = let (Z :. bwh :. bww) = unlift sh :: Z :. Exp Int :. Exp Int
                  in lift (Z :. bwh :. ((bww * 3) + 2))
        r = backpermute bwsh rind rgb
        g = backpermute bwsh gind rgb
        b = backpermute bwsh bind rgb
        in lift (r, g, b)

mixDown :: Exp Word8 -> Exp Word8 -> Exp Word8 -> Exp Word8
mixDown r g b = let r' = (fromIntegral r / 255) :: Exp Float
                    g' = (fromIntegral g / 255) :: Exp Float
                    b' = (fromIntegral b / 255) :: Exp Float
                in round (((r' + g' + b') / 3) * 255)

makeRed :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
makeRed rgb = let (r, g, b) = unlift (rgbP rgb) :: ( Acc (Array DIM2 Word8)
                                                   , Acc (Array DIM2 Word8)
                                                   , Acc (Array DIM2 Word8)
                                                   )
              in r

makeBW :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
makeBW rgb = let (r, g, b) = unlift (rgbP rgb) :: ( Acc (Array DIM2 Word8)
                                                  , Acc (Array DIM2 Word8)
                                                  , Acc (Array DIM2 Word8)
                                                  )
             in zipWith3 mixDown r g b

toTwoVal :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Bool)
toTwoVal bw =
    let bw32 = map fromIntegral bw :: Acc (Array DIM2 Word32)
        sm = the $ (sum (flatten bw32))
        (Z :. bwh :. bww) = unlift (shape bw) :: Z :. Exp Int :. Exp Int
        avg :: Exp Word32
        avg = div sm (fromIntegral (bwh * bww))
    in map (<= avg) bw32

rgbToTwoVal :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Bool)
rgbToTwoVal = toTwoVal . makeBW

twoValToBW :: Acc (Array DIM2 Bool) -> Acc (Array DIM2 Word8)
twoValToBW tv = map (\x -> if x then 255 else 0) tv

-- 8-bit grayscale image to RGB8 image for display purposes.
bwToRGB :: Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
bwToRGB bw =
    let (Z :. bwh :. bww) = unlift (shape bw) :: Z :. Exp Int :. Exp Int
        rgbsh = lift (Z :. bwh :. (3 * bww))
        bwind :: Exp DIM2 -> Exp DIM2
        bwind sh = let (Z :. rgbh :. rgbw) = unlift sh :: Z :. Exp Int :. Exp Int
                   in lift (Z :. rgbh :. (div rgbw 3))
    in backpermute rgbsh bwind bw
