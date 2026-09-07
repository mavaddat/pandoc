{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.ImageSize
   Copyright   : © 2025 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for image type and size detection.
-}
module Tests.ImageSize (tests) where

import Data.Bits (shiftR)
import qualified Data.ByteString as B
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.ImageSize
import Text.Pandoc.Options (def)

-- helpers to construct binary test data:

le32, be32 :: Int -> B.ByteString
le32 n = B.pack $ map (fromIntegral . (n `shiftR`) . (8 *)) [0,1,2,3]
be32 n = B.pack $ map (fromIntegral . (n `shiftR`) . (8 *)) [3,2,1,0]

-- | An ISO BMFF box with the given type and contents.
box :: B.ByteString -> B.ByteString -> B.ByteString
box name body = be32 (8 + B.length body) <> name <> body

-- | An EMF header with the given frame bounds (1/100 mm), reference
-- device size in pixels, and reference device size in mm.
emfFile :: [Int] -> [Int] -> [Int] -> B.ByteString
emfFile frame device mm = B.concat $
  [le32 1, B.replicate 20 0] <> map le32 frame <>
  [" EMF", B.replicate 28 0] <> map le32 (device <> mm)

-- | A WebP RIFF container with the given chunk.
webpFile :: B.ByteString -> B.ByteString
webpFile chunk = "RIFF\0\0\0\0WEBP" <> chunk

svgFile :: B.ByteString -> B.ByteString
svgFile attrs =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" " <> attrs <> "></svg>"

jpegBare, jpegApp0 :: B.ByteString
jpegBare = B.pack [0xff, 0xd8, 0xff, 0xdb] <> "rest"
jpegApp0 = B.pack [0xff, 0xd8, 0xff, 0xe0] <> "rest"

-- an AVIF image with an ispe (image spatial extents) box:
avifIspe :: B.ByteString
avifIspe = box "ftyp" ("avif" <> B.replicate 4 0)
        <> box "meta" (B.replicate 4 0 <>  -- version/flags
             box "iprp" (box "ipco"
               (box "ispe" (B.replicate 4 0 <> be32 640 <> be32 480))))

-- an (animated) AVIF image with dimensions only in a tkhd box:
avisTkhd :: B.ByteString
avisTkhd = box "ftyp" ("avis" <> B.replicate 4 0)
        <> box "moov" (box "trak" (box "tkhd"
             (B.replicate 4 0 <>   -- version/flags
              B.replicate 72 0 <>  -- times, track id, duration, etc.
              be32 (640 * 65536) <> be32 (480 * 65536))))  -- 16.16

-- lossless webp, 100x200: width - 1 in bits 0-13, height - 1 in
-- bits 14-27 of the little-endian word after the 0x2f signature
webpLossless :: B.ByteString
webpLossless = webpFile $ "VP8L\0\0\0\0"
  <> B.pack [0x2f, 0x63, 0xc0, 0x31, 0x00]

-- lossy webp, 320x240: frame tag, then 9d 01 2a keyframe signature,
-- then 14-bit width and height as little-endian 16-bit words
webpLossy :: B.ByteString
webpLossy = webpFile $ "VP8 \0\0\0\0"
  <> B.pack [0x00, 0x00, 0x00, 0x9d, 0x01, 0x2a, 0x40, 0x01, 0xf0, 0x00]

-- extended webp, 1000x500: canvas width and height - 1 as 24-bit
-- little-endian words after the flags
webpExtended :: B.ByteString
webpExtended = webpFile $ "VP8X" <> B.replicate 8 0
  <> B.pack [0xe7, 0x03, 0x00, 0xf3, 0x01, 0x00]

-- zlib-compressed "<</MediaBox [0 0 100 200]>>"
compressedMediaBox :: B.ByteString
compressedMediaBox = B.pack
  [ 120, 156, 179, 177, 209, 247, 77, 77, 201, 76, 116, 202, 175, 80
  , 136, 54, 80, 48, 80, 48, 52, 48, 80, 48, 50, 48, 136, 181, 179
  , 3, 0, 104, 186, 6, 232 ]

tests :: [TestTree]
tests =
  [ testGroup "imageType"
    [ testCase "png" $ imageType "\x89PNG\r\n\x1a\n" @?= Just Png
    , testCase "gif" $ imageType "GIF89a" @?= Just Gif
    , testCase "tiff (little-endian)" $ imageType "II\x2a\0" @?= Just Tiff
    , testCase "tiff (big-endian)" $ imageType "MM\0\x2a" @?= Just Tiff
    , testCase "jpeg with app segment" $ imageType jpegApp0 @?= Just Jpeg
    , testCase "jpeg without app segment" $ imageType jpegBare @?= Just Jpeg
    , testCase "pdf" $ imageType "%PDF-1.5" @?= Just Pdf
    , testCase "eps" $ imageType "%!PS-Adobe-3.0 EPSF-3.0" @?= Just Eps
    , testCase "svg" $ imageType (svgFile "") @?= Just Svg
    , testCase "svg with xml declaration" $
        imageType ("<?xml version=\"1.0\"?>\n" <> svgFile "") @?= Just Svg
    , testCase "svg with BOM" $
        imageType ("\xef\xbb\xbf" <> svgFile "") @?= Just Svg
    , testCase "emf" $
        imageType (emfFile [0,0,1,1] [1,1] [1,1]) @?= Just Emf
    , testCase "webp" $ imageType webpLossless @?= Just Webp
    , testCase "avif brand" $ imageType avifIspe @?= Just Avif
    , testCase "avis brand" $ imageType avisTkhd @?= Just Avif
    , testCase "other ISO media is not avif" $
        imageType (box "ftyp" ("isom" <> B.replicate 4 0)) @?= Nothing
    , testCase "garbage" $ imageType "garbage!" @?= Nothing
    ]
  , testGroup "imageSize"
    [ testGroup "eps"
      [ testCase "zero origin" $
          imageSize def "%!PS EPSF\n%%BoundingBox: 0 0 612 792\n"
            @?= Right (ImageSize 612 792 72 72)
      , testCase "nonzero origin" $
          imageSize def "%!PS EPSF\n%%BoundingBox: 10 20 110 220\n"
            @?= Right (ImageSize 100 200 72 72)
      , testCase "negative origin" $
          imageSize def "%!PS EPSF\n%%BoundingBox: -10 -10 90 190\n"
            @?= Right (ImageSize 100 200 72 72)
      ]
    , testGroup "pdf"
      [ testCase "MediaBox" $
          imageSize def "%PDF-1.4\n<</MediaBox [0 0 612 792]>>"
            @?= Right (ImageSize 612 792 72 72)
      , testCase "MediaBox in compressed object stream" $
          imageSize def ("%PDF-1.5\n<</Type /ObjStm>>\nstream\n"
                          <> compressedMediaBox <> "\nendstream\n")
            @?= Right (ImageSize 100 200 72 72)
      , testCase "corrupt compressed object stream" $
          imageSize def ("%PDF-1.5\n<</Type /ObjStm>>\nstream\n"
                          <> "NOT ZLIB DATA\nendstream\n")
            @?= Left "could not determine PDF size"
      , testCase "MediaBox after corrupt object stream" $
          imageSize def ("%PDF-1.5\n<</Type /ObjStm>>\nstream\n"
                          <> "NOT ZLIB DATA\nendstream\n"
                          <> "<</MediaBox [0 0 300 400]>>")
            @?= Right (ImageSize 300 400 72 72)
      ]
    , testGroup "svg"
      [ testCase "width and height attributes" $
          imageSize def (svgFile "width=\"50\" height=\"60\"")
            @?= Right (ImageSize 50 60 96 96)
      , testCase "viewBox fallback" $
          imageSize def (svgFile "viewBox=\"0 0 100 200\"")
            @?= Right (ImageSize 100 200 96 96)
      , testCase "viewBox with commas" $
          imageSize def (svgFile "viewBox=\"0,0,100,200\"")
            @?= Right (ImageSize 100 200 96 96)
      , testCase "viewBox with fractional values" $
          imageSize def (svgFile "viewBox=\"0, 0, 210.5, 297.3\"")
            @?= Right (ImageSize 210 297 96 96)
      , testCase "no size information" $
          imageSize def (svgFile "")
            @?= Left "could not determine SVG size"
      ]
    , testGroup "emf"
      [ testCase "size and dpi from header" $
          imageSize def (emfFile [0,0,10000,5000] [1024,768] [320,240])
            @?= Right (ImageSize 320 160 81 81)
      , testCase "nonzero frame origin" $
          imageSize def (emfFile [2000,1000,12000,6000] [1024,768] [320,240])
            @?= Right (ImageSize 320 160 81 81)
      , testCase "zero-size reference device" $
          imageSize def (emfFile [0,0,10000,5000] [1024,768] [0,240])
            @?= Left "could not determine EMF size"
      ]
    , testGroup "webp"
      [ testCase "lossless (VP8L)" $
          imageSize def webpLossless @?= Right (ImageSize 100 200 96 96)
      , testCase "lossy (VP8)" $
          imageSize def webpLossy @?= Right (ImageSize 320 240 96 96)
      , testCase "extended (VP8X)" $
          imageSize def webpExtended @?= Right (ImageSize 1000 500 96 96)
      ]
    , testGroup "avif"
      [ testCase "ispe box" $
          imageSize def avifIspe @?= Right (ImageSize 640 480 72 72)
      , testCase "tkhd box" $
          imageSize def avisTkhd @?= Right (ImageSize 640 480 72 72)
      ]
    , testGroup "fixtures"
      [ testCase "lalune.jpg" $ do
          img <- B.readFile "lalune.jpg"
          imageSize def img @?= Right (ImageSize 250 250 120 120)
      , testCase "fb2/test-small.png" $ do
          img <- B.readFile "fb2/test-small.png"
          imageSize def img @?= Right (ImageSize 48 32 71 71)
      , testCase "bodybg.gif" $ do
          img <- B.readFile "bodybg.gif"
          imageSize def img @?= Right (ImageSize 230 334 72 72)
      ]
    ]
  ]
