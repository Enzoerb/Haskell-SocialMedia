module Service.IdenticonService (generateIdenticon) where

import Data.Char (ord)
import Codec.Picture
import Codec.Picture.Types
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BS

type ColorRGB = (Int, Int, Int)

colorFromHash :: String -> ColorRGB
colorFromHash hash = (r, g, b)
  where
    r = read ("0x" ++ take 2 hash) :: Int
    g = read ("0x" ++ take 2 (drop 2 hash)) :: Int
    b = read ("0x" ++ take 2 (drop 4 hash)) :: Int

identiconPattern :: String -> [[Bool]]
identiconPattern hash =
    let unicodeList = take 15 $ map ord hash
        identiconPatternList = foldr (\x acc -> (mod x 3 == 0) : acc) [] unicodeList
        matrix3x5 = (\i -> take 3 . drop i $ identiconPatternList) <$> [0, 3..12]
    in mirrorMatrix matrix3x5

mirrorMatrix :: [[a]] -> [[a]]
mirrorMatrix = map (\row -> row ++ reverse (take 2 row))


patternToImage :: ColorRGB -> [[Bool]] -> Image PixelRGB8
patternToImage (r, g, b) pattern = generateImage pixelRenderer width height
  where
    width = 30 * length (head pattern)
    height = 30 * length pattern
    pixelRenderer x y = 
      if pattern !! (y `div` 30) !! (x `div` 30)
        then PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
        else PixelRGB8 255 255 255

imageToBase64 :: Image PixelRGB8 -> String
imageToBase64 image = BS.unpack . encode . toStrict $ encodePng image

generateIdenticon :: String -> IO (String)
generateIdenticon hash = do
  return $  "data:image/png;base64, " <> (imageToBase64 . patternToImage (colorFromHash hash) $ identiconPattern hash)
