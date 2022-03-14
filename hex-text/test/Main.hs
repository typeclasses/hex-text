import Data.Foldable (Foldable (fold))
import Data.Semigroup ()
import Data.String (IsString (fromString))
import Numeric.Natural (Natural)
import System.Exit (die)
import Text.Hex (decodeHex, encodeHex, lazilyEncodeHex)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

main :: IO ()
main = run tests

tests :: [Natural]
tests = encodeHexTests ++ decodeHexTests ++ lazilyEncodeHexTests

run :: [Natural] -> IO ()
run [] = putStrLn "Okay"
run xs = die $ "Test failures: " ++ show (xs :: [Natural])

infix 0 #
(#) :: a -> Bool -> [a]
x # True  = []
x # False = [x]

encodeHexTests :: [Natural]
encodeHexTests = fold
  [ 1 # (encodeHex . ByteString.singleton) 192
      == fromString "c0"
  , 2 # (encodeHex . ByteString.singleton) 168
      == fromString "a8"
  , 3 # (encodeHex . ByteString.pack) [192, 168, 1, 2]
      == fromString "c0a80102"
  ]

decodeHexTests :: [Natural]
decodeHexTests = fold
  [ 4 # (fmap ByteString.unpack . decodeHex . Text.pack) "c0a80102"
      == Just [192,168,1,2]
  , 5 # (fmap ByteString.unpack . decodeHex . Text.pack) "c0a8010"
      == Nothing
  , 6 # (fmap ByteString.unpack . decodeHex . Text.pack) "x0a80102"
      == Nothing
  , 7 # (fmap ByteString.unpack . decodeHex . Text.pack) "C0A80102"
      == Just [192,168,1,2]
  ]

lazilyEncodeHexTests :: [Natural]
lazilyEncodeHexTests = fold
  [ 8 # (LazyText.take 8 . lazilyEncodeHex . LazyByteString.pack . cycle) [1, 2, 3]
      == fromString "01020301"
  ]
