import Data.Foldable
import Data.String
import Numeric.Natural
import System.Exit
import Text.Hex

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

main = run tests

tests = encodeHexTests <> decodeHexTests <> lazilyEncodeHexTests

run [] = putStrLn "Okay"
run xs = die $ "Test failures: " <> show (xs :: [Natural])

infix 0 #
x # True = []
x # False = [x]

encodeHexTests = fold
  [ 1 # (encodeHex . ByteString.singleton) 192 == fromString "c0"
  , 2 # (encodeHex . ByteString.singleton) 168 == fromString "a8"
  , 3 # (encodeHex . ByteString.pack) [192, 168, 1, 2] == fromString "c0a80102"
  ]

decodeHexTests = fold
  [ 4 # (fmap ByteString.unpack . decodeHex . Text.pack) "c0a80102" == Just [192,168,1,2]
  , 5 # (fmap ByteString.unpack . decodeHex . Text.pack) "c0a8010" == Nothing
  , 6 # (fmap ByteString.unpack . decodeHex . Text.pack) "x0a80102" == Nothing
  , 7 # (fmap ByteString.unpack . decodeHex . Text.pack) "C0A80102" == Just [192,168,1,2]
  ]

lazilyEncodeHexTests = fold
  [ 8 # (LazyText.take 8 . lazilyEncodeHex . LazyByteString.pack . cycle) [1, 2, 3] == fromString "01020301"
  ]
