import Relude hiding ((==))
import System.Environment
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main = readEnv >>= test

readEnv = getEnv "ghc" >>= \t ->
    let Just x = parseMaybe ghcP t in return x

ghcP :: Parser GHC
ghcP = GHC <$> (decimal <* char '.')
           <*> (decimal <* char '.')
           <*> decimal

test = testConstraints . figureConstraints

figureConstraints =
  \case
    GHC 8  0  _   ->  "bytestring" == "0.10.10.0"

    GHC 9  0  _   ->  "bytestring" ^>= "0.11"

    _             ->  []

a ^>= b = [ ConstraintCaret (Package a) (Version b) ]

a == b = [ ConstraintExact (Package a) (Version b) ]

testConstraints cs = callProcess "cabal" $
    "test" : "all" : map (toString . constraintFlag) cs

constraintFlag (ConstraintCaret (Package a) (Version b)) =
    "--constraint=" <> a <> " ^>= " <> b
constraintFlag (ConstraintExact (Package a) (Version b)) =
    "--constraint=" <> a <> " == " <> b


---  Types  ---

type Parser = Parsec Void String

data GHC = GHC Integer Integer Integer
  -- ^ A GHC version number, like GHC 8.10.3

newtype Package = Package Text
  -- ^ A package name, like "containers"

newtype Version = Version Text
  -- ^ A version number, like "0.6"

data Constraint =
    ConstraintCaret Package Version
      -- ^ A package constraint like "containers ^>= 0.6"
  | ConstraintExact Package Version
      -- ^ A package constraint like "containers == 0.6"
