import           System.Exit (exitFailure)
import           System.IO (IO, hSetBuffering, stdout, BufferMode (..))

import qualified Test.Xsh.Lexer
import qualified Test.Xsh.Parser

import           Xsh.Prelude

main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Xsh.Lexer.tests
    , Test.Xsh.Parser.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
