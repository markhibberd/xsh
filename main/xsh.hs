{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad.IO.Class (liftIO)

import qualified System.Console.Haskeline as Haskeline

import           System.Exit (exitWith)
import           System.IO (IO, FilePath)
import           System.IO (BufferMode (..))
import           System.IO (stdout, stderr)
import qualified System.IO as IO

import qualified Options.Applicative as Options

import           Xsh.Prelude
import qualified Xsh.Parser as Parser
import qualified Xsh.Interpretter as Interpretter

data Command =
    StartShell
  | RunScript FilePath

main :: IO ()
main = do
  IO.hSetBuffering stdout LineBuffering
  IO.hSetBuffering stderr LineBuffering
  Options.execParser (Options.info (parser <**> Options.helper) Options.idm) >>= go

parser :: Options.Parser Command
parser =
  (RunScript <$> filepath') <|> (pure StartShell)

go :: Command -> IO ()
go c =
  case c of
    StartShell -> do
      interactive
    RunScript p -> do
      input <- IO.readFile p
      case Parser.parse p input of
        Left msg ->
          IO.putStrLn msg
        Right program -> do
          r <- Interpretter.interpret IO.stdin IO.stdout IO.stderr program
          exitWith r

filepath' :: Options.Parser FilePath
filepath' = Options.strArgument . mconcat $ [
    Options.metavar "SCRIPT"
  , Options.help "Path to script file to execute."
  ]

interactive :: IO ()
interactive = do
  let
    loop = do
      minput <- Haskeline.getInputLine "$ "
      case minput of
        Nothing ->
          pure ()
        Just input -> do
          liftIO $ case Parser.parse "<interactive>" input of
            Left msg ->
              IO.putStrLn msg
            Right ast -> do
              void $ Interpretter.interpret IO.stdin IO.stdout IO.stderr ast
          loop

  Haskeline.runInputT Haskeline.defaultSettings loop
