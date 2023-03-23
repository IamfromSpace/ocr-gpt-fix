{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative (many)
import Data.List.Split (chunksOf)
import Data.Text.Metrics (levenshteinNorm)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client (completeChat, OpenAIClient, ClientError, makeOpenAIClient)
import OpenAI.Resources (ChatCompletionMessage (..), ChatCompletion(..), defaultChatCompletionCreate, ChatCompletionCreate (..), ChatCompletionChoice (..))
import Options.Applicative (argument, command, subparser, info, progDesc, execParser, ParserInfo, str, metavar)
import System.Environment (getEnv)
import System.IO (stderr, hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

systemMessage :: ChatCompletionMessage
systemMessage =
  ChatCompletionMessage "system" "Your task is to correct OCR outputs from scans of Spanish novel pages.  The OCR output is riddled with various types of errors, such as spelling, wrong words, incorrect breaks, and even out of order lines.  Worst of all though, OCR clearly does not interpret Spanish dialog markers correctly.  The goal is to perfectly replicate the text from the original page and ONLY change things that are erroneous.  Each new message is unrelated to the last."

preamble :: T.Text
preamble = "Please fix any errors in the folowing OCR scan of a Spanish novel page:\n\n"

-- TODO: This needs to be configurable, so that users can use
-- the preferred text they have rights to, that this lib may
-- not.
exampleInput :: ChatCompletionMessage
exampleInput =
  ChatCompletionMessage "user" (preamble <> "")

exampleOutput :: ChatCompletionMessage
exampleOutput =
  ChatCompletionMessage "assistant" ""

mkPrompt :: T.Text -> [ChatCompletionMessage]
mkPrompt ocrRaw =
  [ systemMessage
  , exampleInput
  , exampleOutput
  , ChatCompletionMessage "user" (preamble <> ocrRaw)
  ]

mkCCC :: T.Text -> ChatCompletionCreate
mkCCC ocrRaw =
  let
    withDefaults =
      defaultChatCompletionCreate
        "gpt-3.5-turbo"
        (mkPrompt ocrRaw)
  in
    withDefaults
      { cccTemperature = Just 0.54
      -- TODO: Would be nice if this were contextual.  Not
      -- sure how practical it would be to create a Haskell
      -- OpenAI tokenizer.
      , cccMaxTokens = Just 1100
      , cccStop = Just (pure "END")
      }

extractFixed :: ChatCompletion -> T.Text
extractFixed completion =
  let
    ChatCompletion { ccChoices } = completion
    ChatCompletionChoice { cccMessage } = V.head ccChoices
    ChatCompletionMessage { ccmContent } = cccMessage
  in
    ccmContent

requestOcrFix :: T.Text -> OpenAIClient -> IO (Either ClientError T.Text)
requestOcrFix ocrRaw client =
  fmap extractFixed <$> completeChat client (mkCCC ocrRaw)

-- TODO: Need to do caching.  We can hash the input text and
-- then store it in a .ocr-gpt-fix-cache folder.
-- (Probably also need a cache clear command or no-cache flag)
ocrFix :: T.Text -> IO T.Text
ocrFix ocrRaw = do
  manager <- newManager (tlsManagerSettings { managerResponseTimeout = responseTimeoutNone })
  apiKey <- T.pack <$> getEnv "OPENAI_API_KEY"
  client <- pure (makeOpenAIClient apiKey manager 1)
  answer <- requestOcrFix ocrRaw client
  let printStats x = do
        let difference = levenshteinNorm ocrRaw x
        hPutStrLn stderr ("Fix complete.  Levenshtein Norm: " <> show difference)
        return x
  either (error . show) printStats answer

ocrFixStdio :: IO ()
ocrFixStdio =
  TIO.getContents >>= ocrFix >>= TIO.putStr

ocrFixFile :: String -> IO ()
ocrFixFile path =
  TIO.readFile path >>= ocrFix >>= TIO.writeFile (path <> ".fixed")

ocrFixFiles :: [String] -> IO ()
ocrFixFiles paths = const () <$> traverse ocrFixFile paths

ocrFixComposite' :: T.Text -> IO T.Text
ocrFixComposite' multi =
   let
      splitPages :: T.Text -> [T.Text]
      splitPages = fmap T.strip . T.splitOn "PAGE" . T.stripStart

      processPages = traverse ocrFix -- . fmap (T.intercalate "\n\n") . chunksOf 2

      processChapter = traverse processPages

      processChapters = traverse processChapter

      recombinePages = T.intercalate "\n\n" . fmap T.strip

      recombineChapter (heading, pages) = "# " <> heading <> "\n\n" <> recombinePages pages

      recombineChapters = T.intercalate "\n\n" . fmap recombineChapter

      chapters = drop 1 $ T.splitOn "CHAPTER " multi
      chapterNamesWithPages = fmap (fmap splitPages . T.breakOn "\n") chapters
   in
      recombineChapters <$> processChapters chapterNamesWithPages

ocrFixComposite :: IO ()
ocrFixComposite =
   TIO.getContents >>= ocrFixComposite' >>= TIO.putStr

data FixMode
  = Stdio
  | Batch [String]
  | StdioBatch

parser :: ParserInfo FixMode
parser =
  info
    (subparser
      (command "batch" (info (Batch <$> many (argument str (metavar "FILE"))) (progDesc "Process many files simultaneously, writing them to new ones as $NAME.fixed"))
      <> command "stdio" (info (pure Stdio) (progDesc "Process one prompt from stdin and get the result on stdout"))
      <> command "stdio-batch" (info (pure StdioBatch) (progDesc "Process a single string with chapter and page delimeters to build out the final markdown"))
      )
    )
    (progDesc "Fix an OCR scan of a spanish novel.")

main :: IO ()
main = do
  mode <- execParser parser
  case mode of
    Stdio -> ocrFixStdio
    Batch paths -> ocrFixFiles paths
    StdioBatch -> ocrFixComposite
