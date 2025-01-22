module Data.String.Parser.CSV
import Data.Either
import Data.List1
import Data.Maybe
import Data.String
import Data.String.Parser
import Interlude.Monad
import System.File

||| Records have at least one field.
public export
CSVRecord: Type
CSVRecord = List1 String

||| NOTE: newlines within quoted fields are not currently supported.
export loadFile: HasIO m => String -> m (Either String (List CSVRecord))

comma: Applicative m => ParseT m String
comma = cast <$> char ','

doubleDoubleQuote: Monad m => ParseT m String
doubleDoubleQuote = string "\"\"" *> pure "\""

textdata: Monad m => ParseT m String
textdata = pack <$> some (satisfy (\c =>
  (c >= chr 0x20 && c <= chr 0x21) ||
  (c >= chr 0x23 && c <= chr 0x2B) ||
  (c >= chr 0x2D && c <= chr 0x7E)))

cr, lf: Monad m => ParseT m Char
cr = char (chr 0x0D)
lf = char (chr 0x0A)

escaped: Monad m => ParseT m String
escaped = do
    ignore (char '"')
    f <- fastConcat <$> some (textdata <|> comma <|> map cast cr <|> map cast lf <|> doubleDoubleQuote)
    ignore (char '"')
    pure f

field: Monad m => ParseT m String
field = escaped <|> map lowerMaybe (optional textdata)

crlf: Monad m => ParseT m String
crlf = do
    cr <- (lowerMaybe . map cast) <$> optional cr
    lf <- map cast lf
    pure (cr ++ lf)

||| While CSV.loadFile will not process records containing line breaks, csvRecord will.
export
csvRecord: Monad m => ParseT m (List1 String)
csvRecord = do
    first <- field
    more <- some (comma *> field) <|> pure []
    when (null first && null more) (fail "empty lines aren't records")
    ignore (optional crlf)
    eos
    pure (first ::: more)

loadFile filepath = do
    Right f <- openFile filepath Read
        | Left err => pure (Left (show err))
    parseResult <- transformWhile (Right Lin) $ \case
        Left  _  => pure Nothing
        Right rs => do
            False <- fEOF f
                | True => pure Nothing
            Right line <- fGetLine f
                | Left err => pure (Just (Left (show err)))
            pure$ case (map fst (parse (optional csvRecord) line)) of
                Left   err       => Just (Left err)
                Right (Just row) => Just (Right (rs :< row))
                Right  Nothing   => Just (Right rs)
    closeFile f
    case parseResult of
        Left  err     => pure (Left err)
        Right records => pure (Right (records <>> []))

namespace Test

    ||| Demonstrates use of CSV.loadFile.
    export
    echoFile: String -> IO ()
    echoFile filepath = do
        Right rs <- CSV.loadFile filepath
            | Left err => putStrLn (fastConcat ["error: ", err])
        for_ rs $ \r => do
            putStr "Record:\n"
            traverse_ putStrLn r
            putChar '\n'

