module Data.String.Parser.CSV
import Data.Either
import Data.List1
import Data.Maybe
import Data.String.Parser
import Interlude.Monad
import System.File

import Debug.Trace

public export
CSVRecord: Type
CSVRecord = List1 String

||| NOTE: newlines within quoted fields are not currently supported.
export loadFile: HasIO m => String -> m (Either String (List (List1 String)))

comma: Applicative m => ParseT m String
comma = cast <$> char ','

doubleDoubleQuote: Monad m => ParseT m String
doubleDoubleQuote = string "\"\"" *> pure "\""

textdata: Monad m => ParseT m String
textdata = pack <$> some (satisfy (\c =>
  (c >= chr 0x20 && c <= chr 0x21) ||
  (c >= chr 0x23 && c <= chr 0x2B) ||
  (c >= chr 0x2D && c <= chr 0x7E)))

escaped: Monad m => ParseT m String
escaped = do
    ignore (char '"')
    f <- fastConcat <$> some (textdata <|> comma <|> (cast <$> char '\n') <|> doubleDoubleQuote)
    ignore (char '"')
    pure f

field: Monad m => ParseT m String
field = escaped <|> map lowerMaybe (optional textdata)

crlf: Monad m => ParseT m ()
crlf = do
    ignore (optional (char (chr 0x0D)))
    ignore (char (chr 0x0A))

csvRecord: Monad m => ParseT m (List1 String)
csvRecord = do
    first <- field
    more <- some (comma *> field) <|> pure []
    ignore (optional crlf)
    eos
    pure (first ::: more)

parseRecord: Monad m => String -> m (Either String (List1 String))
parseRecord s = map (map fst) (parseT csvRecord s)

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
            trace "got a line: \{line}" (pure ())
            pure$ case (map fst (parse csvRecord line)) of
                 Left  err => Just (Left err)
                 Right row => trace "Successful parse" $Just (Right (rs :< row))
    closeFile f
    case parseResult of
        Left  err     => pure (Left err)
        Right records => pure (Right (records <>> []))

namespace Test

    export
    echoFile: String -> IO ()
    echoFile filepath = do
        Right rs <- CSV.loadFile filepath
            | Left err => putStrLn (fastConcat ["error: ", err])
        for_ rs $ \r => do
            putStr "Entry:\n"
            traverse_ putStrLn r
            putChar '\n'

