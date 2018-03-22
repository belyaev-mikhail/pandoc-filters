#!/usr/bin/env runhaskell

import Text.Pandoc.JSON
import Text.Pandoc.Readers
import Text.Pandoc.Generic
import Text.Pandoc.Options
import Text.Pandoc.Logging
import Data.Default
import Data.Text (Text)
import Data.Text as Text hiding (filter, concat, lines, map)
import Data.Text.IO as Text
import Control.Monad

import System.FilePath

import Text.Pandoc.Class
import Control.Monad.IO.Class

defaultReaderName :: String -> FilePath -> String
defaultReaderName fallback x =
  case takeExtension x of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".md"       -> "markdown"
    ".markdown" -> "markdown"
    ".muse"     -> "muse"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".org"      -> "org"
    ".lhs"      -> "markdown+lhs"
    ".db"       -> "docbook"
    ".opml"     -> "opml"
    ".wiki"     -> "mediawiki"
    ".dokuwiki" -> "dokuwiki"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
    ".docx"     -> "docx"
    ".t2t"      -> "t2t"
    ".epub"     -> "epub"
    ".odt"      -> "odt"
    ".pdf"      -> "pdf"  -- so we get an "unknown reader" error
    ".doc"      -> "doc"  -- so we get an "unknown reader" error
    _           -> fallback

stripHeader (Pandoc _ blocks) = blocks

adjustHeader adjust (Header level attrs contents) = Header (level + adjust) attrs contents
adjustHeader _ b = b

concatMapIO :: (a -> PandocIO [a]) -> [a] -> PandocIO [a]
concatMapIO f lst = concat <$> mapM f lst

readWith :: Either String (Reader PandocIO, Extensions) -> FilePath -> PandocIO [Block]
readWith (Left error) fname = 
    do 
        logOutput $ CouldNotFetchResource fname error
        return []
readWith (Right (TextReader f, exts)) fname =
    do 
        text <- liftIO $ Text.readFile fname
        pandoc <- f def{ readerExtensions = exts } text
        return $ stripHeader pandoc
readWith (Right (ByteStringReader f, exts)) fname =
    do
        bs <- readFileLazy fname
        pandoc <- f def{ readerExtensions = exts } bs
        return $ stripHeader pandoc

readFormat :: String -> FilePath -> PandocIO [Block]
readFormat fmt fname = do 
                            exists <- fileExists fname
                            if exists 
                                then do
                                        let readerSig = getReader fmt
                                        readWith readerSig fname
                                else return []

doInclude :: Block -> PandocIO [Block]
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
    if "include" `elem` classes
        then do 
                    blocks <- forM (lines contents) $ \f ->
                        do  
                            let fmt = 
                                        case lookup "format" namevals of
                                             Just format -> format
                                             Nothing -> defaultReaderName "markdown" f 
                            included <- readFormat fmt f
                            full <- bottomUpM (concatMapIO doInclude) included
                            case lookup "leveloffset" namevals of
                                 Just offset -> return $ bottomUp (adjustHeader $ read offset) full
                                 Nothing -> return full
                    return $ concat blocks
        else return [cb]
doInclude x = return [x]

main :: IO ()
main = toJSONFilter (runIOorExplode.doInclude)
