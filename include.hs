#!/usr/bin/env runhaskell

import Text.Pandoc.JSON
import Text.Pandoc.Readers
import Text.Pandoc.Generic
import Text.Pandoc.Options
import Data.Default
import Data.Text (Text)
import Data.Text as Text hiding (filter, concat, lines)
import Data.Text.IO as Text
import Control.Monad

import Text.Pandoc.Class
import Control.Monad.IO.Class

stripHeader (Pandoc _ blocks) = blocks

concatMapIO :: (a -> PandocIO [a]) -> [a] -> PandocIO [a]
concatMapIO f lst = concat <$> mapM f lst

readMD :: FilePath -> PandocIO [Block]
readMD fname = do
                    exists <- fileExists fname
                    if exists 
                        then do
                                text <- liftIO $ Text.readFile fname
                                md <- readMarkdown def{readerExtensions = getDefaultExtensions "markdown"} text
                                return $ stripHeader md
                        else return []

doInclude :: Block -> PandocIO [Block]
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
    if "include" `elem` classes
        then do 
                    blocks <- forM (lines contents) $ \f ->
                        do  
                            md <- readMD f
                            bottomUpM (concatMapIO doInclude) md
                    return $ concat blocks
        else return [cb]
doInclude x = return [x]

main :: IO ()
main = toJSONFilter (runIOorExplode.doInclude)
