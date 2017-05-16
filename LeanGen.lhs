LeanGen - Markov Chain based Yung Lean lyrics generator
Training data is a manually generated .txt file of most of his lyrics

The text is split up into paragraphs first of all
Each paragraph is then organized whether or not it's repeated later or not
(ex: hook, refrain, chorus)
All the repeated paragraphs are concatenated into a list later, and all the
verses are also catted separately.
Each group is then fed into a Markov Chain, both on a word level
where the last 2 words determine the next word, and on a character level, where
the next sequence of characters is determined by the last n characters, where
n is the average word length

> {-# LANGUAGE QuasiQuotes                      #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Data.MarkovChain
> import System.Random (mkStdGen)
> import Text.RE.TDFA.String
> import Data.List

> type Line       = String
> type Paragraph  = [String]

Converts text into paragraphs
This way, if a paragraph's first element is a string containing [Hook:]
or something analogous, I can identify the whole paragraph as a hook

> linesToParagraph :: [Line] -> [Paragraph]
> linesToParagraph [] = []
> linesToParagraph lns
>   | null first = linesToParagraph rest
>   | otherwise  = first : linesToParagraph rest
>   where first = takeWhile(/= "") lns
>         rest  = dropWhile(== "") . drop (length first) $ lns


Identifies wheter or not a paragraph is repeated
The regex is very simple: \[.*:\]
It looks for anything between square braces with a colon
right before the closing brace

> isHook paragraph = matched $ head paragraph ?=~ [re|\[.*:\]|]
> isVerse paragraph = not (matched $ head paragraph ?=~ [re|\[.*:\]|])

Some metrics used to *hopefully* improve the quality of the generated lyrics

Lines per paragraph: used on the corpus after applying linesToParagraph

> linesPerPar :: [Paragraph] -> Int
> linesPerPar corpus =
>   let
>     lsLines = map (length) corpus
>     lsLinesFixed = filter (\x -> x /= 1) lsLines
>     avg = sum (lsLinesFixed) `div` length lsLinesFixed
>   in avg


Words per line: used on the raw text

> wordsPerLine :: String -> Int
> wordsPerLine corpus = let
>     wordCount = map (length.words) (lines corpus)
>     avg = sum (wordCount) `div` length (lines corpus)
>   in avg


Characters per word: used on the raw text

> charsPerWord :: String -> Int
> charsPerWord corpus =
>   let
>     splitWords = words corpus
>     wordLens = map (length) splitWords
>     avg = sum (wordLens) `div` length wordLens
>   in avg


Generates text using a word-level Markov Chain

> wordGen paragraphs parLines lineWords =
>   let
>     g = mkStdGen parLines
>     gen = take parLines $ run lineWords paragraphs 10 g
>   in gen


Generates text using a character-level Markov Chain

> charGen paragraphs parLines lineWords wordChars =
>   let
>     g = mkStdGen (wordChars * lineWords * parLines)
>     text = intercalate " " paragraphs
>     gen = take (wordChars * lineWords * parLines) $ run wordChars text 0 g
>   in gen


> main :: IO()
> main = do
>   rawText <- readFile "data/corpus.txt"
>   let wordChars = charsPerWord rawText
>   let lineWords = wordsPerLine rawText
>   let paragraphs = linesToParagraph $ lines rawText
>   let hooks = filter (isHook) paragraphs
>   let verses = filter (isVerse) paragraphs
>   let catHooks = concat hooks
>   let catVerses = concat verses
>   let hookParLines = linesPerPar hooks
>   let verseParLines = linesPerPar verses
>   let wordGenHook = wordGen catHooks hookParLines lineWords
>   putStrLn "Hook generated on a word level:"
>   mapM_ print wordGenHook
>   let wordGenVerse = wordGen catVerses verseParLines lineWords
>   putStrLn "Verse generated on a word level:"
>   mapM_ print wordGenVerse
>   let charGenHook = charGen catHooks hookParLines lineWords wordChars
>   putStrLn "Hook generated on a character level:"
>   print charGenHook
>   let charGenVerse = charGen catVerses verseParLines lineWords wordChars
>   putStrLn "Verse generated on a character level:"
>   print charGenVerse
