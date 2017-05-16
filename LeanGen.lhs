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

> linesPerPar corpus = do
>   let lsLines = map (length) corpus
>   let lsLinesFixed = filter (\x -> x /= 1) lsLines
>   let avg = sum (lsLinesFixed) `div` length lsLinesFixed
>   return avg


Words per line: used on the raw text

> wordsPerLine corpus = do
>   let wordCount = map (length.words) (lines corpus)
>   let avg = sum (wordCount) `div` length (lines corpus)
>   return avg


Characters per word: used on the raw text

> charsPerWord corpus = do
>   let splitWords = words corpus
>   let wordLens = map (length) splitWords
>   let avg = sum (wordLens) `div` length wordLens
>   return avg


Generates text using a word-level Markov Chain

> wordGen paragraphs parLines lineWords = do
>   let g = mkStdGen parLines
>   let text = intercalate " " paragraphs
>   let gen = take parLines $ run (words lineWords) text 0 g
>   return gen


Generates text using a character-level Markov Chain

> charGen paragraphs parLines lineWords wordChars = do
>   let g = mkStdGen (wordChars * lineWords * parLines)
>   let text = intercalate " " paragraphs
>   let gen = take (wordChars * lineWords * parLines) $ run wordChars text 0 g
>   return gen


> main :: IO()
> main = do
>   rawText <- readFile "data/corpus.txt"
>   let wordChars = charsPerWord rawText
>   let lineWords = wordsPerLine rawText
>   let paragraphs = linesToParagraph $ lines rawText
>   let hooks = filter (isHook) paragraphs
>   let verses = filter (isVerse) paragraphs
>   let hookParLines = linesPerPar hooks
>   let verseParLines = linesPerPar verses
>   let wordGenHook = wordGen hooks hookParLines lineWords
>   putStrLn "Hook generated on a word level:"
>   print wordGenHook
>   let wordGenVerse = wordGen verses verseParLines lineWords
>   putStrLn "Verse generated on a word level:"
>   print wordGenVerse
>   let charGenHook = charGen hooks hookParLines lineWords wordChars
>   putStrLn "Hook generated on a character level:"
>   print charGenHook
>   let charGenVerse = charGen verses verseParLines lineWords wordChars
>   putStrLn "Verse generated on a character level:"
>   print charGenVerse
