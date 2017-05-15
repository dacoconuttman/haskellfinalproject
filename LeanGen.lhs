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
> isVerse paragraph = not( matched $ head paragraph ?=~ [re|\[.*:\]|])




> main :: IO()
> main = do
>   rawText <- readFile "data/corpus.txt"
>   let paragraphs = linesToParagraph $ lines rawText
>   print $ head paragraphs
>   let hooks = filter (isHook) paragraphs
>   let verses = filter (isVerse) paragraphs
>   print $ head hooks
>   print $ head verses
>   let g = mkStdGen 100
>   putStrLn $ "Character by character: \n"
>   putStrLn $ take 1000 $ run 3 rawText 0 g
>   putStrLn $ "\nWord by word: \n"
>   putStrLn $ unwords $ take 100 $ run 2 (words rawText) 0 g
