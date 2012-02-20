import Control.Monad
import Data.Char
import Data.List
import qualified Data.Text as T
import System.Directory
import System.IO

{- Type Synonyms & Variables -}

type Author = String

directory = "federalist/"

filenamePrefixes :: [Author]
filenamePrefixes = ["hamilton", "madison", "unknown"]

-- | stopWords referenced from http://www.textfixer.com/resources/common-english-words.txt
stopWords = ["a", "able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your"]

{- Preprocessing -}

-- | countWordsWithPlurals takes a (large) string and produces a list of Count/Words-tuples,
--   counting both plurals and regular words
--
-- NOTE: see README for the number of words reduced in this use case by noStopWords and f (the
--       stem recognizer)
countWordsWithPlurals :: String -> [(Int, [T.Text])]
countWordsWithPlurals = map (\ws -> (length ws, ws))
                      . groupBy f
                      . map T.pack
                      . sort
                      . noStopWords
                      . alphaOnly
                      . words
                      . map toLower
  where
    -- | noStopWords removes the likely-to-be-insignificant stop words within the text
    noStopWords = filter (`notElem` stopWords)
    alphaOnly = map (filter isAlpha)
    -- | f is an (English) stem recognizer
    f a b = case T.commonPrefixes a b of
      Nothing -> False -- No common prefix, therefore this is not a (simple) plural
      Just (pre, suf1, suf2) ->
        -- Rules for (simple) English plurals, according to 
        -- http://en.wikipedia.org/wiki/English_plural
        if (suf1==suf2)
          || (suf1==T.empty && suf2==T.pack "s")
          || (suf1==T.empty && suf2==T.pack "es")
          || (suf1==T.pack "y" && suf2==T.pack "ies")
        then True
        else False

-- | redoCountWordsWithPlurals is an expensive hack, but I don't have time to build something
--   betters; after individually counting documents, we build a corpus of words and wordcounts
--   with this (again, very expensive) combining function
redoCountWordsWithPlurals :: [(Int, [T.Text])] -> [(Int, [T.Text])]
redoCountWordsWithPlurals = countWordsWithPlurals
                          . concat
                          . tail
                          . map (\x -> ' ' : T.unpack x)
                          . concat
                          . snd
                          . unzip

-- | noDots  makes sure we don't try to read `.' or `..'
noDots :: [FilePath] -> [FilePath]
noDots = filter (\x->x/="."&&x/="..")

-- | groupPapers groups papers by the same author
groupPapers :: [FilePath] -> [[FilePath]]
groupPapers = groupBy (\x y -> takeWhile isAlpha y `isPrefixOf` x)

-- | readPapers takes a sorted directory listing containing files of `$filenamePrefix[0-9]+\.txt',
--
--   This is the type I would like:
--
--     readPapers :: [FilePath] -> IO [(Author, [[(Int, [T.Text])]])]
--
--   But the liftM and mapM and fmap stuff gets confusing... so I guess we could zip in main with
--   filenamePrefixes?
--
readPapers :: [FilePath] -> IO [[[(Int, [T.Text])]]]
readPapers = mapM (mapM (fmap countWordsWithPlurals . readFile))
           . groupPapers -- [[]]
           . prependPath
           . noDots
  where
    -- | prependPath adds the directory back in for readFile
    prependPath = map (directory++)

{- Classifier -}

-- | thetas computes the likelihood of each words of a given document occuring, and takes into
--   account a given pseudocount gamma and constant multiplier k (to be determined through cross-
--   validation); equivalent to:
--
--     \hat\theta=(\hat\theta_1...\hat\theta_k) where \hat\theta_i=\frac{x_i}{x_1+...+x_k}
--
thetas :: (Integral a1, Fractional b) => a1 -> a1 -> [(a1, [a])] -> [(a, b)]
thetas k gamma ws = zip firstWords $ map (\(c,_) -> fromIntegral (c + gamma) / totalCount) ws
  where
    (counts, words) = unzip ws
    totalCount = fromIntegral $ k*gamma + sum counts
    firstWords = map head words

-- | likelihood computes the likelihood that a given document, ws, was written by the same author
--   of ds; requires a k and gamma value to pass to thetas; equivalent to:
--
--     \mathcal{l}(\theta)=\prod_{t=1}^m p(x_t|\theta)
--
--likelihood :: (Integral a1, Fractional b) => a1 -> a1 -> [(a1, [a])] -> [(a1, [a])] -> [b]
{-
likelihood k gamma ds ws = map (\w -> foldr ((*) . (w `given` (thetas k gamma))) 1 ds') ws
  where
    ds' = snd $ unzip ds
    given (c,words) b =
      case (T.unpack $ head words) `lookup` uncurry (\theta words ->
          zip (map (T.unpack . head) words) theta) b of
        Nothing -> 0.0
        Just t -> c * t -- NOTE: revise
-}

{- Cross-validation -}

-- | crossValidate takes a learning algorithm, a classifier, and a list of examples
crossValidate l xs = map f [n-a | a <- [1..n-1]] where
  n = length xs
  f i = l training tests where (training, tests) = splitAt i xs

{- Main -}

main :: IO ()
main = do
  files <- getDirectoryContents directory
  -- NOTE: the following comprehension is a hack, since I can't figure out the correct way to
  --       fulfill the type I want for readPapers
  docs':_ <- readPapers files
  docs <- zipWithM (curry return) (noDots files) docs'
  corpusWords' <- foldM (\w (filename, words) -> do
      let wordCount = length words
      hPutStrLn stderr $ show wordCount++" unique words in `"++filename++"'"
      return (w++words)
    ) [] (take 30 docs) -- NOTE: this isn't the most elegant way to do it, but I am short on time
                        --       and I know the first 30 are *not* unknown
  let corpusWords = redoCountWordsWithPlurals corpusWords'
  let corpusWordCount = length corpusWords
  hPutStrLn stderr $ "\n"++show corpusWordCount++" unique words across all documents\n"
  -- NOTE: same comment about take 30 as above
  let grouped = uncurry (\files words -> zip (groupPapers files) words) . unzip $ take 30 docs
  -- NOTE: same comment about take 30 as above
  let grouped' = uncurry (\files words -> zip files words) . unzip $ take 30 docs
  -- NOTE: same comment about take 15 as above
  let hamilton = snd . unzip $ take 15 grouped'
  let madison = snd . unzip . take 15 $ drop 15 grouped'
  mapM_ (hPutStrLn stderr . show . crossValidate (likelihood 1 0)) hamilton
  docThetas <- forM grouped (\(file, words) -> do
    let docThetas = thetas 1 0 words
    hPutStrLn stderr $ "Author: "++show file
    hPutStrLn stderr $ show docThetas
    return (file, docThetas)
    )
  hPutStrLn stderr $ "Going"
