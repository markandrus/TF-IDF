import Control.Monad
import Data.Char
import Data.Function (on)
import Data.List
import Data.Ord (comparing)
import Data.Maybe
import Text.Printf
import System.Directory
import System.IO

type Author = String
type Corpus = [(String, Int)]
type TFIDF = [(String, Float)]

dir :: FilePath
dir = "federalist/"

authors :: [Author]
authors = ["hamilton", "madison", "unknown"]

-- | stopWords referenced from http://www.textfixer.com/resources/common-english-words.txt
stopWords = ["a", "able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your"]

-- |noDots removes the `.' and `..' directory listings
noDots :: [FilePath] -> [FilePath]
noDots = filter (\x -> x/="." && x/="..")

-- |commonPrefix returns the common prefix of two lists
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix x y = fst . unzip . fst . span (uncurry (==)) $ zip x y

-- |groupPapers groups papers by the same author
groupPapers :: [FilePath] -> [[FilePath]]
groupPapers = groupBy (\x y -> any (`isPrefixOf` commonPrefix x y) authors)

-- |isPlural determines whether the latter of two (English) words is a (simple) pluralization of
--  the first
isPlural :: String -> String -> Bool
isPlural a b = case commonPrefix a b of
  "" -> False
  pre -> (suf1==""  && suf2=="s")
      || (suf1==""  && suf2=="es")
      || (suf1=="y" && suf2=="ies")
      -- NOTE: I didn't catch this until later, but consider the fact that
      --       `sort ["apply","applies"]` produces `["applies","apply"]`
      || (suf2==""  && suf1=="s")
      || (suf2==""  && suf1=="es")
      || (suf2=="y" && suf1=="ies")
    where
      suf1 = drop (length pre) a
      suf2 = drop (length pre) b

-- |isSameOrPlural determines whether the two words are the same or if the latter of the two is a
--  (simple, English) pluralization of the first
isSameOrPlural :: String -> String -> Bool
isSameOrPlural a b = (a==b) || isPlural a b

-- |uniques takes a list of words and returns a list of word/count-tuples, taking into account
--  (simple) English pluralizations
makeCorpus :: [String] -> Corpus
makeCorpus = map (\(x:_) -> (x, length x))
           . groupBy isSameOrPlural
           . filter (`notElem` stopWords)
           . sort
           . map (map toLower . filter isAlpha)

-- |unionUniques takes two lists of word/count-tuples and combines them, taking into account
--  (simple) English pluralizations
unionCorpus :: [Corpus] -> Corpus
unionCorpus (x:xs) = foldr (\a b -> reduce $ a++b) x xs
  where
    -- |reduce combines the counts of identical and plural words
    reduce :: Corpus -> Corpus
    reduce = map (foldr (\(w',c') (w,c) -> (w',c+c')) ("",0))
           . groupBy (isSameOrPlural `on` fst)
           . sortBy (comparing fst)

-- |makeTFIDF computes the "term frequency--inverse document frequency" of a Corpus given the
--  per-document Corpuses which compose it
makeTFIDF :: Corpus -> [Corpus] -> TFIDF
makeTFIDF corpus corpusPerDoc = normalize
                              $ map (\(word,count) ->
                                      ( word
                                        -- tf * idf
                                      , fromIntegral count * docCount
                                          / (1.0 + countOccurs word corpusPerDoc)
                                      )
                                    ) corpus
  where
    -- |normalize normalizes the frequencies
    normalize tfidf = let m = sum . snd $ unzip tfidf in map (\(w,f) -> (w,f/m)) tfidf
    -- |countOccurs, given a word, and a list of corpuses, sums the number of occurences of said
    --  word, returning it as a Float
    countOccurs :: String -> [Corpus] -> Float
    countOccurs word = fromIntegral
                     . foldr ((+) . fromJust) 0
                     . filter isJust
                     . map (lookup word)
    -- |docCount is the number of documents as a Float
    docCount :: Float
    docCount = fromIntegral $ length corpusPerDoc

main :: IO ()
main = do
  files <- (zip authors . groupPapers . noDots) `liftM` getDirectoryContents dir
  corpusPerAuthor <- forM files (\(author, docs) -> do

      hPrintf stderr "Author: %s\n  Files: %d\n" author (length docs)

      (wordsPerDoc, corpusPerDoc) <- unzip `liftM`
        forM docs (\filePath -> do
            wordsFromFile <- words `liftM` readFile (dir++filePath)
            let corpus = makeCorpus wordsFromFile
            hPrintf stderr "    File: %s\n" filePath
            hPrintf stderr "      Words:   %d\n" (length wordsFromFile)
            hPrintf stderr "      Uniques: %d\n" (length corpus)
            return (wordsFromFile, corpus)
          )

      let totalWords = foldr ((+) . length) 0 wordsPerDoc
      hPrintf stderr "  Total Words:   %d\n" totalWords

      let corpus = unionCorpus corpusPerDoc
      hPrintf stderr "  Total Uniques: %d\n\n" (length corpus)

      return (author, (corpus, corpusPerDoc))
    )

  let authorTFIDFs = map (\(author, (corpus, corpusPerDoc)) ->
                           ( author
                           , makeTFIDF corpus corpusPerDoc
                           )
                         ) corpusPerAuthor

  forM_ authorTFIDFs (\(author, tfidf) -> do
      hPrintf stderr "TFIDF for Author: %s\n" author
      forM_ (sortBy (comparing snd) tfidf) (\(word, freq) -> do
          hPrintf stderr "  Frequency: %f  Word: %s\n" freq word
        )
      hPutStrLn stderr ""
    )

  -- NOTE: we know the first two
  let known = take 2 authorTFIDFs :: [(Author, TFIDF)]

  putStrLn "Analysis of unknown documents"
  forM_ (zip (fromJust $ lookup "unknown" files)
             (snd . fromJust $ lookup "unknown" corpusPerAuthor)
        ) (\(file, corpus) -> do
      printf "  File: %s\n" file
      let m = fromIntegral (sum . snd $ unzip corpus) :: Float
      let corpus' = map (\(word, count) -> (word, fromIntegral count / m)) corpus
      let fromJust' j | isNothing j = 0
                      | otherwise = fromJust j
      let differences = sortBy (comparing snd)
                      . zip authors
                      $ map (\(author, authorTFIDF) ->
                              foldr (\(word,freq) a ->
                                      a + (fromJust' $ lookup word authorTFIDF) - freq
                                    ) 0.0
                              -- NOTE: when using intersectBy, the element from the first list is
                              --       the one retained
                            $ intersectBy (\(word,_) (authorWord,_) ->
                                            word `isSameOrPlural` authorWord
                                          ) corpus' authorTFIDF
                            ) known
      forM_ (differences) (\(author,diff) -> do
          printf "      Author: %s\n        Difference: %f\n" author diff
        )
      printf "   Chose: %s\n\n" . fst $ head differences
    )
