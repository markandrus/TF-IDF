Document Classification using Term Frequency--Inverse Document Frequency
========================================================================

This is a document classifier which parses a selection of Alexander Hamilton's and James
Madison's contributions to the Federalist Papers, in order to determine the authorship of eleven
disputed documents.

Roughly, the idea is to take all the documents of known authorship and to generate a signature for
them based on the ${lt*idf}$ of each term in the corpus. Then, for each document of unknown
authorship, we want to compare the frequency of terms appearing in said document against the
signatures we know. We expect the signature which aligns the closest with a document to indicate
the authorship of said document.

Implementation
--------------

### Preprocessing

We do all of the following in order to reduce the number of non-essential variables ultimately
observed by our classifier.

1. Lowercase all letters

2. Filter non-alphabetical characters (such as quotation marks, hyphens, etc.)
 * Both of these yield easier comparison between words

3. Consider all pluralized words equivalent, according to
   [English pluralization rules](http://en.wikipedia.org/wiki/English_plural)

4. Remove all stop words as defined
   [here](http://www.textfixer.com/resources/common-english-words.txt)

### Process

The program is invoked with `runhaskell Main.hs`. Upon execution, the program:

1. Groups all documents in `federalist' by author into three groups (according to filename)
2. Preprocesses and prints the number of unique words found for each document (according to
   criteria in section "Preprocessing" (NOTE: we go ahead and calculate for the known and unknown
   documents--keeping them in memory--rather than recalculate the unkowns later)
3. Counts all unique words in the corpus (NOTE: Hamilton's and Madison's words, excluding the
   unknown documents) (also follows the criteria in section "Preprocessing")
4. We cross-validate for k and gamma individually for Hamilton and Madison
5. Calculates the thetas (using the k and gamma we acquired from cross-validation in step 4.)
   corresponding to the frequency of each word which occurs in a given text
 * Specifically, we print the "dictionary" and each word's theta value for both Hamilton and
   Madison's corpus-es
6. Calculate the likelihood that each of the unknown texts is either Hamilton or Madison, printing
   the classifications and likelihoods as it processes

The comments in the code explain the functioning of these parts.

Results
-------

The algorithm attributes each of the unknown documents to Alexander Hamilton. This is consistent
statistical analysis of the Federalist Papers.

Todo
----

* Implement cross-validation to ensure that some fluke is not responsible for our attribution of
	every document to Hamilton
* Add other test data
