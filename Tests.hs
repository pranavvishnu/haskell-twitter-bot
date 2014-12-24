{-
Author: Pranav Vishnu Ramabhadran
-}

module Tests where

import Responses
import Main
import qualified Data.ByteString as BS
import Data.Text (Text, unpack, split, pack, dropAround, toLower, words)
import Data.Aeson
import GHC.Generics
import Data.List
import qualified Data.Trie as T
import qualified Codec.Binary.UTF8.String as C
import Test.HUnit

testCleanString :: Test
testCleanString = 
	TestList [(pack "blah") ~=? cleanString (pack "    blah?,"),
			  (pack "blah blah") ~=? cleanString (pack "blah blah  ?")]

testingTrie :: T.Trie(String)
testingTrie = 
	T.fromList [(BS.pack $ C.encode "dystopia", "hunger games"),
				(BS.pack $ C.encode "jk rowling", "harry potter")]
testTrie :: Test
testTrie = 
	TestList [testingTrie 
			  ~=? 
			  addToTrie ["Dystopia, Hunger Games", "JK Rowling, Harry Potter"],
			  T.fromList [(BS.pack $ C.encode "dystopia", "hunger games")]  
			  ~=? 
			  addToTrie ["Dystopia, Hunger Games"],
			  T.fromList []  
			  ~=? 
			  addToTrie []]

testSearch :: Test
testSearch =
	TestList [Nothing
			  ~=? 
			  searchForTopic testingTrie ["hunger" , "games"] ,
			  Just "hunger games" 
			  ~=? 
			  searchForTopic testingTrie ["dystopia"],
			  Nothing
			  ~=? 
			  searchForTopic testingTrie []]

testingEmoTrie :: T.Trie(String)
testingEmoTrie = 
	T.fromList [(BS.pack $ C.encode "good", "2"),
				(BS.pack $ C.encode "just okay", "-1"),
				(BS.pack $ C.encode "worst", "-5")]
testScore :: Test
testScore =
	TestList [0
			  ~=? 
			  scoreEmotions testingEmoTrie ["hunger" , "games"] ,
			  -3
			  ~=? 
			  scoreEmotions testingEmoTrie ["good", "whatever", "worst"],
			  0
			  ~=? 
			  scoreEmotions testingEmoTrie []]

testingTweet :: Tweet
testingTweet = Tweet (pack "  ? Here is, a test Tweet?  .  ") 1
testWords :: Test
testWords =
	TestList [["here", "is", "a", "test", "tweet"]
			  ~=? 
			  getWords testingTweet]

testText :: Test
testText =
	TestList [["a", "a test", "test", "test tweet", "tweet"]
			  ~=? 
			  getText ["a", "test", "tweet"]]
