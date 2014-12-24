{-
Author: Pranav Vishnu Ramabhadran
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Responses where

import qualified Data.ByteString as BS
import Data.Text (Text, unpack, split, pack, dropAround, toLower, words)
import Data.Aeson
import GHC.Generics
import Data.List
import qualified Data.Trie as T
import qualified Codec.Binary.UTF8.String as C
import Test.HUnit


-- | Type for tweets. Use only the fields you are interested in.
--   The parser will filter them. To see a list of available fields
--   see <https://dev.twitter.com/docs/platform-objects/tweets>.
data Tweet =
  Tweet { text :: !Text,
          id   :: Int
          } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

-- ||| Functions related to Tries and building responses.

-- | Removes spaces and punctuation from around the words and converts to lowercase
cleanString :: Text -> Text
cleanString = toLower . dropAround (==' ') . dropAround (== '?') . dropAround (== ',') . dropAround (== '.') . dropAround (== '!')

-- | Keeps adding elements to Trie until there are no more.
addToTrie :: [String] -> T.Trie (String)
addToTrie inp = 
	(go inp T.empty)
	where
		go :: [String] -> T.Trie (String) -> T.Trie (String)
		go [] trie  = trie
		go (x:xs) t = 
			let 
			new1 :: [String]
			new1 = map (unpack . cleanString) $ split (==',') (pack x) in 
			let 
			temp :: BS.ByteString
			temp = BS.pack $ C.encode $ new1 !! 0 in
			let 
			temp2 :: String
			temp2 = new1 !! 1 in
			let 
			new2 :: (BS.ByteString, String)
			new2 = (temp, temp2) in
			go xs (T.insert temp temp2 t)

-- | Iterates through a list of words until it sees a known topic.
searchForTopic :: T.Trie(String) -> [String] -> Maybe String
searchForTopic topics tweet
	| T.null topics = Nothing
	| otherwise   =
		go topics tweet
		where
			go :: T.Trie(String) -> [String] -> Maybe String
			go _    []      = Nothing
			go tpcs (x:xs)  = 
				case (T.lookup (BS.pack $ C.encode x) tpcs) of
					Just a  -> Just a
					Nothing -> go tpcs xs

-- | Creates an emotion score for a given tweet
scoreEmotions :: T.Trie(String) -> [String] -> Int
scoreEmotions emotions tweet
	| T.null emotions = 0
	| otherwise       =
		go emotions tweet 0
		where
			go _    []   score  = score
			go emts (x:xs) scr  = 
				case (T.lookup (BS.pack $ C.encode x) emts) of
					Just a  -> go emts xs (scr + (read a))
					Nothing -> go emts xs scr

-- | Builds a Trie from file for topics
buildTopics :: IO (T.Trie(String))
buildTopics = do
	file <- readFile "topics.txt"
	return $ addToTrie (lines file)

-- | Builds a Trie from file for responses
buildResponses :: IO (T.Trie(String))
buildResponses = do
	file <- readFile "responses.txt"
	return $ addToTrie (lines file)

-- | Builds a Trie from file for emotion score
buildEmotions :: IO (T.Trie(String))
buildEmotions = do
	file <- readFile "emotions.txt"
	return $ addToTrie (lines file)

-- ||| Functions related to actually taking the tweet and creating a response.

-- | Gets just the words as Strings from a Tweet. Gets rid of empty strings
--   and extraneous symbols as well as converts everything to lowercase.
getWords :: Tweet -> [String]
getWords twt = filter (/="") $ 
	map (unpack . cleanString) (Data.Text.words (text twt))

-- | Create combinations of words that occur together
getText :: [String] -> [String]
getText []       = []
getText [x]      = [x]
getText (x:y:xs) = x:(x ++ " " ++ y):(getText (y:xs))


-- | Functions that take in topic info and repond with with appropriate levels
--   of enthusiasm.
responseHappy1 :: String -> String
responseHappy1 text = "Ehhh, " ++ text ++ " is okay I guess #whatever"

responseHappy2 :: String -> String
responseHappy2 text = "Whoooo, " ++ text ++ " is so great! #killinit"

responseHappy3 :: String -> String
responseHappy3 text = "OMG, " ++ text ++ " is literally the best ever #besthingever"

responseSad1 :: String -> String
responseSad1 text = "Uhhh, " ++ text ++ " is whatever #whocares"

responseSad2 :: String -> String
responseSad2 text = "Ugh, " ++ text ++ " is kinda bad #neveragain"

responseSad3 :: String -> String
responseSad3 text = "Just no. " ++ text ++ " was absolutely the worst thing to happen to humanity"

-- | The function that ties everything together. We take in a tweet and spit out
--   an appropriate response.
respond :: Tweet -> IO(String)
respond tweet = do
	topics    <- buildTopics
	responses <- buildResponses
	emotions  <- buildEmotions
	text <- return $ getText $ getWords tweet
	emo  <- return $ scoreEmotions emotions text
	case (searchForTopic topics text) of
		Nothing -> return "Sadly, I have no idea what you're talking about"
		Just a  -> do
			case (searchForTopic responses [a]) of 
				Nothing -> return "Sadly, I have no idea what you're talking about"
				Just r  -> 
					if      (emo >= 4)  then return $ responseHappy3 r
					else if (emo >= 2)  then return $ responseHappy2 r 
					else if (emo >= 0)  then return $ responseHappy1 r 
					else if (emo >= -2) then return $ responseSad1   r 
					else if (emo >= -4) then return $ responseSad2   r 
					else    				 return $ responseSad3   r 

