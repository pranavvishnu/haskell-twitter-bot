Haskell Twitter Bot by Pranav Vishnu Ramabhadran
========================================

What it is:
-----------
A Twitter bot. You can tweet from it, read the timeline, look for mentions etc. and respond. The fun stuff has to do with the responses since I've cooked up an "emotional response engine" (it isn't real if it doesn't have a buzzword-filled name) that basically tried to respond with appropriate enthusiasm/disdain depending on what you tweet at it. So saying "OMG I really love Harry Potter" would get an equally enthusaistic response, while saying something like "I hate the hunger games" would get an equally acerbic response. At the moment, I've just typed out a few random inputs this understands that have to do with Harry Potter and the Hunger Games, but this is built for efficiency with a large number of inputs since I'm using Tries for lookup.

How to use it:
--------------

First, you need to go to Twitter's Dev console from the account you want to use as a bot, and get the Consumer Key, Consumer Secret for your account. Then you'll also need to get an Access Token and Secret for this specific project. add those into the Main file in the places marked out for it.

Fire up GHCI and run `:load Main`. You might need to install `http-conduit` and `authenticate-oauth` before this. You also will need to install `split` and `bytestring-trie`. Type in `timeline [username]` to see the most recent 5 tweets from a specific user. Type in `tweet [string]` to tweet out whatever string you provide from your Twitter account. Even better (and most importantly), type in `runMain` to look at the 5 most recent mentions (this is arbitrary and could just respond to all mentions instead, or any other number) and respond to them automatically using the bot's special "emotional response engine". Make sure you tweet something new at the bot (preferably regarding Harry Potter or The Hunger Games) before running this (to avoid Tweitter API violations)!


How it works:
-------------

0. Build up Tries for `topics.txt`, `response.txt` and `emotions.txt`. These files are simply Key-Value Pairs separated by commas. These tries will store trimmed, lowercase versions of these pairs stripped of common punctuation like periods, commas, question marks and exclamation marks at either end of the string.
1. Hit Twitter's API for a list of most recent mentions. Only ask for mentions after the tweet with ID as saved in the file `latest.txt` or if it's empty, ID `1`, hence returning all mentions. 
2. For each tweet:
    - Create a list of all the words in the tweet and every pair of words that occur together.
    - Read through this list, looking for a match in our `topics` trie. If we find a match, then we can build up an appropiate response using the `response` trie, else we just return a standard response saying that we couldn't understand the tweet.
    - If we find a match, now look through the same list of words, but check for words that map to an `emotion` score, thus building up an emotion score.
    - Using the emotion score and the tweet response, tweet out the appropriate response using the `tweet` function.