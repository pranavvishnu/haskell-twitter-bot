{-
Author: Pranav Vishnu Ramabhadran
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import GHC.Generics
import Control.Concurrent.Timer.Lifted
import Control.Concurrent.Suspend.Lifted
import qualified Responses as R

-- ||| Private Keys and Credentials

myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "your consumer key here"
           , oauthConsumerSecret = "your consumer secret here"
             }

mycred :: Credential
mycred = newCredential "your access token here"
                       "your access token secret here"

-- ||| Functions that interact with the Twitter API

-- | This function reads a timeline JSON and parse it using the 'Tweet' type.
timeline :: String -- ^ Screen name of the user
         -> IO (Either String [R.Tweet]) -- ^ If there is any error parsing the JSON data, it
                                       --   will return 'Left String', where the 'String'
                                       --   contains the error information.
timeline name = do
  -- Firstly, we create a HTTP request with method GET.
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  return $ eitherDecode $ responseBody res

-- | This function takes a string to tweet out and sends a POST reqeuest to do so.
tweet :: String -- ^ String to tweet out
         -> IO (Either String R.Tweet) -- ^ If there is any error parsing the JSON data, it
                                       --   will return 'Left String', where the 'String'
                                       --   contains the error information.
tweet text = do
  -- Firstly, we create a HTTP request with method POST
  req1 <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json"
  let req = urlEncodedBody [("status", pack text)] req1 -- We need to use ByteStrings here
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  return $ eitherDecode $ responseBody res

-- | This function reads the mentions JSON and parses it using the 'Tweet' type.
mentions :: String -- ^ ID of the most recent mention we've responded to
         -> IO (Either String [R.Tweet]) -- ^ If there is any error parsing the JSON data, it
                                       --   will return 'Left String', where the 'String'
                                       --   contains the error information.
mentions sid = do
  -- Firstly, we create a HTTP request with method GET.
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/mentions_timeline.json?since_id=" ++ sid
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  return $ eitherDecode $ responseBody res

-- | This function only returns mentions that we haven't yet looked at.
runMentions :: String -- ^ ID of the most recent mention we've responded to
            -> IO()   -- ^ Runs the code to check our file whether we've 
                      --   already seen something of greater ID and finally,
                      --   stores the latest ID in our current query.
runMentions sid = do
  -- First we read our file to find the most recent ID we have stored
  old <- readFile "latest.txt"
  -- First check whether we have any data stored in our local file
  if (length old > 0) 
    then do
      -- If this ID is greater than the input then use it, else use inputted ID
      let d1 = read old :: Float
      let d2 = read sid :: Float
      if (d1 > d2) 
        then do
          -- Get the list of mentions
          ets <- mentions old
          case ets of
            Left err -> print err
            Right ts -> do
              let len = length ts
              if (len > 0)
                then do
                  -- If we have any new mentions, store the ID of the latest one
                  let top = head ts 
                  writeFile "latest.txt" (show (R.id top))
                  mapM_ genResponse $ take 5 ts
                else do
                  mapM_ genResponse $ take 1 ts
        else do
          -- Get the list of mentions
          ets <- mentions sid
          case ets of
            Left err -> print err
            Right ts -> do
              let len = length ts
              if (len > 0)
                then do
                  -- If we have any new mentions, store the ID of the latest one
                  let top = head ts 
                  writeFile "latest.txt" (show (R.id top))
                  mapM_ genResponse $ take 5 ts
                else do
                  mapM_ genResponse $ take 1 ts
    else do
        -- Get the list of mentions
        ets <- mentions sid
        case ets of
          Left err -> print err
          Right ts -> do
            let len = length ts
            if (len > 0)
              then do
                -- If we have any new mentions, store the ID of the latest one
                let top = head ts 
                writeFile "latest.txt" (show (R.id top))
                mapM_ genResponse $ take 5 ts
              else do
                mapM_ genResponse $ take 1 ts

-- | Takes a tweet, finds the response and tweets it out.
genResponse :: R.Tweet -> IO (Either String R.Tweet)
genResponse twt = do
  res <- R.respond twt
  tweet res

-- | The actual work done by the main function.
runMain :: IO ()
runMain = do
  runMentions "1"
          

-- | The main function. This just sets up a Timer.
main :: IO (Timer IO)
main = do
  -- Starts a timer that runs realMain every 10 minutes
  repeatedTimer runMain (mDelay 10)
  