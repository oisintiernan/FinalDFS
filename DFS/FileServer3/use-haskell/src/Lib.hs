{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib
    ( startApp
    ) where



import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.Char
import           Data.Bits
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import qualified Data.List.Split              as DLS
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Directory
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           UseHaskellAPI



serverN        = "File Server 3"         :: String

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8003 $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy FileServerAPI
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server FileServerAPI
server = download
    :<|> update
    :<|> list
    :<|> init

  where



    download:: Instruction -> Handler FileData -- fns with no input, second getREADME' is for demo below
    download (Instruction e_file (Ticket e_u e_t e_sk)) = liftIO $ do
      warnLog "got here"
      let u    = encryptDecrypt sharedKey e_u
      let t    = encryptDecrypt sharedKey e_t
      let sk   = encryptDecrypt sharedKey e_sk
      let file = encryptDecrypt sk e_file
      warnLog file
      pa <- getCurrentDirectory
      pa <- getCurrentDirectory
      let path2 = pa++"/src/TF/"
      let filep = (path2++ file) ::FilePath
      file_there <- doesFileExist (filep)
      case file_there of
        False -> do
          warnLog "sending empty file"
          return $ FileData "" "" ""
        True  -> do
          warnLog "found file sending file"
          content <- readFile $ path2++file
          let e_content = encryptDecrypt sk content
          let _file     = encryptDecrypt sk file
          return $ FileData e_content _file "" 
    
    update:: Instruction_U -> Handler Bool
    update (Instruction_U (FileData e_con e_fn _) _ (Ticket e_u e_t e_sk)) = liftIO $ do
      warnLog "got here"
      let u     = encryptDecrypt sharedKey e_u
      let t     = encryptDecrypt sharedKey e_t
      let sk    = encryptDecrypt sharedKey e_sk
      let con   = encryptDecrypt sk e_con
      let fn    = encryptDecrypt sk e_fn
      pa <- getCurrentDirectory
      let path2 = pa++"/src/TF/"
      let filep = (path2++ fn) ::FilePath
      file_there <- doesFileExist (filep)
      case file_there of
        False -> do
          warnLog "writing new file"
          writeFile filep con
          return True
        True  -> do
          warnLog "overwriting file"
          writeFile filep con
          return True


    list:: Ticket -> Handler FileHere
    list (Ticket e_u e_t e_sk) = liftIO $ do
      warnLog $ "looking in directory"
      let u = encryptDecrypt sharedKey e_u
      let t = encryptDecrypt sharedKey e_t
      let sk = encryptDecrypt sharedKey e_sk
      case t of
        "0" -> return $ FileHere "" []
        _ -> do
          pa <- getCurrentDirectory
          let path2 = pa++"/src/TF/"
          contents <- getDirectoryContents path2
          return $ FileHere serverN contents

    init:: Handler Init
    init = liftIO $ do
      warnLog $ "- Explaining to client the general operations performed with this file server\n"
      let purpose = "- This company file server is set up such that a user can extract read and write to files, this will allow users to work on projects simultaeneously\n"
      let security = "The Security System is as follows\n        - To use this files server you will need to enter your user name and password to verify your identity\n        - Information sent between servers and between servers and clients will also be encrypted"
      let functions = "The functions available are as follows\n        - View Projects: TYPE view\n        - Read Files: TYPE read\n        - Update Files: Not made yet\n"
      return $ Init purpose functions security



-- What follows next is some helper function code that makes it easier to do warious things such as use
-- a mongoDB, post console log statements define environment variables for use in your programmes and so forth.
-- The code is not written particularly to be understood by novice Haskellers, but should be useable relatively easily
-- as set out above.

-- | error stuff
custom404Error msg = err404 { errBody = msg }

sq :: String -> String
sq s@[c]                     = s
sq s        | last s == '"'  = init s
        | otherwise          = s


splitFP :: String -> [String]
splitFP fp = do
  DLS.splitOn "/" fp




-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def




