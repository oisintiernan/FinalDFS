

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
import           Control.Monad                (when,liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Bits
import           Data.Char
import qualified Data.List.Split                    as DLS
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime, NominalDiffTime, addUTCTime)
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
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           System.Directory
import           System.Random
import           UseHaskellAPI

tgtstr         = "ticketgrantingticket"  :: String
authKey        = "222"                   :: String  



startApp :: IO () 
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8000 $ setLogger aplogger defaultSettings
  runSettings settings app


taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion


app :: Application
app = serve api server

api :: Proxy AuthenticationAPI
api = Proxy

server :: Server AuthenticationAPI
server = authInit
    :<|> ticketGrantingService
  
  where
    

    authInit :: SignIn -> Handler AuthRes
    authInit (SignIn user passUser) = liftIO $ do
      usersdoc <- readFile "../users.txt"
      let uPs = DL.words usersdoc
      warnLog passUser
      let Just a = DL.elemIndex user uPs
      let b = a + 1
      if (encryptDecrypt (sq (uPs !! b)) (sq(uPs !! a)) == passUser) then do
        let pass = (uPs !! b)
        sessionKey <- liftIO $ generateRandomString
        return $ AuthRes (encryptDecrypt pass sessionKey) (TGT (encryptDecrypt authKey tgtstr) (encryptDecrypt authKey user) (encryptDecrypt authKey sessionKey))
      else do 
        return $ AuthRes "IU" (TGT "" "" "")
      
    


    ticketGrantingService :: TGT -> Handler Ticket
    ticketGrantingService (TGT encTGT tgt_username tgt_seshkey) = liftIO $ do
      currTime <- liftIO $ getCurrentTime
      let time = addUTCTime (1800::NominalDiffTime) currTime
      let tgt_pt = encryptDecrypt authKey encTGT
      --back to just tgt
      let username = encryptDecrypt authKey tgt_username
      -- just username
      let seshkey = encryptDecrypt authKey tgt_seshkey
      -- just session key
      warnLog tgt_pt
      if (tgt_pt == tgtstr) then do
        let e_time    = encryptTime    sharedKey time
        let e_seshkey = encryptDecrypt sharedKey seshkey
        let e_user    = encryptDecrypt sharedKey username
        return $ Ticket e_user e_time e_seshkey
      else do
        return $ Ticket "ITGT" "" ""


custom404Error msg = err404 { errBody = msg }


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

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)


mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False 


mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True



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






sq :: String -> String
sq s@[c]                     = s
sq s        | last s == '"'  = init s
        | otherwise          = s

randomNum :: IO Int
randomNum = randomRIO(1,100)

generateRandomString :: IO String
generateRandomString = liftM (DL.take 10 . randomRs ('a','z')) newStdGen



