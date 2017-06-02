-- This file is commented extensively for non-haskell programmers

-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

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

-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager,
                                               httpLbs,
                                               parseRequest,
                                               responseBody)
import           RestClient
import           Servant
import           Servant.API                  
import           Servant.Client               
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           System.Directory
import           UseHaskellAPI




startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."
  cur_dir <- getCurrentDirectory
  let lock_path = cur_dir ++ "/src/lockedFiles"
  removeDirectoryRecursive lock_path
  createDirectory lock_path
  writeFile (lock_path++"/TF1") "example of locking"
  forkIO $ taskScheduler 5

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion




fileS_API :: Proxy UseHaskellAPI.FileServerAPI
fileS_API = Proxy

download        :: Instruction       -> ClientM FileData
update          :: Instruction_U     -> ClientM Bool
list            :: Ticket           -> ClientM FileHere
init            :: ClientM Init

( download :<|> update :<|> list :<|> init) = client fileS_API

app :: Application
app = serve api server

api :: Proxy DirectoryServerAPI
api = Proxy


server :: Server DirectoryServerAPI
server = searchFiles
    :<|> listAllFiles
    :<|> downloadfiles
    :<|> uploadfiles

  where

    searchFiles :: Instruction -> Handler Location
    searchFiles (Instruction e_f tick@(Ticket e_u e_t e_k)) = liftIO $ do
      let time = decryptTime sharedKey e_t
      curtime <- getCurrentTime
      if (time > curtime) then do
        warnLog "valid user: Start Searching"
        let sk = encryptDecrypt sharedKey e_k
        let f = encryptDecrypt sk e_f
        fs1 <- search f 8001 tick
        warnLog "Searching FS1"
        case fs1 of
          False -> do 
            fs2 <- search f 8002 tick
            warnLog "Searching FS2"
            case fs2 of
              False -> do
                fs3 <- search f 8003 tick
                warnLog "Searching FS3"
                case fs3 of
                  False -> do 
                    return $ Location "NF" ("")
                  True -> do
                    return $ Location "server 3" (show fileserver3)
              True -> do
                return $ Location "server 2" (show fileserver2)
          True-> do
            return $ Location "server 1" (show fileserver1)
      else do
        return $ Location "IT" ""

    

    
    listAllFiles :: Instruction -> Handler FileHere
    listAllFiles (Instruction e_port tick@(Ticket e_u e_t e_sk))  = liftIO $ do
      let time = decryptTime sharedKey e_t
      curtime <- getCurrentTime
      manager <- newManager defaultManagerSettings
      if (time > curtime) then do
        warnLog "valid user:List all files"
        let sk = encryptDecrypt sharedKey e_sk
        let port = decryptPort sk e_port 
        file1 <- runClientM (list (tick)) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
        case file1 of
          (Left err) -> do 
            return $ FileHere "ER" []
          (Right list)-> do
            let filehere = list
            return filehere
      else do
        return $ FileHere "IT" []

    

    downloadfiles :: Instruction_D -> Handler FileData
    downloadfiles (Instruction_D f p tick@(Ticket e_u e_t e_k)) = liftIO $ do
      let k =  encryptDecrypt sharedKey e_k
      let port = decryptPort k p
      let time = decryptTime sharedKey e_t
      curtime <- getCurrentTime
      manager <- newManager defaultManagerSettings
      if (time > curtime) then do
        warnLog "valid user: download file"
        let file = encryptDecrypt k f
        pa <- getCurrentDirectory
        let path2 = pa++"/src/lockedFiles/"
        ls <- listDirectory path2
        case (elem file ls) of
          False -> do
            file <- runClientM (download (Instruction f (tick))) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
            case file of
              (Left err) -> do
                return $ FileData "NF" "" ""
              (Right download) -> do
                let (FileData con na l) = download
                warnLog "Locking File"
                let cont = encryptDecrypt k con
                let name = encryptDecrypt k na
                writeFile (path2++name) cont
                let locked = "U"
                return $ FileData con na locked
          True -> do
            file <- runClientM (download (Instruction f (tick))) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
            case file of
              (Left err) -> do
                return $ FileData "NF" "" ""
              (Right download) -> do
                let (FileData con na l) = download
                let locked = "L"
                return $ FileData con na locked
      else do
        return $ FileData "IT" "" ""


    uploadfiles :: Instruction_U -> Handler Bool
    uploadfiles (Instruction_U filedata@(FileData ef_contents ef_name lock) p tick@(Ticket e_u e_t e_k)) = liftIO $ do
      let k =  encryptDecrypt sharedKey e_k
      let port = decryptPort k p
      let time = decryptTime sharedKey e_t
      curtime <- getCurrentTime
      manager <- newManager defaultManagerSettings
      if (time > curtime) then do
        warnLog "valid user: uploading file"
        if (lock == "W") then do
          let file = encryptDecrypt k ef_name
          warnLog "removing lock from file"
          pa <- getCurrentDirectory
          let path2 = pa ++ "/src/lockedFiles/" ++ file
          check <- doesFileExist path2
          if (check == True) then do
            removeFile path2
            file <- runClientM (update (Instruction_U filedata "" tick)) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
            case file of
              (Left err) -> do
                return False
              (Right update) -> do
                return True
          else do
            file <- runClientM (update (Instruction_U filedata "" tick)) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
            case file of
              (Left err) -> do
                return False
              (Right update) -> do
                return True
        else do
          file <- runClientM (update (Instruction_U filedata "" (tick))) (ClientEnv manager (BaseUrl Http "localhost" (port) ""))
          case file of
            (Left err) -> do
              return False
            (Right update) -> do
              return True
      else do
        return $ False


   
  

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



search:: String -> Int -> Ticket -> IO Bool
search f p tick@(Ticket u t k) = do
  manager <- newManager defaultManagerSettings
  file <- runClientM (list (tick)) (ClientEnv manager (BaseUrl Http "localhost" (p) ""))
  warnLog (show file)
  case file of
    (Left err) -> do
        return $ False
    (Right list) -> do
      let (FileHere serv fi) = list
      case (elem f fi) of
        False -> return $ False
        True  -> return $ True




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




