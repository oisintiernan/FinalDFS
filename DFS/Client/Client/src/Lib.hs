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
{-# LANGUAGE ScopedTypeVariables  #-}

module Lib
    ( someFunc
    ) where

import           Servant.API
import           Servant.Client-- trying to make a post request with servant!!!!
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson                   
import           Data.Aeson.TH
import           Data.Bits
import           Data.Char
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Char8   as L
import qualified Data.List                    as DL
import qualified Data.List.Split              as DLS
import           Data.Maybe                   (catMaybes)
import           Control.Monad
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager,
                                               httpLbs,
                                               parseRequest,
                                               responseBody)
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.IO
import           System.Directory
import           Data.Proxy
import           System.Posix.Files
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           UseHaskellAPI

directoryAPI :: Proxy DirectoryServerAPI
directoryAPI = Proxy

searchFiles   :: Instruction -> ClientM Location
listAllFiles  :: Instruction -> ClientM FileHere
downloadfiles :: Instruction_D -> ClientM FileData
uploadfiles   :: Instruction_U -> ClientM Bool


(searchFiles :<|> listAllFiles :<|> downloadfiles:<|> uploadfiles) = client directoryAPI

fileS_API :: Proxy UseHaskellAPI.FileServerAPI
fileS_API = Proxy

download        :: Instruction       -> ClientM FileData
update          :: Instruction_U     -> ClientM Bool
list            :: Ticket            -> ClientM FileHere
init            :: ClientM Init

( download :<|> update :<|> list :<|> init) = client fileS_API

authAPI :: Proxy UseHaskellAPI.AuthenticationAPI
authAPI = Proxy


authInit              :: SignIn       -> ClientM AuthRes
ticketGrantingService :: TGT          -> ClientM Ticket


(authInit :<|> ticketGrantingService) = client authAPI



someFunc :: IO ()
someFunc = do
  putStrLn "Welcome to Oisin Tiernans Distributed File System. Enjoy Please enter your username"
  un <- getLine
  putStrLn "Please enter your password"
  pw <- getLine
  let pu = encryptDecrypt pw un
  manager <- newManager defaultManagerSettings
  authres   <- runClientM (authInit (SignIn un pu)) (ClientEnv manager (BaseUrl Http serverAddr (authServer) ""))
  case (authres) of
	(Left err) -> do
		putStrLn "Oops, that username/password is invalid try again"
		someFunc
	(Right authInit) -> do
		let (AuthRes e_sk tgt@(TGT e_tgt e_un e_sek)) = authInit
		if (e_sk == "UI") then do
			putStrLn "Oops, that username/password is invalid try again"
			someFunc
			else do
				putStrLn "valid user, obtaining session key"
				let sk = encryptDecrypt pw e_sk
				putStrLn "Obtaining ticket"
				t   <- runClientM (ticketGrantingService (tgt)) (ClientEnv manager (BaseUrl Http serverAddr (authServer) ""))
				case (t) of
					(Left err) -> do
						putStrLn "Oops, something went wrong, please try again"
						someFunc
					(Right ticketGrantingService) -> do
						let ti@(Ticket u time s) = ticketGrantingService
						if (u == "ITGT") then do
							putStrLn "invalid tgt, try again"
							someFunc
						else do
							putStrLn "valid ticket, connecting to directory server"
							dirServ ti sk


dirServ:: Ticket -> String -> IO()
dirServ tick sk = do
	putStrLn "functions:\n    - download\n    - upload\n    - search\n    - list\n    - sign out"
	putStrLn "what function would you like to use?"
	manager <- newManager defaultManagerSettings
	fctn <- getLine
	case fctn of
		"download" -> do
			putStrLn "what file?"
			df <- getLine
			putStrLn "what file server (1/2/3)? (press 0 and search the file if unknown)"
			p ::Int <- readLn
			if (p == 0) then do
				dirServ tick sk
			else do
				let port = 8000 + p
				let e_p  = encryptPort    sk port
				checkCache tick e_p df sk
		"upload"   -> do
			putStrLn "what file?"
			uf <- getLine
			putStrLn "what file server (1/2/3)? (press 0 and search the file if unknown)"
			p ::Int <- readLn
			let por = 8000 + p
			cur_dir <- getCurrentDirectory
			let current_path = cur_dir ++ "/src/Current/" ++ uf
			conts <- readFile current_path
			let e_content = encryptDecrypt sk conts
			let e_p = encryptPort sk por
			let e_uf= encryptDecrypt sk uf
			ul tick e_p sk (FileData e_content e_uf "")
		"search"   -> do
			--authres   <- runClientM (searchFiles (SignIn un pu)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
			putStrLn "What file would you like to search for???"
			s_file <- getLine
			let e_s_file = encryptDecrypt sk s_file
			search tick e_s_file sk
		"list"     -> do
			listall1 tick sk
			--authres   <- runClientM (listAllFiles (SignIn un pu)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
			dirServ tick sk
		"sign out" -> do
			signOut
		_          -> do
			putStrLn "please enter a valid function"
			dirServ tick sk

signOut :: IO ()
signOut = do
	cur_dir <- getCurrentDirectory
	let cache_path = cur_dir ++ "/src/Cache"
	let curr_path = cur_dir ++ "/src/Current"
	removeDirectoryRecursive cache_path
	removeDirectoryRecursive curr_path
	createDirectory cache_path
	createDirectory curr_path
	putStrLn "signing out.......goodbye"

checkCache:: Ticket -> String -> String -> String-> IO()
checkCache t port name sk = liftIO $ do
	cur_dir <- getCurrentDirectory
	let cache_path = cur_dir ++ "/src/Cache/"
	ls <- listDirectory cache_path
	case (elem name ls) of
		False -> do
			putStrLn "downloading"
			let e_name = encryptDecrypt sk name
			dl t port e_name sk
		True -> do
			putStrLn "retrieving from cache"
			file_contents <- readFile (cache_path ++ name)
			let curr_path = cur_dir ++ "/src/Current/" ++ name
			writeFile curr_path file_contents
			dirServ t sk

ul:: Ticket -> String -> String -> FileData -> IO()
ul tick port sk file@(FileData c n l) = liftIO $ do
	manager <- newManager defaultManagerSettings
	cur_dir <- getCurrentDirectory
	let current_path = cur_dir ++ "/src/Current/" ++ (encryptDecrypt sk n)
	permissions <- getPermissions current_path
	let w = (show permissions)
	if (w == "Permissions {readable = True, writable = True, executable = False, searchable = False}") then do
		let lock = "W"
		file   <- runClientM (uploadfiles (Instruction_U (FileData c n lock) port tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
		case file of
			(Left err) -> do
				putStrLn "whoops something when wrong, try again"
				dirServ tick sk
			(Right uploadfiles) -> do
				putStrLn "uploading file, removing lock, removing from current directory"
				removeFile current_path
				dirServ tick sk
	else do
		let lock = "R"::String
		file   <- runClientM (uploadfiles (Instruction_U (FileData c n lock) port tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
		case file of
			(Left err) -> do
				putStrLn "whoops something when wrong, try again"
				dirServ tick sk
			(Right uploadfiles) -> do
				putStrLn "uploading file, lock will remain as user only has read privileges, removing from current directory"
				removeFile current_path
				dirServ tick sk






dl:: Ticket -> String -> String -> String-> IO()
dl t p n sk = liftIO $ do
	manager <- newManager defaultManagerSettings
	file   <- runClientM (downloadfiles (Instruction_D n p t)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
	case file of
		(Left err) -> do
			putStrLn "error, try again"
			dirServ t sk
		(Right downloadfiles) -> do
			let (FileData e_content e_name lock) = downloadfiles
			let content = encryptDecrypt sk e_content
			let name = encryptDecrypt sk e_name
			cur_dir <- getCurrentDirectory
			let path = cur_dir ++ "/src/Current/" ++ name
			let cache_path = cur_dir ++ "/src/Cache/" ++ name
			writeFile path content
			writeFile cache_path content
			if (lock == "L") then do
				setFileMode path ownerReadMode
				setFileMode cache_path ownerReadMode
				dirServ t sk
			else do
			    dirServ t sk

search:: Ticket-> String -> String -> IO()
search tick file sk = liftIO $ do
	manager <- newManager defaultManagerSettings
	file   <- runClientM (searchFiles (Instruction file tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
	case file of
		(Left err) -> do
			putStrLn "error, try again"
			dirServ tick sk
		(Right downloadfiles) -> do
			let (Location a b) = downloadfiles
			putStrLn a
			dirServ tick sk

listall1:: Ticket -> String -> IO()
listall1 tick sk = liftIO $ do
	let e_p = encryptPort sk fileserver1
	manager <- newManager defaultManagerSettings
	file   <- runClientM (listAllFiles (Instruction e_p tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
	case file of
		(Left err) -> do
			putStrLn "error, try again"
			dirServ tick sk
		(Right downloadfiles) -> do
			let (FileHere a b) = downloadfiles
			putStrLn $ "Server 1" ++ (show b)
			listall2 tick sk

listall2:: Ticket -> String -> IO()
listall2 tick sk = liftIO $ do
	let e_p = encryptPort sk fileserver2
	manager <- newManager defaultManagerSettings
	file   <- runClientM (listAllFiles (Instruction e_p tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
	case file of
		(Left err) -> do
			putStrLn "error, try again"
			dirServ tick sk
		(Right downloadfiles) -> do
			let (FileHere a b) = downloadfiles
			putStrLn $ "Server 2" ++(show b)
			listall3 tick sk

listall3::Ticket -> String  -> IO()
listall3 tick sk = liftIO $ do
	let e_p = encryptPort sk fileserver3
	manager <- newManager defaultManagerSettings
	file   <- runClientM (listAllFiles (Instruction e_p tick)) (ClientEnv manager (BaseUrl Http serverAddr (8080) ""))
	case file of
		(Left err) -> do
			putStrLn "error, try again"
			dirServ tick sk
		(Right downloadfiles) -> do
			let (FileHere a b) = downloadfiles
			putStrLn $ "Server 3" ++ (show b)
			dirServ tick sk







