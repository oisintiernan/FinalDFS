{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UseHaskellAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.Bits
import           GHC.Generics
import           Servant
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Char

authServer  = 8000        :: Int
fileserver1 = 8001        :: Int
fileserver2 = 8002        :: Int
fileserver3 = 8003        :: Int
sharedKey   = "1234"      :: String
serverAddr  = "localhost" :: String



data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

data SignIn  =  SignIn { userName :: String
                       , passUser :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)




data FileData = FileData { contents :: String
                         , filen    :: String
                         , locked   :: String
                         } deriving (Generic,ToJSON,FromJSON,Show)


data FileHere = FileHere {serverName :: String
                         ,files      :: [FilePath]
                         } deriving (Generic,ToJSON,FromJSON,Show)                        
 
data Init = Init { purpose    :: String
                  ,functions  :: String
                  ,security   :: String
                 } deriving (Generic,ToJSON,FromJSON,Show)



data TGT = TGT         { tgt      :: String
                       , username :: String
                       , sKey     :: String
                       } deriving (Show, Generic, FromJSON, ToJSON) 

data AuthRes = AuthRes { sessionKey :: String
                       , tgt_enc    :: TGT
                       } deriving (Show, Generic, FromJSON, ToJSON)    

data Ticket  =  Ticket { usern    :: String
                       , expTime  :: String
                       , seshKey  :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

data Location = Location { fileserver :: String
                         , port       :: String
                         } deriving (Show, Generic, FromJSON, ToJSON)

data Instruction = Instruction { command :: String
                               , ticket  :: Ticket
                               } deriving (Show, Generic, FromJSON, ToJSON)

data Instruction_U = Instruction_U { file    :: FileData
                                   , po      :: String 
                                   , tick    :: Ticket
                                 } deriving (Show, Generic, FromJSON, ToJSON)

data Instruction_D = Instruction_D { filena  :: String
                                   , portNo  :: String
                                   , ticker  :: Ticket
                                 } deriving (Show, Generic, FromJSON, ToJSON)



data List_FileHere = List_FileHere {list_files ::[FileHere]
                                   } deriving (Show, Generic, FromJSON, ToJSON)




type DirectoryServerAPI = "searchFiles" :> ReqBody '[JSON] Instruction     :> Post '[JSON] Location
      :<|> "listAllFiles"               :> ReqBody '[JSON] Instruction     :> Post '[JSON] FileHere
      :<|> "downloadfiles"              :> ReqBody '[JSON] Instruction_D   :> Post '[JSON] FileData
      :<|> "uploadfiles"                :> ReqBody '[JSON] Instruction_U   :> Post '[JSON] Bool

type FileServerAPI = "download"         :> ReqBody '[JSON] Instruction     :> Post '[JSON] FileData
      :<|> "update"                     :> ReqBody '[JSON] Instruction_U   :> Post '[JSON] Bool
      :<|> "list"                       :> ReqBody '[JSON] Ticket          :> Post '[JSON] FileHere
      :<|> "init"                       :> Get '[JSON] Init


type AuthenticationAPI = "authInit"     :> ReqBody '[JSON] SignIn  :> Post '[JSON] AuthRes
      :<|> "ticketGrantingService"      :> ReqBody '[JSON] TGT  :> Post '[JSON] Ticket





sharedServerSecret :: String
sharedServerSecret = "This is the shared server secret."

encryptDecrypt :: String -> String -> String
encryptDecrypt key text = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) text
-- XOR each element of the text with a corresponding element of the key

encryptTime :: String  -> UTCTime  -> String
encryptTime key time = encryptDecrypt key (show(time) :: String)

decryptTime :: String  -> String  -> UTCTime
decryptTime key text = (read $ encryptDecrypt key text) :: UTCTime

encryptPort :: String  -> Int  -> String
encryptPort key port = encryptDecrypt key (show(port) :: String)

decryptPort :: String  -> String  -> Int
decryptPort key text = (read $ encryptDecrypt key text) :: Int

encryptDecryptArray :: String -> [String] -> [String]
encryptDecryptArray key array = do
  encryptedArray <- map (encryptDecrypt key) array
  return encryptedArray
