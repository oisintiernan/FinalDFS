{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module UseHaskellAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           UseHaskellAPI


directoryAPI :: Proxy DirectoryServerAPI
directoryAPI = Proxy

searchFiles   :: Instruction   -> ClientM Location
listAllFiles  :: Instruction       -> ClientM FileHere
downloadfiles :: Instruction_D -> ClientM FileData
uploadfiles   :: Instruction_U -> ClientM Bool


(searchFiles :<|> listAllFiles :<|> downloadfiles :<|> uploadfiles) = client directoryAPI




fileS_API :: Proxy FileServerAPI
fileS_API = Proxy

download        :: Instruction       -> ClientM FileData
update          :: Instruction_U     -> ClientM Bool
list            :: Ticket            -> ClientM FileHere
init            :: ClientM Init

( download :<|> update :<|> list :<|> init) = client fileS_API

authAPI :: Proxy AuthenticationAPI
authAPI = Proxy


authInit              :: SignIn       -> ClientM AuthRes
ticketGrantingService :: TGT          -> ClientM Ticket




(authInit :<|> ticketGrantingService) = client authAPI