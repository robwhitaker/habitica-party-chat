{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Protolude                      hiding (State, state)

import           Data.Aeson                     (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Types               as Aeson
import qualified Data.Map.Strict                as Map
import           Data.UUID                      (UUID)
import qualified Data.UUID                      as Uuid
import qualified Data.Yaml                      as Yaml

import qualified Network.HTTP.Req               as Req
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import qualified Web.Scotty                     as Scotty

type ClientId = UUID

data Auth = Auth
    { authHabiticaId     :: ClientId
    , authHabiticaApiKey :: UUID
    } deriving (Generic)

instance FromJSON Auth

data Client = Client
    { clientHabiticaId :: ClientId
    , clientConnection :: WS.Connection
    }

data Config = Config
    { confPort    :: Int
    , confGroupId :: UUID
    , confDomain  :: Text
    }

instance FromJSON Config where
    parseJSON = Aeson.withObject "Config" $ \o ->
        Config
            <$> o .: "port"
            <*> o .: "groupId"
            <*> o .: "domain"

newtype HabiticaResponse = HabiticaResponse
    { hrSuccess :: Bool }

instance FromJSON HabiticaResponse where
    parseJSON = Aeson.withObject "HabiticaResponse" (.: "success")

data InboundMessage = InboundMessage
    { inMsgGroupId :: UUID
    , inMsgSender  :: ClientId
    , inMsgText    :: Text
    }

instance FromJSON InboundMessage where
    parseJSON = Aeson.withObject "InboundMessage" $ \o -> do
        chatData <- o .: "chat"
        InboundMessage
            <$> chatData .: "groupId"
            <*> chatData .: "uuid"
            <*> chatData .: "text"

data OutboundMessage = OutboundMessage
    { outMsgSender :: ClientId
    , outMsgText   :: Text
    } deriving (Generic)

instance ToJSON OutboundMessage

type State = [Client]

main :: IO ()
main = do
    config <- Yaml.decodeFileThrow ".config.yaml"
    state <- newMVar []
    httpApp <- scottyApp state config
    Warp.run 3000 $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp state config)
        httpApp

scottyApp :: MVar State -> Config -> IO Wai.Application
scottyApp stateRef config = Scotty.scottyApp $ do
    Scotty.get "/" $ do
        Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
        Scotty.file "build/index.html"

    Scotty.post "/" $ do
        mbMsg <- Aeson.decode' <$> Scotty.body
        case mbMsg of
            Nothing -> do
                Scotty.status Http.badRequest400
                Scotty.finish

            Just msg -> do
                when (inMsgGroupId msg /= confGroupId config) $ do
                    Scotty.status Http.unauthorized401
                    Scotty.finish
                liftIO $ broadcast (inMsgSender msg) stateRef (inMsgText msg)

wsApp :: MVar State -> Config -> WS.ServerApp
wsApp stateRef config pending =
    validateClientConnection pending config >>= maybe (return ()) initClient
  where
    initClient client = do
        let (clientId, conn) = (clientHabiticaId client, clientConnection client)
        joinClient client stateRef
        WS.withPingThread conn 30 (return ()) $
            finally
                (forever $ void (WS.receiveDataMessage conn))
                (disconnectClient clientId stateRef)

validateClientConnection :: WS.PendingConnection -> Config -> IO (Maybe Client)
validateClientConnection pending config
    | WS.requestPath reqHead /= "/" = do
        WS.rejectRequest pending "Unexpected request path"
        return Nothing
    | Map.notMember "Host" headerMap = do
        WS.rejectRequest pending "Missing \"Host\" header"
        return Nothing
    | Map.lookup "Host" headerMap /= Just (toS confHost) = do
        WS.rejectRequest pending "Tried to connect from unexpected host"
        return Nothing
    -- TODO: Check for secure connection (wss://)?
    | otherwise = do
        -- TODO: this creates a connection but potentially discards the reference
        --       (if user validation fails). Not sure how to handle this yet.
        conn <- WS.acceptRequest pending
        authReq <- WS.receiveData conn
        maybe (return Nothing) (authorizeClient conn) (Aeson.decode' authReq)
  where
    reqHead = WS.pendingRequest pending
    headerMap = Map.fromList $ WS.requestHeaders reqHead
    confHost = confDomain config <> ":" <> show (confPort config)

    authorizeClient conn auth = do
        -- TODO: defaultHttpConfig might explode on failure and leak credentials
        --       into the terminal. Not sure.
        habiticaResponse <- Req.runReq Req.defaultHttpConfig $ do
            v <- Req.req
                    Req.GET
                    (Req.https "habitica.com/api/v3/user/anonymized")
                    Req.NoReqBody
                    Req.jsonResponse
                    ( mconcat
                        [ Req.header "x-api-user" (toS $ Uuid.toString $ authHabiticaId auth)
                        , Req.header "x-api-key" (toS $ Uuid.toString $ authHabiticaApiKey auth)
                        -- TODO: x-client
                        ]
                    )
            return $ Aeson.parseMaybe Aeson.parseJSON $ Req.responseBody v

        case hrSuccess <$> habiticaResponse of
            Just True ->
                return . Just $ Client
                    { clientHabiticaId = authHabiticaId auth
                    , clientConnection = conn
                    }

            _ -> return Nothing

joinClient :: Client -> MVar State -> IO ()
joinClient client stateRef =
    modifyMVar_ stateRef $ return . (client :)

withoutClient :: ClientId -> State -> State
withoutClient clientId =
    filter $ (/=clientId) . clientHabiticaId

disconnectClient :: ClientId -> MVar State -> IO ()
disconnectClient clientId stateRef =
    modifyMVar_ stateRef (return . withoutClient clientId)

broadcast :: ClientId -> MVar State -> Text -> IO ()
broadcast clientId stateRef msg = do
    clients <- withoutClient clientId <$> readMVar stateRef
    forM_ clients $ \client ->
        WS.sendTextData (clientConnection client) (Aeson.encode $ OutboundMessage clientId msg)
