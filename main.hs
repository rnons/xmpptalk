{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad (forM, forM_, forever, when)
import           Data.Aeson (Result(..), fromJSON)
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.LocalTime
import           Data.Yaml
import           GHC.Generics (Generic)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Network.TLS ( Params(pConnectVersion, pAllowedVersions, pCiphers)
                             , Version(TLS10, TLS11, TLS12)
                             , defaultParamsClient )
import           Network.TLS.Extra (ciphersuite_medium)
import           Network.Xmpp
import           Network.Xmpp.IM 


data Account = Account
    { username       :: Text
    , password       :: Text
    } deriving (Show, Generic)

instance FromJSON Account

loadYaml :: String -> IO (HM.HashMap Text Value)
loadYaml fp = do
    mval <- decodeFile fp
    case mval of
        Nothing  -> error $ "Invalid YAML file: " ++ show fp
        Just obj -> return obj

parseYaml :: FromJSON a => Text -> HM.HashMap Text Value -> a
parseYaml key hm =
    case HM.lookup key hm of
        Just val -> case fromJSON val of
                        Success s -> s
                        Error err -> error $ "Falied to parse " 
                                           ++ T.unpack key ++ ": " ++ show err
        Nothing  -> error $ "Failed to load " ++ T.unpack key 
                                              ++ " from config file"

main :: IO ()
main = do
    config <- loadYaml "bot.yml"
    let account = parseYaml "Account" config :: Account
    result <- 
        session "google.com"
                (Just (const [plain (username account) 
                                    Nothing 
                                    (password account)], Nothing))
                def { sessionStreamConfiguration = def 
                        { tlsParams = defaultParamsClient 
                            { pConnectVersion = TLS10
                            , pAllowedVersions = [TLS10, TLS11, TLS12]
                            , pCiphers = ciphersuite_medium } } }
    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ show e
    sendPresence def sess

    startGUI Config
        { tpPort = 10005
        , tpCustomHTML = Nothing
        , tpStatic = "static"
        } $ setup sess (T.unpack $ username account)

mkElementList :: [Jid] -> String -> IO ElementList
mkElementList jids class_ = 
    forM jids $ \jid -> do
        let name = (T.unpack $ jidToText $ toBare jid)
            id_ = class_ ++ "-" ++ name
        ele <- UI.div # set UI.id_ id_
                      #. class_
                      # set html name
        return (name, ele)

setup :: Session -> String -> Window -> IO ()
setup sess accountName w = do
    return w # set title "XMPP Talk"
    UI.addStyleSheet w "default.css"

    roster <- getRoster sess
    let jids = M.keys $ items roster
    rosterList <- mkElementList jids "roster"
    tabList <- mkElementList jids "tab"
    chatList <- mkElementList jids "chat"
    channelList <- forM jids $ \jid -> do
            let name = (T.unpack $ jidToText $ toBare jid)
            chan <- Chan.newChan
            return (name, chan)

    chatting <- newIORef [] :: IO (IORef [String])
    active <- newIORef ""

    let rosterMap = HM.fromList rosterList
        tabMap = HM.fromList tabList
        chatMap = HM.fromList chatList
        channelMap = HM.fromList channelList

    tabBar <- UI.div # set UI.id_ "tab-bar"
    chatPane <- UI.div # set UI.id_ "chat-pane"
                       #. "chats"
    rosterPane <- UI.div # set UI.id_ "roster-pane"
    msgInput <- UI.textarea # set UI.id_ "msg-input"
    let 
        mkRoster :: IO Element
        mkRoster = do
            element rosterPane #+ map element (HM.elems rosterMap)

        mkTabBar :: IO Element
        mkTabBar = do
            c <- readIORef chatting
            eles <- forM tabList $ \(name, ele) -> do
                let vis = if name `elem` c then "inline-block" else "none"
                element ele # set style [("display", vis)]
            element tabBar #+ map element eles

        mkChats :: IO Element
        mkChats = do
            act <- readIORef active
            eles <- forM chatList $ \(name, ele) -> do
                let vis = if name == act then "block" else "none"
                element ele # set style [("display", vis)]
            element chatPane #+ map element eles

        redoLayout :: IO ()
        redoLayout = do
            getBody w #+
                [ mkTabBar
                , mkChats
                , mkRoster
                , element msgInput
                ]
            return ()

    forM rosterList $ \(name, ele) -> do
        on UI.click ele $ \_ -> do
            c <- readIORef chatting
            when (name `notElem` c) $ modifyIORef chatting (name:)
            modifyIORef active (const name)
            redoLayout

    on UI.sendValue msgInput $ \content -> do
        when (not (null content)) $ do
            --print content
            now <- getTime
            element msgInput # set value ""
            act <- readIORef active
            let msg = simpleIM (parseJid act) (T.pack content)
            sendMessage msg sess
            let chan = channelMap HM.! act
            Chan.writeChan chan (now, accountName, content)
    redoLayout
    forkIO $ receiveMessage sess channelMap
    updateMessage w channelList chatMap
    return ()

type TalkMessage = (String, String, String)
type ChannelList = [(String, Chan.Chan TalkMessage)]
type ChannelMap = HM.HashMap String (Chan.Chan TalkMessage) 
type ElementList = [(String, Element)]
type ElementMap = HM.HashMap String Element

updateMessage :: Window -> ChannelList -> ElementMap -> IO ()
updateMessage w channelList chatMap = do
    forM_ channelList $ \(name, chan) -> forkIO $ do
        messages <- Chan.getChanContents chan
        forM_ messages $ \msg -> do
            let ele = chatMap HM.! name
            atomic w $ do
                element ele #+ [mkMessage msg]
                UI.scrollToBottom ele
                --print messages

receiveMessage :: Session -> ChannelMap -> IO ()
receiveMessage sess channelMap = forever $ do
    msg <- getMessage sess
    let from = T.unpack $ jidToText $ toBare $ fromJust $ messageFrom msg
    when (HM.member from channelMap) $ do
        let chan = channelMap HM.! from
        now <- getTime
        case getIM msg of
            Just msg' -> 
                forM_ (map bodyContent $ imBody msg') $ \content ->
                    Chan.writeChan chan (now, from, T.unpack content)
            Nothing   -> return ()

mkMessage :: TalkMessage -> IO Element
mkMessage (timestamp, nick, content) = 
    UI.div #. "message" #+
        [ UI.div #. "name"      #+ [string $ nick ++ ": "]
        , UI.div #. "content"   #+ [string content]
        , UI.div #. "timestamp" #+ [string timestamp]
        ]
        
getTime :: IO String
getTime = do
    ztime <- getZonedTime
    return $ take 5 $ show $ localTimeOfDay $ zonedTimeToLocalTime ztime

