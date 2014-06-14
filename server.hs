{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

import Options.Applicative

import Control.Exception
import Control.Monad
import Control.Concurrent

import System.Environment
import System.FilePath
import Database.SQLite.Simple as SQLite

import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as ICU
import Data.Typeable
import Data.Maybe
import Data.Unique
import Data.Word
import Data.Char
import qualified Data.Aeson as Json

import Network.HTTP.Types (urlEncode)
import Network.HTTP.Client
import Network.Simple.TCP

data SkkServException
    = End
    deriving (Show, Typeable)
instance Exception SkkServException

newtype Timeout = Timeout Unique deriving (Eq, Typeable)
instance Show Timeout where show _ = "<<timeout>>"
instance Exception Timeout

data Options = Options { optJisyo :: FilePath
                       , optPort  :: Word16
                       , optCode  :: String
                       , optPlist :: Bool
                       }

main :: IO ()
main = do
    homef <- maybe id (\f -> (f </>)) <$> lookupEnv "HOME"
    execParser (opts homef) >>= \case
        Options{optPlist = True} -> writePlist
        Options{..} -> doAction optJisyo optPort optCode
  where
    opts homef = info (helper <*> options homef) fullDesc
    defOpts s  = long (map toLower s) <> short (head s) <> metavar (map toUpper s)
    options homef = Options
        <$> strOption (defOpts "file" <> value (homef ".dict.sqlite3") <> help "dict file" <> showDefaultWith id)
        <*> option    (defOpts "port" <> value 1178 <> help "port" <> showDefault)
        <*> strOption (defOpts "code" <> value "EUC-JP" <> help "charcode" <> showDefaultWith id)
        <*> switch    (long   "plist" <> help "write plist of launchd")

doAction :: FilePath -> Word16 -> String -> IO ()
doAction jisyo usePort charCode =
    SQLite.withConnection jisyo $ \conn -> 
    withManager defaultManagerSettings $ \mgr ->
    withSocketsDo $ 
    serve HostAny (show usePort) $ \(sock, _) -> do
        icu <- ICU.open charCode Nothing
        forever ( do
            mbmsg <- recv sock 10240
            maybe (throwIO End) (talk icu mgr conn sock . toKey icu) mbmsg
                `catches` (handler icu sock mbmsg)
            ) `catch` (\End -> return ())
  where
    toKey icu = T.takeWhile (/= ' ') . ICU.toUnicode icu
    handler icu sock mbmsg = 
        [ Handler $ \End -> throwIO End
        , Handler $ \e@(SomeException _) -> print e >> case mbmsg of
            Nothing  -> return ()
            Just msg ->
                send sock (ICU.fromUnicode icu $ '0' `T.cons` toKey icu msg `T.append` " \n")
        ]

talk :: ICU.Converter -> Manager -> SQLite.Connection -> Socket -> T.Text -> IO ()
talk icu mgr conn sock key
    | T.head key == '0' = throwIO End
    | T.head key == '1' = lookupCandidates False icu conn (T.tail key) mgr >>= send sock
    | T.head key == '2' = send sock "0.1 \n"
    | T.head key == '3' = send sock "127.0.0.1:0.0.0.0: \n"
    | T.head key == '4' = lookupCandidates True icu conn (T.tail key) mgr >>= send sock
    | otherwise         = return ()

formatValues :: T.Text -> [(T.Text, Maybe T.Text)] -> T.Text
formatValues k [] = '4' `T.cons` k `T.append` " \n"
formatValues _ vs = (`T.append` "/\n") . ("1/" `T.append`) . T.intercalate "/" $
    map (\(v,a) -> v `T.append` maybe T.empty (T.cons ';') a) vs

lookupCandidates :: Bool -> ICU.Converter -> Connection -> T.Text -> Manager -> IO S.ByteString
lookupCandidates cand icu conn k mgr = SQLite.withTransaction conn $ do
    i <- fromOnly . head <$> SQLite.query conn "SELECT count(*) FROM googleime WHERE key = ? LIMIT 1" (Only k)
    if (i :: Int) > 0
        then return ()
        else timeout (2 * 10 ^ (5 :: Int)) () (requestGoogleIme k mgr >>= 
            mapM_ (\r -> do
                SQLite.execute conn "INSERT OR IGNORE INTO dict (jisyo,key,value,priority) VALUES ('googleime', ?, ?, 1000)" (k,r)
                SQLite.execute conn "INSERT OR IGNORE INTO googleime VALUES (?)" (Only k)
                ))
            `catch` (\(SomeException _) -> return ())
    r <- if cand 
         then SQLite.query conn "SELECT value, annotation FROM dict WHERE key like ?" (SQLite.Only $ k `T.snoc` '%')
         else SQLite.query conn "SELECT value, annotation FROM dict WHERE key = ?"    (SQLite.Only k)
    return . ICU.fromUnicode icu $ formatValues k r

requestGoogleIme :: T.Text -> Manager -> IO [T.Text]
requestGoogleIme word mgr = do
    r <- m <$> parseUrl "http://www.google.com/transliterate?langpair=ja-Hira|ja"
    b <- responseBody <$> httpLbs r mgr
    case join $ maybeToList (Json.decode b :: Maybe [(T.Text, [T.Text])]) of
        []      -> return []
        (_,c):_ -> return c
  where
    m r = r { queryString = S.concat
        [ queryString r 
        , "&text="
        , urlEncode True (T.encodeUtf8 word)
        , ","
        ] }

timeout :: Int -> a -> IO a -> IO a
timeout t def f = do
    mv    <- newEmptyMVar
    timer <- forkIO . void $ threadDelay t >> tryPutMVar mv def
    f >>= tryPutMVar mv >> killThread timer
    readMVar mv

writePlist :: IO ()
writePlist = do
    e      <- getExecutablePath
    Just h <- lookupEnv "HOME"
    let p = h </> "Library/LaunchAgents/com.dizzy-life.hskkenv.plist"
    writeFile p (macPlist e)
    putStrLn $ "launchctl load " ++ p

macPlist :: FilePath -> String
macPlist prog = unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    , "<plist version=\"1.0\">"
    , "<dict>"
    , "  <key>KeepAlive</key>"
    , "  <true/>"
    , "  <key>Label</key>"
    , "  <string>com.dizzy-life.hskkserv</string>"
    , "  <key>ProgramArguments</key>"
    , "  <array>"
    , "    <string>" ++ prog ++ "</string>"
    , "  </array>"
    , "  <key>RunAtLoad</key>"
    , "  <true/>"
    , "  <key>WorkingDirectory</key>"
    , "  <string>/usr/local</string>"
    , "  <key>StandardErrorPath</key>"
    , "  <string>/usr/local/var/postgres/server.log</string>"
    , "</dict>"
    , "</plist>"
    ]

