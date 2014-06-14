{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.Directory
import Control.Applicative
import Control.Monad
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.ByteString as S
import qualified Data.Text as T

processLine :: T.Text -> [(T.Text, T.Text, Maybe T.Text)]
processLine l
    | T.null l        = []
    | T.head l == ';' = []
    | otherwise = 
        let (k, vs) = T.break (== ' ') l
            sp      = filter (not . T.null) . T.split (== '/') $ T.tail vs
            spp     = map (T.break (== ';')) sp
        in map (\(v,a) -> (k,v,if T.null a then Nothing else Just (T.tail a))) spp

processFile :: FilePath -> IO [(T.Text, T.Text, Maybe T.Text)]
processFile file = do
    icu <- ICU.open "EUC-JP" Nothing
    concatMap processLine . T.lines . ICU.toUnicode icu <$> S.readFile file

createDB :: SQLite.Connection -> IO ()
createDB conn = do
    SQLite.execute_ conn "PRAGMA journal_mode = WAL"
    SQLite.execute_ conn "CREATE TABLE dict (id INTEGER PRIMARY KEY AUTOINCREMENT, jisyo TEXT NOT NULL, key TEXT NOT NULL, value TEXT NOT NULL, annotation Text, priority INTEGER NOT NULL, UNIQUE (key, value))"
    SQLite.execute_ conn "CREATE TABLE googleime (key TEXT PRIMARY KEY)"
    SQLite.execute_ conn "CREATE INDEX dict_key_index ON dict(key)"

main :: IO ()
main = getArgs >>= \case
    db:files -> doesFileExist db >>= \dbe -> SQLite.withConnection db $ \conn -> do
        unless dbe $ createDB conn
        SQLite.withTransaction conn $
            forM_ (map splitPriority files) $ \(f,p) -> processFile f >>= 
                mapM_ (\(k,v,a) -> SQLite.execute conn "INSERT OR IGNORE INTO dict (jisyo,key,value,annotation,priority) VALUES (?,?,?,?,?)" (f,k,v,a, maybe 100 id p))
    _ -> putStrLn "db file:priority.."

splitPriority :: String -> (FilePath, Maybe Int)
splitPriority s = case break (== ':') s of
    (f, []) -> (f, Nothing)
    (f, p)  -> (f, Just .read $ tail p)
