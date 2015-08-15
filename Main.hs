import Data.Map (fromList, lookup, Map, insert)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Prelude hiding (lookup)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, BufferMode(..), Handle)

type Key = String
type Value = String
type DB = Map Key Value

-------------------------------------------------------------------------------
-- Server stuff
-------------------------------------------------------------------------------

version :: String
version = "0.1.0"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = getPort args
    database <- atomically $ newTVar $ fromList [("__version__", version)]
    sock <- listenOn $ PortNumber $ fromIntegral port
    putStrLn $ "Listening on localhost:" ++ show port
    sockHandler sock database

getPort :: [String] -> Int
getPort (x:_) = read x :: Int
getPort [] = 7777

crlf :: String
crlf = "\r\n"

sockHandler :: Socket -> TVar DB -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- forkIO $ commandProcessor handle db
    sockHandler sock db

getCommand :: Handle -> Key -> TVar DB -> IO ()
getCommand handle cmd db = do
    m <- atomRead db
    let value = getValue m cmd
    hPutStr handle $ concat ["$", valLength value, crlf, value, crlf]
        where
            valLength = show . length

setCommand :: Handle -> Key -> Value -> TVar DB -> IO ()
setCommand handle key value db = do
    updateValue (insert key value) db
    hPutStr handle $ "+OK" ++ crlf

commandProcessor :: Handle -> TVar DB -> IO ()
commandProcessor handle db = do
    line <- hGetLine handle
    case words line of
        "*2":_ -> do
            _   <- hGetLine handle -- argSize
            _   <- hGetLine handle -- arg
            _   <- hGetLine handle -- keySize
            key <- hGetLine handle
            getCommand handle key db

        "*3":_ -> do
            _     <- hGetLine handle -- argSize
            _     <- hGetLine handle -- arg
            _     <- hGetLine handle -- keySize
            key   <- hGetLine handle
            size  <- hGetLine handle -- valueSize
            value <- hGetLine handle

            setCommand  handle key (take (getSize size) value) db

        _  -> hPutStrLn handle "Unknown command"
    commandProcessor handle db

getSize :: String -> Int
getSize = read . init . tail

-------------------------------------------------------------------------------
-- Data stuff
-------------------------------------------------------------------------------

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

getValue :: DB -> Key -> Value
getValue db k =
    case lookup k db of
      Just s -> s
      Nothing -> "null"
