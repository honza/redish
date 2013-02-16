import Control.Concurrent.STM
import Control.Monad
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.Map (fromList, lookup, Map, insert)
import Prelude hiding (lookup)

type DB = (Map String String)

-------------------------------------------------------------------------------
-- Server stuff
-------------------------------------------------------------------------------

version :: String
version = "0.0.1"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = getPort args
    database <- atomically $ newTVar $ fromList [("__version__", version)]
    sock <- listenOn $ PortNumber $ fromIntegral port
    putStrLn $ "Listening on localhost:" ++ (show port)
    sockHandler sock database

getPort :: [String] -> Int
getPort (x:_) = read x :: Int
getPort [] = 7777

sockHandler :: Socket -> (TVar DB) -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- forkIO $ commandProcessor handle db
    sockHandler sock db

getCommand :: Handle -> String -> (TVar DB) -> IO ()
getCommand handle cmd db = do
    m <- atomRead db
    value <- getValue m cmd
    hPutStrLn handle value

setCommand :: Handle -> [String] -> (TVar DB) -> IO ()
setCommand handle cmd db = do
    appV (conv k v) db
    hPutStrLn handle "OK"
    where k = (head cmd)
          v = (unwords (tail cmd))


commandProcessor :: Handle -> (TVar DB) -> IO ()
commandProcessor handle db = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        ("get") -> getCommand handle (unwords (tail cmd)) db
        ("set") -> setCommand handle (tail cmd) db
        _ -> do hPutStrLn handle "Unknown command"
    commandProcessor handle db

-------------------------------------------------------------------------------
-- Data stuff
-------------------------------------------------------------------------------

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

conv :: String -> String -> DB -> DB
conv = insert

appV :: (DB -> DB) -> TVar DB -> IO ()
appV fn x = atomically $ modifyTVar x fn

getValue :: DB -> String -> IO (String)
getValue db k = do
    case lookup k db of
      Just s -> return s
      Nothing -> return "null"
