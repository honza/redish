import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans (liftIO)
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

version = "0.0.1"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    database <- atomically $ newTVar $ fromList [("__version__", version)]
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on localhost:" ++ (head args)
    sockHandler sock database

sockHandler :: Socket -> (TVar DB) -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle db
    sockHandler sock db

getCommand :: Handle -> String -> (TVar DB) -> IO ()
getCommand handle cmd db = do
    m <- atomRead db
    value <- getValue m cmd
    hPutStrLn handle $ value

setCommand :: Handle -> [String] -> (TVar DB) -> IO ()
setCommand handle cmd db = do
    d <- atomRead db
    x <- atomically $ setValue db d (head cmd) (unwords (tail cmd))
    hPutStrLn handle $ "OK"

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

atomRead = atomically . readTVar

getValue :: DB -> String -> IO (String)
getValue db k = do
    case lookup k db of
      Just s -> return s
      Nothing -> return "null"

setValue :: (TVar DB) -> DB -> String -> String -> STM ()
setValue db map k v = do
    writeTVar db $ insert k v map
