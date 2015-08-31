import Data.Map (fromList, lookup, Map, insert)
import Data.Char (toLower)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPutStr, hIsEOF,
                  BufferMode(..), Handle)

type Key = String
type Value = String
type DB = Map Key Value
data Command = Get Key
             | Set Key Value

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

runCommand :: Handle -> Maybe Command -> TVar DB -> IO ()
runCommand handle (Just (Get key)) db = do
    m <- atomRead db
    let value = getValue m key
    hPutStr handle $ concat ["$", valLength value, crlf, value, crlf]
        where
            valLength = show . length
runCommand handle (Just (Set key value)) db = do
    updateValue (insert key value) db
    hPutStr handle $ "+OK" ++ crlf
runCommand _ Nothing _ = do
  return ()

stripCrlf :: String -> String
stripCrlf = init

getSize :: String -> Maybe Int
getSize = readMaybe . stripCrlf . tail

getArgSize :: Handle -> IO (Maybe Int)
getArgSize h = do
  s <- hGetLine h
  return $ getSize s

getArgSize2 :: Handle -> MaybeT IO Int
getArgSize2 h = MaybeT $ getArgSize h

getArg :: Handle -> MaybeT IO String
getArg h = MaybeT $ do
  arg <- hGetLine h
  return $ Just (stripCrlf arg)

processGet :: Handle -> IO (Maybe Command)
processGet h = runMaybeT $ do
  argSize <- getArgSize2 h
  arg <- getArg h
  -- liftIO $ print argSize

  guard $ argSize == length arg
  guard $ (map toLower arg) == "get"

  keySize <- getArgSize2 h
  key <- getArg h

  guard $ keySize == length key

  return $ Get key

processSet :: Handle -> IO (Maybe Command)
processSet h = runMaybeT $ do
  argSize <- getArgSize2 h
  arg <- getArg h
  guard $ argSize == length arg
  guard $ arg == "set"

  keySize <- getArgSize2 h
  key <- getArg h
  guard $ keySize == length key

  valueSize <- getArgSize2 h
  value <- getArg h
  guard $ valueSize == length value

  return $ Set key value

decideCommand :: Handle -> IO (Maybe Command)
decideCommand h = runMaybeT $ do
  arg <- getArgSize2 h
  case arg of
    2 -> do
      c <- MaybeT $ processGet h
      return c
    3 -> do
      c <- MaybeT $ processSet h
      return c
    _ -> do
        lift $ hPutStrLn h (concat ["-ERR Unknown command", crlf])
        MaybeT $ return Nothing

commandProcessor :: Handle -> TVar DB -> IO ()
commandProcessor handle db = do
  ineof <- hIsEOF handle
  if ineof
     then return ()
     else do
        command <- decideCommand handle
        runCommand handle command db
        commandProcessor handle db

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
