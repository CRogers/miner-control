{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP
import Data.Aeson
import Data.Aeson.Lens
import Data.List (intersperse)
import Data.Time (getCurrentTime, formatTime)
import Control.Monad
import Control.Lens ((^.))
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

devAlive :: Int -> Command Bool
devAlive devnum = Command "devs" $ decodeAlive devnum

restart :: Command ()
restart = Command "restart" $ const (Just ())

oneSecond :: Int
oneSecond = 1000 * 1000

restartIfDead :: Int -> Int -> IO ()
restartIfDead devnum port = forever $ do
	malive <- sendCommandInNewStream port (devAlive devnum)
	case malive of
		Nothing -> return ()
		Just alive ->
			if alive then return ()
			else do
				sendCommandInNewStream port restart
				time <- getCurrentTime
				let timeStr = formatTime defaultTimeLocale "%Y/%m/%e %H:%M" time
				putStrLn $ timeStr ++ " - Restarted device on port " ++ show port
	void . threadDelay $ 60 * oneSecond

waitForQuit :: IO ()
waitForQuit = do
	input <- getLine
	if input == "q" then return ()
	else waitForQuit

main :: IO ()
main = do
	args <- getArgs
	forM_ args $ \arg ->
		let (dev, port) = read arg :: (Int,Int) in
		void . forkIO $ restartIfDead dev port
	putStrLn $ "Listening on devices/ports: " ++ concat (intersperse ", " args)
	waitForQuit

removeNullTerminator :: ByteString -> ByteString
removeNullTerminator = BS.filter (/= '\0')

type RespParser r = (ByteString -> Maybe r)
data Command r = Command String (RespParser r)

instance ToJSON (Command r) where
	toJSON (Command name _) = object ["command" .= name]

sendCommandInNewStream :: Int -> Command r -> IO (Maybe r)
sendCommandInNewStream port command = do
	conn <- openStream "localhost" port
	resp <- sendCommand conn command
	close conn
	return resp

writeCommand :: HandleStream ByteString -> Command r -> IO () 
writeCommand conn = do
	void . writeBlock conn . encode

readResponse :: HandleStream ByteString -> IO (Maybe ByteString)
readResponse conn = do
	resp <- readLine conn
	return $ case resp of
		Left _ -> Nothing
		Right x -> Just $ removeNullTerminator x

sendCommand :: HandleStream ByteString -> Command r -> IO (Maybe r)
sendCommand conn command@(Command _ respParser) = do
	writeCommand conn command
	resp <- readResponse conn
	return $ resp >>= respParser
 
decodeAlive :: Int -> ByteString -> Maybe Bool
decodeAlive devnum jsonbs = do
	let getKey k = decode jsonbs ^. key "DEVS" . nth devnum . key k
	status <- getKey "Status" :: Maybe ByteString
	mhs5s <- getKey "MHS 5s" :: Maybe Double
	return (status == "Alive" && mhs5s > 0.1) 

devsJson :: ByteString
devsJson = "{\"STATUS\":[{\"STATUS\":\"S\",\"When\":1387728554,\"Code\":9,\"Msg\":\"1 GPU(s) - 0 ASC(s) - 0 PGA(s) - \",\"Description\":\"cgminer 3.7.2\"}],\"DEVS\":[{\"GPU\":0,\"Enabled\":\"Y\",\"Status\":\"Alive\",\"Temperature\":0.00,\"Fan Speed\":0,\"Fan Percent\":0,\"GPU Clock\":0,\"Memory Clock\":0,\"GPU Voltage\":0.000,\"GPU Activity\":0,\"Powertune\":0,\"MHS av\":0.00,\"MHS 5s\":0.00,\"Accepted\":0,\"Rejected\":0,\"Hardware Errors\":0,\"Utility\":0.00,\"Intensity\":\"D\",\"Last Share Pool\":-1,\"Last Share Time\":0,\"Total MH\":0.0000,\"Diff1 Work\":0,\"Difficulty Accepted\":0.00000000,\"Difficulty Rejected\":0.00000000,\"Last Share Difficulty\":0.00000000,\"Last Valid Work\":1387728160,\"Device Hardware%\":0.0000,\"Device Rejected%\":0.0000,\"Device Elapsed\":378}],\"id\":1}"