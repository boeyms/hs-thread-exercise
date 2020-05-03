{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad (forever, forM_)
import Core.Program
import Core.System
import Core.Text
import Data.Time.Clock

foreverWriteTimeTo :: MVar String -> Program None ()
foreverWriteTimeTo mv = forever $ do
    sleep 5
    now <- liftIO getCurrentTime
    _ <- liftIO $ tryTakeMVar mv
    liftIO $ putMVar mv $ show now

foreverPrintFrom :: MVar String -> Program None ()
foreverPrintFrom mv = forever $ do
    -- s <- liftIO $ readMVar mv
    maybeString <- liftIO $ tryReadMVar mv
    forM_ maybeString $ write . intoRope
    sleep 2

program :: Program None ()
program = do
    stringMVar <- liftIO newEmptyMVar
    _ <- fork $ foreverPrintFrom stringMVar
    foreverWriteTimeTo stringMVar

main :: IO ()
main = execute program

-- vim: sw=4 sts=4 ts=4 et
