{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.Log
  ( initLogger ) where

import           Control.Monad             (liftM)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Configurator
import           Paths_snaplet_hslogger
import           Snap.Snaplet
import           System.Log
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

-- | Initialize the Logger Snaplet.
--
-- No custom `v` type here (just Unit) because the logger state is
-- globally maintained anyway and this snaplet is just providing an
-- easy default configuration for HSLogger.
initLogger :: SnapletInit b ()
initLogger = makeSnaplet "hslogger" description datadir $ do
    conf <- getSnapletUserConfig

    logfile   <- liftIO $ require conf "log_file"
    loglevel  <- liftIO $ require conf "log_level"
    logformat <- liftIO $ require conf "log_format"
    logname   <- liftIO $ require conf "default_logger"

    let lvl = (read loglevel) :: Priority
    h <- liftIO $ fileHandler logfile lvl >>= \lh -> return $
        setFormatter lh (simpleLogFormatter logformat)

    liftIO $ updateGlobalLogger logname (addHandler h)
    liftIO $ updateGlobalLogger logname (setLevel lvl)

    liftIO $ noticeM logname $ "Configured the logger with a level of: " ++ (show loglevel)

  where
    description = "Snaplet for HSLogger library"
    datadir = Just $ liftM (++"/resources/hslogger") getDataDir
