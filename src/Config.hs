{-# LANGUAGE OverloadedStrings #-}
-- | All built-in configuration goes here, to ease deployment and debugging.
module Config where

import Prelewd hiding (Text)

import Data.String (String)
import Data.Text
import System.IO (FilePath)
import System.Log.Logger
import Util.Defs

-- | The verbosity of messages output to stderr. This can be overridden for
--   certain components by using customLogLevels.
--
--   Possible values:
--
--   * DEBUG
--   * INFO
--   * NOTICE
--   * WARNING
--   * ERROR
--   * CRITICAL
--   * ALERT
--   * EMERGENCY
logLevel :: Priority
logLevel = DEBUG

-- | Sets custom priority thresholds for different components. For example, if
--   UI.Render is being too noisy, just add ("UI.Render", WARNING) to this list.
customLogLevels :: [(String, Priority)]
customLogLevels = [ ("Game.Texture", INFO)
                  ]

-- | The format string to use while logging.
--
--   * $msg - The actual log message
--   * $loggername - The name of the logger
--   * $prio - The priority level of the message
--   * $tid - The thread ID
--   * $pid - Process ID (Not available on windows)
--   * $time - The current time
--   * $utcTime - The current time in UTC Time
logFormat :: String
logFormat = "[$time : $loggername : $prio] $msg"

-- | The location of textures - relative to the root of the data directory.
texturePrefix :: FilePath
texturePrefix = "assets"

-- | The number of times per second to run the update loop.
targetFramerate :: Int
targetFramerate = 60

-- | The (x, y) dimensions of the screen. Eventually, this should be dynamic,
--   and full screen should be supported. Until then though, we have this
--   monstrosity.
windowDimensions :: Dimensions
windowDimensions = (800, 600)

-- | The text to display the window's title bar.
windowTitle :: Text
windowTitle = "Chess - By B & C"
