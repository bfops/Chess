-- | All built-in configuration goes here, to ease deployment and debugging.
module Config ( logLevel
              , customLogLevels
              , logFormat
              , texturePrefix
              ) where

import System.Log.Logger

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
customLogLevels = []

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
