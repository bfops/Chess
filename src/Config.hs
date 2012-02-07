module Config ( logLevel
              ) where

import System.Log.Logger

-- | The verbosity of messages output to stderr. Possible values:
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
