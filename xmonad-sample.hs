
module Main where

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Store

main :: IO ()
main = do
  xmonad $ def {
      startupHook = do
          -- Read stored setup at startup
          readStoredProps
          autostartStored,
      
      manageHook = useStoredProps <+> manageHook def
    } `additionalKeysP` [("M-s", storeCurrentWorkspace)]

