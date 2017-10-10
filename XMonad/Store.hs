{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable #-}
module XMonad.Store (
    -- * Data types
    StoredProperties (..),
    -- * Store configuration
    storeWorkspace, storeCurrentWorkspace, storeAllWorkspaces,
    -- * Startup
    readStoredProps, autostartStored,
    -- * ManageHooks
    useStoredProps,
    -- * Utilities
    selectQuery
  )
  where

import Control.Exception
import Control.Applicative
import Control.Monad
import System.FilePath
import System.FilePath.Glob
import System.Environment
import Data.Maybe
import Data.List
import Data.Yaml
import Data.Typeable

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Util.WindowProperties
import XMonad.AppGroups (oneOf)
import XMonad.Utils

data StoredProperties = StoredProperties {
    windowClass :: String,
    windowTitleRE :: String,
    windowCommand :: String,
    runOnStart :: Bool,
    runOnSelect :: Bool,
    targetWorkspace :: WorkspaceId }
  deriving (Eq, Show, Read, Typeable)

instance ToJSON StoredProperties where
  toJSON ps = object ["class" .= windowClass ps,
                      "title" .= windowTitleRE ps,
                      "command" .= windowCommand ps,
                      "autostart" .= runOnStart ps,
                      "run" .= runOnSelect ps,
                      "workspace" .= targetWorkspace ps ]

instance FromJSON StoredProperties where
  parseJSON (Object o) =
    StoredProperties
      <$> o .: "class"
      <*> o .:? "title" .!= ".*"
      <*> o .:? "command" .!= ".*"
      <*> o .:? "autostart" .!= False
      <*> o .:? "run" .!= False
      <*> o .: "workspace"
  parseJSON x = fail $ "Invalid object for stored window properties: " ++ show x

instance ExtensionClass [StoredProperties] where
  initialValue = []

getWindowTitle :: Window -> X String
getWindowTitle w = do
    d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catch` \(SomeException _) -> getTextProperty d w wM_NAME
        extract prop = do l <- wcTextPropertyToTextList d prop
                          return $ if null l then "" else head l
    io $ bracket getProp (xFree . tp_value) extract `catch` \(SomeException _) -> return ""

getWindowClass :: Window -> X String
getWindowClass win = do
    cls <- withDisplay $ \d -> fmap resName $ io $ getClassHint d win
    return cls

getWindowTitleRE :: Window -> X String
getWindowTitleRE win = do
  title <- getWindowTitle win
  let ws = words title
  let w = if length ws == 1
            then title
            else head ws ++ " .*"
  return w 

getWindowCommand :: Window -> X String
getWindowCommand win = do
  mbPid <- getProp32s "_NET_WM_PID" win
  case mbPid of
    Just [pid] -> do
                  cmd <- io $ readFile ("/proc/" ++ show pid ++ "/cmdline")
                  return $ filter (/= '\0') cmd
    _  -> return ""

processCommand :: Query String
processCommand = do
  window <- ask
  liftX $ getWindowCommand window

getWindowProps :: WorkspaceId -> Window -> X StoredProperties
getWindowProps wksp win = do
  cls <- getWindowClass win
  tre <- getWindowTitleRE win
  cmd <- getWindowCommand win
  return $ StoredProperties cls tre cmd False False wksp

getStoredQry :: StoredProperties -> Query Bool
getStoredQry props =
    title ~? windowTitleRE props
    <&&>
    className ~? windowClass props
    <&&>
    processCommand ~? windowCommand props

workspaceWindowProps :: WorkspaceId -> X [StoredProperties]
workspaceWindowProps wksp = do
  ws <- windowsOnWorkspace wksp
  forM ws $ \win -> getWindowProps wksp win

storeCurrentWorkspace :: X ()
storeCurrentWorkspace = do
  getCurrentWorkspace >>= storeWorkspace

storeWorkspace :: WorkspaceId -> X ()
storeWorkspace wksp = do
  home <- io $ getEnv "HOME"
  let path = home </> ".xmonad" </> "store" </> (wksp ++ ".workspace")
  props <- workspaceWindowProps wksp
  io $ encodeFile path props

storeAllWorkspaces :: X ()
storeAllWorkspaces = do
  ws <- withWindowSet $ \s -> return $ W.workspaces s
  forM_ ws $ \wksp -> do
      storeWorkspace (W.tag wksp)

readStoredProps :: X ()
readStoredProps = do
  home <- io $ getEnv "HOME"
  let mask = home </> ".xmonad" </> "store" </> "*.workspace"
  paths <- io $ glob mask
  props <- forM paths $ \path -> do
               x <- io $ decodeFile path
               return $ fromMaybe [] (x :: Maybe [StoredProperties])
  XS.put $ concat props
  -- io $ print $ concat props

useStoredProps :: ManageHook
useStoredProps = do
    window <- ask
    props <- liftX $ XS.get
    go props window
  where
    go [] _ = doF id
    go (p:ps) window = do
      let qry = getStoredQry p
      matching <- liftX $ runQuery qry window
      if matching
        then createAndMove True Nothing (targetWorkspace p)
        else go ps window

getConfiguredWorkspaces :: X [WorkspaceId]
getConfiguredWorkspaces = do
  props <- XS.get
  return $ nub $ sort $ map targetWorkspace props

propsForWorkspace :: WorkspaceId -> X [StoredProperties]
propsForWorkspace wksp = do
  props <- XS.get
  return props

queryForWorkspace :: WorkspaceId -> Query Bool
queryForWorkspace wksp = do
  props <- liftX $ XS.get
  let ps = [p | p <- props, targetWorkspace p == wksp]
  oneOf $ map getStoredQry ps

autostartStored :: X ()
autostartStored = do
  props <- XS.get
  forM_ (props :: [StoredProperties]) $ \ps -> do
    when (runOnStart ps) $
      spawn (windowCommand ps)

selectQuery :: GSConfig WorkspaceId -> X ()
selectQuery gsc = do
  workspaces <- getConfiguredWorkspaces
  let pairs = [(wksp, wksp) | wksp <- workspaces]
  selected <- gridselect gsc pairs
  whenJust selected $ \wksp -> do
    let qry = queryForWorkspace wksp
    wins <- matchingWindows qry
    if null wins
      then do
           props <- propsForWorkspace wksp
           forM_ props $ \ps -> do
               when (runOnSelect ps) $ 
                 spawn (windowCommand ps)
      else do
           addWorkspace wksp
           forM_ wins $ \w -> windows (W.shiftWin wksp w)

