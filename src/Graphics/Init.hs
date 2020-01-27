{-|
Author: Tomas Möre, 2017
Modified by: Daniel Reuter 2018
Module for initating simple window.
-}
module Graphics.Init (makeBasicWindow) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Data.StateVar hiding (get)

{-|
Creates a simple window
-}
makeBasicWindow :: String -> IO (Either String GLFW.Window)
makeBasicWindow title = do
  windowResult <- initGLFW title 600 600
  case windowResult of
    Right _ -> do
      pure windowResult
    a -> pure a

{- |
Initates GLFW with some arbitrary settings
-}

initGLFW :: String -> Int -> Int -> IO (Either String GLFW.Window)
initGLFW title width height = do
  initSuccess <- GLFW.init
  if not initSuccess
    then pure $ Left "Couldn't init glfw"
    else do
    GLFW.windowHint $ GLFW.WindowHint'Samples 1
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'RefreshRate 60
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'Resizable True
    maybeWindow <- GLFW.createWindow width height title Nothing Nothing
    case maybeWindow of
      Nothing -> pure $ Left "Couldn't create glfw window"
      Just window -> do GLFW.makeContextCurrent maybeWindow
                        GL.depthFunc $= Just GL.Lequal
                        pure $ Right window
