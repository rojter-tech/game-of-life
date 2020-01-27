module Main where
{-|
Author: Tomas Möre
Modified by: Daniel Reuter
-}
import Program.Loop

import qualified Graphics.Cube as Cube
import qualified Graphics.Init as Init
import qualified Graphics.Shaders as Shaders
import qualified Graphics.UI.GLFW as GLFW

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad

import Input.Controls

import System.Directory

vertexShader :: String
vertexShader = "shaders/cube.vert"
fragmentShader :: String
fragmentShader =  "shaders/cube.frag"

main :: IO ()
main = do
  renderDataEither <- loadProgram
  case renderDataEither  of
    Right (window, renderData) -> do
      runProgram window
                 renderData
                 defaultState
                 update
    Left e -> putStrLn e

loadProgram :: IO (Either String (GLFW.Window, Cube.RenderData))
loadProgram = runExceptT $ do
  window <- ExceptT $ Init.makeBasicWindow "rOjterGameOfLife"
  liftIO $ GLFW.makeContextCurrent (Just window)
  vertexExists <- liftIO $ doesFileExist vertexShader
  unless vertexExists $ throwE ("Vertex shader file doesn't exist: " ++ vertexShader)
  fragmentExists <- liftIO $ doesFileExist fragmentShader
  unless fragmentExists $  throwE ("fragment shader file doesn't exist: " ++ fragmentShader)
  baseProgram <- ExceptT $ Shaders.loadProgram vertexShader fragmentShader
  renderData <- liftIO $ Cube.initRenderData baseProgram
  pure (window, renderData)
