{-|
Author: Daniel Reuter
-}
module Input.Controls (
    defaultState,
    update
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Input.Events
import Input.Presets
import Program.Loop

instance HasDrawData State where
    cubes = cellset
    cameraPosition = camera  

data State = State { 
    cellset :: Cellset, 
    camera :: Camera,
    isrunning :: IsRunning
    } deriving Show

type Cellset = Set Cell
type Cell = (Float, Float)
type IsRunning = Bool
type Camera = (Float, Float, Float)

defaultState :: State
defaultState = 
    State {
        cellset = emptySet,
        camera = (0, 0, 10),
        isrunning = False
    }

{-|
Main functions
-}

updateCellState :: Cellset -> State
updateCellState cellstate = State {
    cellset = cellstate,
    camera = (0, 0, 10),
    isrunning = False
}

hasCell :: Cell -> Cellset -> Bool
hasCell cell cells = Set.member cell cells

executeCell :: Cell -> Cellset -> Cellset
executeCell cell cells = Set.delete cell cells

createCell :: Cell -> Cellset -> Cellset
createCell cell cells = Set.insert cell cells

offset :: Cell -> Cell -> Cell
offset (x, y) (dx, dy) = (x + dx, y + dy)

getAliveNeighbors :: Cell -> Cellset -> Cellset
getAliveNeighbors cellToCheck cellGrid =
    Set.filter (\cell -> hasCell cell cellGrid) (getNeighbors cellToCheck)

getActiveCells :: Cellset -> Cellset
getActiveCells cells = 
    Set.union cells $ Set.unions [getNeighbors cell | cell <- Set.toList cells]

getXCamPos :: Camera -> Float
getXCamPos (x, _, _) = x

getYCamPos :: Camera -> Float
getYCamPos (_, y, _) = y

getZCamPos :: Camera -> Float
getZCamPos (_, _, z) = z

getNeighbors :: Cell -> Cellset
getNeighbors cell = 
    Set.fromList [
        offset cell (dx, dy)
        | dx <- offsets, dy <- offsets,
        not (dx == 0 && dy == 0)
    ]
    where
        offsets = [-1, 0, 1]

survives :: Cell -> Cellset -> Bool
survives cell cells =
    if hasCell cell cells then
        livingNeighborSize == 3 || livingNeighborSize == 2
    else
        livingNeighborSize == 3
    where
        livingNeighbors = getAliveNeighbors cell cells
        livingNeighborSize = Set.size livingNeighbors

shiftCamera :: Camera -> State -> State
shiftCamera (x, y, z) state =
    State {
        cellset = (cellset state),
        camera = (getXCamPos cs + x, getYCamPos cs + y, getZCamPos cs + z),
        isrunning = runstate
        }
    where
        cs = camera state
        runstate = isrunning state

update :: Event -> State -> State
update LeftArrow state = shiftCamera (-1, 0, 0) state
update RightArrow state = shiftCamera (1, 0, 0) state
update UpArrow state = shiftCamera (0, 1, 0) state
update DownArrow state = shiftCamera (0, -1, 0) state

update (CharPress char) state
    | char == 'f' = shiftCamera (0, 0, 1) state
    | char == 'j' = shiftCamera (0, 0, -1) state
    | char == 'c' = updateCellState emptySet
    | char == '1' = updateCellState pufferOne
    | char == '2' = updateCellState pufferTwo
    | char == '3' = updateCellState gosperGliderGun
    | char == '4' = updateCellState object_117P18
    | otherwise = state

update Enter state = 
    State {
        cellset = cellset state,
        camera = camera state,
        isrunning = not $ isrunning state
        }

update Space state = 
    if stateIsRunning then
        state
    else
        if removableCell then
            State {
                cellset = removeCell,
                camera = camstate,
                isrunning = False
                }
        else
            State {
                cellset = addCell,
                camera = camstate,
                isrunning = False
                }
    where
        stateIsRunning = isrunning state
        camstate = camera state
        cellsetstate = cellset state
        addCell = createCell thisCell cellsetstate
        removeCell = executeCell thisCell cellsetstate
        removableCell = hasCell thisCell cellsetstate
        thisCell = (getXCamPos camstate, getYCamPos camstate)

update Generation state = 
    if stateIsRunning then
        State {
            cellset = nextGeneration,
            camera = camstate,
            isrunning = True
            }
    else
        state
    where
        stateIsRunning = isrunning state
        camstate = camera state
        nextGeneration = Set.filter isSurviving $ getActiveCells thisGeneration
        isSurviving = \cell -> survives cell thisGeneration
        thisGeneration = cellset state
