{-# LANGUAGE OverloadedStrings #-}

module GCodeGenerator where

import AST (Block (ChangeSpindleSpeed, ChangeTool, Comment, MachineCode, Move, NoArgumentCommand, Other, OtherCommand, ToolLengthCompensation, WaitCommand), CenterOffset (CenterOffset), Delay (DelayMilliseconds, DelaySeconds), Direction (CW), Motion (ArcMove, LinearMove, RapidMove), Position (Position), Program (Program), Speed (Speed), ToolLengthCompensationDirection (Forward), unset)
import Control.Monad.State.Strict (State, gets, modify)
import Data.Text (Text, pack)
import qualified Data.Text as T

data Context = Context
  { contextLineNumber :: Int,
    contextMode :: Mode,
    contextPosition :: Position,
    contextSpeed :: Speed
  }
  deriving (Show, Eq)

-- コード生成されなくなるのを防ぐため、初期値は最初に現れないはずの値に設定
initialGeneratorContext :: Context
initialGeneratorContext = Context 10 None (Position unset unset unset) (Speed unset)

data Mode = G0 | G1 | G2 | G3 | None deriving (Show, Eq)

generateGCode :: Program -> State Context Text
generateGCode (Program blocks) = do
  case blocks of
    [] -> return ""
    (block : _) -> case block of
      Comment _ -> generateBlockCode block
      Other _ -> generateBlockCode block
      _ -> do
        lineNumberCode <- generateLineNumberCode
        codes <- mapM generateBlockCode blocks
        return $ lineNumberCode <> T.concat codes

generateLineNumberCode :: State Context Text
generateLineNumberCode = do
  lineNumber <- gets contextLineNumber
  modify $ \s -> s {contextLineNumber = lineNumber + 10}
  return $ "N" <> pack (show lineNumber)

generateBlockCode :: Block -> State Context Text
generateBlockCode (Move (RapidMove position)) = generateRapidMoveCode position
generateBlockCode (Move (LinearMove position speed)) = generateLinerMoveCode position speed
generateBlockCode (Move (ArcMove position centerOffset direction speed)) = generateArcMoveCode position centerOffset direction speed
generateBlockCode (WaitCommand delay) = return $ generateWaitCommandCode delay
generateBlockCode (NoArgumentCommand command) = return $ " " <> command
generateBlockCode (ChangeTool tool) = return $ " T" <> pack (show tool)
generateBlockCode (ChangeSpindleSpeed speed) = return $ " S" <> pack (show speed)
generateBlockCode (MachineCode code) = return $ " M" <> pack (show code)
generateBlockCode (ToolLengthCompensation direction compensation tool) = return $ generateToolLengthCompensationCode direction compensation tool
generateBlockCode (Comment comment) = return $ "(" <> comment <> ")"
generateBlockCode (OtherCommand otherCommand) = return $ "(unknown command: " <> otherCommand <> ")"
generateBlockCode (Other other) = return other

generateRapidMoveCode :: Position -> State Context Text
generateRapidMoveCode position = do
  modeCode <- generateModeCode G0
  positionCode <- generatePositionCode position
  return $ modeCode <> positionCode

generateLinerMoveCode :: Position -> Speed -> State Context Text
generateLinerMoveCode position speed = do
  modeCode <- generateModeCode G1
  positionCode <- generatePositionCode position
  speedCode <- generateSpeedCode speed
  return $ modeCode <> positionCode <> speedCode

generateArcMoveCode :: Position -> CenterOffset -> Direction -> Speed -> State Context Text
generateArcMoveCode position centerOffset direction speed = do
  modeCode <- generateModeCode (if direction == CW then G2 else G3)
  positionCode <- generatePositionCode position
  let centerOffsetCode = generateCenterOffsetCode centerOffset
  speedCode <- generateSpeedCode speed
  return $ modeCode <> positionCode <> centerOffsetCode <> speedCode

generateModeCode :: Mode -> State Context Text
generateModeCode mode = do
  prevMode <- gets contextMode
  modify $ \s -> s {contextMode = mode}
  return $ if mode == prevMode then "" else " " <> pack (show mode)

generateWaitCommandCode :: Delay -> Text
generateWaitCommandCode (DelayMilliseconds milliseconds) = " G4 P" <> pack (show milliseconds)
generateWaitCommandCode (DelaySeconds seconds) = " G4 P" <> pack (show seconds)

generateToolLengthCompensationCode :: ToolLengthCompensationDirection -> Double -> Int -> Text
generateToolLengthCompensationCode direction compensation tool =
  let modeCode = if direction == Forward then " G43" else " G44"
      compensationCode = " Z" <> pack (show compensation)
      toolCode = if tool == 0 then "" else " H" <> pack (show tool)
   in modeCode <> compensationCode <> toolCode

generatePositionCode :: Position -> State Context Text
generatePositionCode (Position x y z) = do
  Position prevX prevY prevZ <- gets contextPosition
  modify $ \s -> s {contextPosition = Position x y z}
  let resultX = if x == prevX || x == unset then "" else " X" <> pack (show x)
  let resultY = if y == prevY || y == unset then "" else " Y" <> pack (show y)
  let resultZ = if z == prevZ || z == unset then "" else " Z" <> pack (show z)
  return $ resultX <> resultY <> resultZ

-- 状態を見る必要はないためStateは使わない
generateCenterOffsetCode :: CenterOffset -> Text
generateCenterOffsetCode (CenterOffset i j) = " I" <> pack (show i) <> " J" <> pack (show j)
  -- 0の場合は省略可能だが、KitMill用のG-Codeは省略されていないためそのまま出力
  -- let resultI = if i == 0 then "" else " I" <> pack (show i)
  --     resultJ = if j == 0 then "" else " J" <> pack (show j)
  --  in resultI <> resultJ

generateSpeedCode :: Speed -> State Context Text
generateSpeedCode (Speed speed) = do
  Speed prevSpeed <- gets contextSpeed
  modify $ \s -> s {contextSpeed = Speed speed}
  let result = if speed == prevSpeed || speed == unset then "" else " F" <> pack (show speed)
  return result
