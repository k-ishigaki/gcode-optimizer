{-# LANGUAGE OverloadedStrings #-}

module GCodeParser where

import AST (Block (ChangeSpindleSpeed, ChangeTool, Comment, MachineCode, Move, NoArgumentCommand, Other, OtherCommand, ToolLengthCompensation, WaitCommand), CenterOffset (CenterOffset), Delay (DelayMilliseconds, DelaySeconds), Direction (CCW, CW), Motion (ArcMove, LinearMove, RapidMove), Position (Position, positionX, positionY, positionZ), Program (Program), Speed (Speed), ToolLengthCompensationDirection (Backward, Forward), unset)
import Control.Applicative (empty)
import Control.Monad.State.Strict (MonadTrans (lift), State, gets, modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    ParsecT,
    anySingle,
    choice,
    manyTill,
    optional,
    skipSome,
    (<|>),
  )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (WriterT [Text] (State MoveContext))

logMessage :: Text -> Parser ()
logMessage msg = lift $ tell [msg]

-- Move Context (G-Code modal 1 type address)
data MoveContext = MoveContext
  { mode :: MoveMode,
    position :: Position,
    speed :: Speed
  }
  deriving (Show, Eq)

data MoveMode = G0 | G1 | G2 | G3 deriving (Show, Eq)

initialContext :: MoveContext
initialContext = MoveContext G0 (Position unset unset unset) (Speed unset)

parseProgram :: Parser Program
parseProgram = do
  blocks <- manyTill (parseBlock <* sc) eof
  return $ Program blocks

parseBlock :: Parser Block
parseBlock = do
  parsedBlock <- try parseMove <|> try parseWait <|> try parseComment <|> try parseNoArgumentCommand <|> try parseToolChange <|> try parseChangeSpindleSpeed <|> try parseMachineCode <|> try parseToolLengthCompensation <|> try parseOtherCommand <|> parseOther
  logMessage $ pack (show parsedBlock)
  return parsedBlock

parseMove :: Parser Block
parseMove = do
  _ <- optional lineNumber
  codeMode <- parseMode -- 移動モード（G0など）
  -- モードに応じてパース
  motion <- case codeMode of
    -- 高速移動
    G0 -> do
      RapidMove <$> parsePosition
    -- 直線移動
    G1 -> do
      previousPos <- gets position
      previousSpeed <- gets speed
      codePos <- optional parsePosition
      codeSpeed <- optional parseFeedRate
      if (codePos, codeSpeed) == (Nothing, Nothing)
        then fail "either position or speed is required"
        else return $ LinearMove (fromMaybe previousPos codePos) (fromMaybe previousSpeed codeSpeed)
    -- 円弧移動
    _ -> do
      previousPos <- gets position
      previousSpeed <- gets speed
      codePos <- optional parsePosition
      offset <- parseCenterOffset -- どんな場合でもIかJは必須
      codeSpeed <- optional parseFeedRate
      return $ ArcMove (fromMaybe previousPos codePos) offset (if codeMode == G2 then CW else CCW) (fromMaybe previousSpeed codeSpeed)
  return $ Move motion

-- 停止命令のパース
parseWait :: Parser Block
parseWait = do
  _ <- optional lineNumber
  _ <- symbol "G4"
  _ <- symbol "P" <|> symbol "X"
  delaySeconds <- try $ lexeme (optional L.float)
  delayMilliSeconds <- lexeme L.decimal
  let delay = case delaySeconds of
        Just s -> DelaySeconds s
        Nothing -> DelayMilliseconds delayMilliSeconds
  return $ WaitCommand delay

lineNumber :: Parser ()
lineNumber = symbol "N" *> (L.decimal :: Parser Int) *> sc

-- G0などのパース（前回の値を引き継ぐ都合で常に成功するため注意）
parseMode :: Parser MoveMode
parseMode = do
  modeFromCode <- optional (symbol "G0" <|> symbol "G1" <|> symbol "G2" <|> symbol "G3")
  currentMode <- gets mode
  let resolvedMode = case modeFromCode of
        Just "G0" -> G0
        Just "G1" -> G1
        Just "G2" -> G2
        Just "G3" -> G3
        _ -> currentMode
  -- 決定されたモードを保持しておく
  modify $ \s -> s {mode = resolvedMode}
  return resolvedMode

-- 位置のパース（X/Y/Zどれも指定されていない場合のみエラー）
parsePosition :: Parser Position
parsePosition = do
  codeX <- optional $ symbol "X" *> parseDouble
  codeY <- optional $ symbol "Y" *> parseDouble
  codeZ <- optional $ symbol "Z" *> parseDouble

  currentPosition <- gets position

  let resolvedX = fromMaybe (positionX currentPosition) codeX
  let resolvedY = fromMaybe (positionY currentPosition) codeY
  let resolvedZ = fromMaybe (positionZ currentPosition) codeZ

  modify $ \s -> s {position = Position resolvedX resolvedY resolvedZ}

  case (codeX, codeY, codeZ) of
    (Nothing, Nothing, Nothing) -> fail "either x/y/z is required"
    _ -> return $ Position resolvedX resolvedY resolvedZ

-- 円弧中心オフセットのパース（I/Jどちらも指定されていない場合のみエラー）
parseCenterOffset :: Parser CenterOffset
parseCenterOffset = do
  i <- optional $ symbol "I" *> parseDouble
  j <- optional $ symbol "J" *> parseDouble
  case (i, j) of
    (Nothing, Nothing) -> fail "either i/j/k is required"
    -- IとJは指定されない場合は0となる（X/Y/Zとは違い前回の値を引き継がない）
    _ -> return $ CenterOffset (fromMaybe 0 i) (fromMaybe 0 j)

-- フィードレートのパース（指定されていない場合エラー）
parseFeedRate :: Parser Speed
parseFeedRate = do
  codeSpeed <- optional $ symbol "F" *> parseDouble

  currentSpeed <- gets speed

  let resolvedSpeed = maybe currentSpeed Speed codeSpeed

  modify $ \s -> s {speed = resolvedSpeed}

  case codeSpeed of
    Nothing -> fail "speed is unspecified"
    Just s -> return $ Speed s

parseComment :: Parser Block
parseComment = do
  _ <- symbol "("
  content <- manyTill anySingle (symbol ")")
  return $ Comment (pack content)

parseNoArgumentCommand :: Parser Block
parseNoArgumentCommand = do
  _ <- optional lineNumber
  command <- choice (map symbol ["G90", "G17", "G20", "G21", "G54", "G94", "G95", "G49", "G0", "G1", "G2", "G3"])
  return $ NoArgumentCommand command

parseToolChange :: Parser Block
parseToolChange = do
  _ <- optional lineNumber
  _ <- symbol "T"
  toolNumber <- lexeme L.decimal
  return $ ChangeTool toolNumber

parseChangeSpindleSpeed :: Parser Block
parseChangeSpindleSpeed = do
  _ <- optional lineNumber
  _ <- symbol "S"
  codeSpeed <- lexeme L.decimal
  return $ ChangeSpindleSpeed codeSpeed

parseMachineCode :: Parser Block
parseMachineCode = do
  _ <- optional lineNumber
  _ <- symbol "M"
  code <- lexeme L.decimal
  return $ MachineCode code

parseToolLengthCompensation :: Parser Block
parseToolLengthCompensation = do
  _ <- optional lineNumber
  code <- symbol "G43" <|> symbol "G44"
  compensation <- symbol "Z" *> parseDouble
  toolNumber <- optional $ symbol "H" *> lexeme L.decimal
  let tool = fromMaybe 0 toolNumber
  return $ case code of
    "G43" -> ToolLengthCompensation Forward compensation tool
    _ -> ToolLengthCompensation Backward compensation tool

parseOtherCommand :: Parser Block
parseOtherCommand = do
  lineNumber
  text <- sc *> manyTill anySingle eof
  return $ OtherCommand (pack text)

parseOther :: Parser Block
parseOther = do
  text <- sc *> manyTill anySingle eof
  return $ Other (pack text)

parseDouble :: Parser Double
parseDouble = lexeme (L.signed sc (try L.float <|> fmap (fromIntegral :: Integer -> Double) L.decimal))

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (skipSome spaceWithoutNewline) empty empty
  where
    spaceWithoutNewline = char ' ' <|> char '\t'
