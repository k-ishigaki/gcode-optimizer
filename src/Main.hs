{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
  ( decodeUtf8C,
    encodeUtf8C,
    evalStateC,
    linesUnboundedC,
    mapC,
    mapMC,
    runConduitRes,
    runStateC,
    sinkFile,
    sourceFile,
    unlinesC,
    (.|),
  )
import Control.Monad.State.Strict (MonadState (get, put), StateT, runState, execStateT)
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Text (Text, dropWhileEnd, pack)
import GCodeParser (MoveContext, initialContext, parseProgram)
import Text.Megaparsec
  ( errorBundlePretty,
    runParserT,
  )
import AST (Program (Program), Block (Other), Position (Position))
import GCodeGenerator (Context, generateGCode, initialGeneratorContext)
import Optimizer (OptimizerContext, optimizeProgram, initialOptimizerContext)
import TimeEstimator (initialEstimatorContext, EstimatorContext (estimatorContextTotalTime, estimatorContextSegments), estimateProgram, totalMinTime, computeLineEndAngles, SphericalAngle (sphericalAngleAzimuth))

-- 1行をパースし、状態を更新しつつ処理
processLine :: (Monad m) => Text -> StateT MoveContext m Program
processLine line = do
  currentState <- get
  let ((result, _), finalState) = runState (runWriterT (runParserT parseProgram "" line)) currentState
  case result of
    Left err -> return $ Program [Other $ pack $ "Error : " ++ errorBundlePretty err]
    Right prog -> do
      put finalState
      return prog

optimizeProgramLine :: (Monad m) => Program -> StateT OptimizerContext m Program
optimizeProgramLine prog = do
  currentState <- get
  let (optimizedProgram, finalState) = runState (optimizeProgram prog) currentState
  put finalState
  return optimizedProgram

generateCodeLine :: (Monad m) => Program -> StateT Context m Text
generateCodeLine prog = do
  currentState <- get
  let (code, finalState) = runState (generateGCode prog) currentState
  put finalState
  return code

estimateTime :: (Monad m) => Program -> StateT EstimatorContext m Program
estimateTime prog = do
  currentState <- get
  let ((), finalState) = runState (estimateProgram prog) currentState
  put finalState
  return prog


-- 改行文字をトリムする関数
trimCRLF :: Text -> Text
trimCRLF = dropWhileEnd (`elem` ['\r', '\n'])

-- ファイル全体を処理
processFile :: FilePath -> IO ()
processFile filePath = do
  ((), state) <- runConduitRes $
    runStateC initialEstimatorContext $
    sourceFile filePath -- ファイルをストリームとして読み込む
      .| decodeUtf8C -- UTF-8にデコード
      .| linesUnboundedC -- 行ごとにストリーム化
      .| mapC trimCRLF -- 改行文字を削除
      .| evalStateC initialContext (mapMC processLine) -- 累積状態を持ちながら処理
      .| evalStateC initialOptimizerContext (mapMC optimizeProgramLine) -- 累積状態を持ちながら処理
      .| mapMC estimateTime
      .| evalStateC initialGeneratorContext (mapMC generateCodeLine) -- 累積状態を持ちながら処理
      .| unlinesC -- 行を結合
      .| encodeUtf8C -- UTF-8にエンコード
      .| sinkFile (filePath ++ ".optimized.cnc") -- 結果をファイルに書き込む
  let time = estimatorContextTotalTime state
  let segments = estimatorContextSegments state
  putStrLn $ "total time: " ++ show time
  putStrLn $ "remain segments: " ++ show segments
  return ()

main :: IO ()
main = do
  putStrLn "Processing file..."
  processFile "1001.cnc"
  putStrLn "Done!"
  let testTime = totalMinTime 360000 [(1000.0,13.388162868743418)]
  putStrLn $ "test total time: " ++ show testTime
  putStrLn $ "test angle 1: " ++ show (sphericalAngleAzimuth (computeLineEndAngles (Position 31.819 8.124 0) (Position 32.001 8.147 0)) * 180 / pi)
  putStrLn $ "test angle 2: " ++ show (sphericalAngleAzimuth (computeLineEndAngles (Position 32.001 8.147 0) (Position 32.023 8.161 0)) * 180 / pi)
  return ()
