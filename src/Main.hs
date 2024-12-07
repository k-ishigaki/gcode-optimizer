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
    sinkFile,
    sourceFile,
    unlinesC,
    (.|),
  )
import Control.Monad.State.Strict (MonadState (get, put), StateT, runState)
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Text (Text, dropWhileEnd, pack)
import GCodeParser (MoveContext, initialContext, parseProgram)
import Text.Megaparsec
  ( errorBundlePretty,
    runParserT,
  )
import AST (Program (Program), Block (Other))
import GCodeGenerator (Context, generateGCode, initialGeneratorContext)

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

generateCodeLine :: (Monad m) => Program -> StateT Context m Text
generateCodeLine prog = do
  currentState <- get
  let (code, finalState) = runState (generateGCode prog) currentState
  put finalState
  return code

-- 改行文字をトリムする関数
trimCRLF :: Text -> Text
trimCRLF = dropWhileEnd (`elem` ['\r', '\n'])

-- ファイル全体を処理
processFile :: FilePath -> IO ()
processFile filePath = do
  runConduitRes $
    sourceFile filePath -- ファイルをストリームとして読み込む
      .| decodeUtf8C -- UTF-8にデコード
      .| linesUnboundedC -- 行ごとにストリーム化
      .| mapC trimCRLF -- 改行文字を削除
      .| evalStateC initialContext (mapMC processLine) -- 累積状態を持ちながら処理
      .| evalStateC initialGeneratorContext (mapMC generateCodeLine) -- 累積状態を持ちながら処理
      .| unlinesC -- 行を結合
      .| encodeUtf8C -- UTF-8にエンコード
      .| sinkFile (filePath ++ ".out") -- 結果をファイルに書き込む

main :: IO ()
main = do
  putStrLn "Processing file..."
  processFile "1001.cnc"
  putStrLn "Done!"
