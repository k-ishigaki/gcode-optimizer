module Optimizer where
import AST (Program (Program), Position (Position), Motion (LinearMove, RapidMove, ArcMove), Block (Move), unset)
import Control.Monad.State.Strict (State, gets, modify)

newtype OptimizerContext
  = OptimizerContext {optimizerContextPosition :: Position}
  deriving (Show, Eq)

initialOptimizerContext :: OptimizerContext
initialOptimizerContext = OptimizerContext (Position unset unset unset)

optimizeProgram :: Program -> State OptimizerContext Program
optimizeProgram (Program blocks) = do
  optimizedBlocks <- mapM optimizeBlock blocks
  return $ Program optimizedBlocks

optimizeBlock :: Block -> State OptimizerContext Block
optimizeBlock (Move motion) = do
  optimizedMotion <- optimizeMotion motion
  return $ Move optimizedMotion
optimizeBlock block = return block

optimizeMotion :: Motion -> State OptimizerContext Motion
optimizeMotion motion = do
  case motion of
    LinearMove (Position x y z) _ -> do
      Position _ _ prevZ <- gets optimizerContextPosition
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      if z == prevZ && z > 0
        then do
          return $ RapidMove (Position x y z)
        else do
          return motion
    RapidMove (Position x y z) -> do
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      return motion
    ArcMove (Position x y z) _ _ _ -> do
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      return motion
