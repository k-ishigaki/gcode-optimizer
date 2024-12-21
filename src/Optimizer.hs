module Optimizer where
import AST (Program (Program), Position (Position), Motion (LinearMove, RapidMove, ArcMove), Block (Move), unset, Speed (Speed))
import Control.Monad.State.Strict (State, gets, modify)

newtype OptimizerContext
  = OptimizerContext {optimizerContextPosition :: Position}
  deriving (Show, Eq)

initialOptimizerContext :: OptimizerContext
initialOptimizerContext = OptimizerContext (Position unset unset unset)

optimizeProgram :: Program -> State OptimizerContext Program
optimizeProgram (Program blocks) = do
  optimizedBlocks <- mapM optimizeBlock blocks
  return $ Program (concat optimizedBlocks)

optimizeBlock :: Block -> State OptimizerContext [Block]
optimizeBlock (Move motion) = do
  optimizedMotions <- optimizeMotion motion
  return $ map Move optimizedMotions
optimizeBlock block = return [block]

optimizeMotion :: Motion -> State OptimizerContext [Motion]
optimizeMotion motion = do
  case motion of
    LinearMove (Position x y z) _ -> do
      Position prevX prevY prevZ <- gets optimizerContextPosition
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      -- Z > 0の平面移動、またはZ > 0への上昇移動をRapidMoveに変更
      if (z == prevZ && z > 0) || (x == prevX && y == prevY && z > prevZ)
        then do
          return [RapidMove (Position x y z)]
        else do
          -- 最大粗切り込みピッチx1.5までのZ軸下降移動をRapidMoveに変更
          if x == prevX && y == prevY && (prevZ - z) > 0.02 * 1.5
            then do
              return [RapidMove (Position x y (z + 0.02 * 1.5)), motion]
            else do
              return [motion]
    RapidMove (Position x y z) -> do
      Position prevX prevY prevZ <- gets optimizerContextPosition
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      -- Z > 0の平面移動、またはZ > 0への上昇移動をRapidMoveに変更
      if (z == prevZ && z > 0) || (x == prevX && y == prevY && z > prevZ)
        then do
          return [LinearMove (Position x y z) (Speed 15)]
        else do
          return [motion]
    ArcMove (Position x y z) _ _ _ -> do
      modify $ \s -> s {optimizerContextPosition = Position x y z}
      return [motion]
