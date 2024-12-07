{-# LANGUAGE DeriveGeneric #-}

module AST where

import GHC.Generics (Generic)
import Data.Text (Text)

-- G-Code プログラム全体
newtype Program = Program [Block]
  deriving (Show, Eq, Generic)

-- プログラムの基本単位
data Block
  = Move Motion                -- 移動 (線形移動, 円弧移動など)
  | WaitCommand Delay          -- 一時停止
  | NoArgumentCommand Text     -- 引数なしコマンド
  | ChangeTool Int             -- ツールチェンジ
  | ChangeSpindleSpeed Int     -- スピンドル速度変更
  | MachineCode Int            -- 機械コマンド（Mコマンド）
  | ToolLengthCompensation ToolLengthCompensationDirection Double Int -- 工具長補正
  | Comment Text               -- コメント
  | OtherCommand Text          -- その他（コマンド）
  | Other Text                 -- その他（コマンド以外）
  deriving (Show, Eq, Generic)

-- 工具長補正方向
data ToolLengthCompensationDirection
  = Forward
  | Backward
  deriving (Show, Eq, Generic)

-- 移動モーションの抽象化
data Motion
  = RapidMove Position -- 高速移動
  | LinearMove Position Speed -- 移動
  | ArcMove Position CenterOffset Direction Speed -- 円弧移動
  deriving (Show, Eq, Generic)

-- ディレイ量
data Delay
  = DelayMilliseconds Int
  | DelaySeconds Double
  deriving (Show, Eq, Generic)

-- 位置の定義
data Position = Position
  { positionX :: Double
  , positionY :: Double
  , positionZ :: Double
  } deriving (Show, Eq, Generic)

-- 中心座標オフセット（XY平面のみ対応）
data CenterOffset = CenterOffset
  { centerOffsetI :: Double,
    centerOffsetJ :: Double
  } deriving (Show, Eq, Ord, Generic)

-- 速度 (送り速度など)
newtype Speed = Speed Double
  deriving (Show, Eq, Ord, Generic)

-- 円弧の方向
data Direction = CW | CCW  -- 時計回り (CW) または反時計回り (CCW)
  deriving (Show, Eq, Generic)

-- 未設定であることを示すDouble値
unset :: Double
unset = 1 / 0