module TimeEstimator where
import AST (Motion (LinearMove, RapidMove, ArcMove), Position (Position), CenterOffset (CenterOffset), Direction (CW), Speed (Speed), Block (Move), Program (Program))
import Control.Monad.State.Strict (State, gets, modify)
import Data.Fixed (mod')
import Data.Foldable (Foldable(foldl'))


-- 前のパスの終端Vector -> 前のパスの(終端速度 -> 合計時間) -> 終端速度 -> 合計時間

data EstimatorContext = EstimatorContext
  { estimatorContextPosition :: Position,
    estimatorContextAngle :: SphericalAngle,
    estimatorContextTotalTime :: Double,
    estimatorContextSegments :: [(Double, Double)] -- 最大速度と経路長の配列
  }

-- 極座標角度
data SphericalAngle = SphericalAngle
  { sphericalAngleZenith :: Double, -- 天頂角 (0 ≤ θ ≤ π)
    sphericalAngleAzimuth :: Double -- 方位角 (0 ≤ φ < 2π)
  }
  deriving (Show, Eq)

fromCartesian :: (Double, Double, Double) -> SphericalAngle
fromCartesian (x, y, z) =
  SphericalAngle
    { sphericalAngleZenith = atan2 (sqrt (x ** 2 + y ** 2)) z, -- θ
      sphericalAngleAzimuth = atan2 y x -- φ
    }


initialEstimatorContext :: EstimatorContext
initialEstimatorContext = EstimatorContext (Position 0 0 0) (SphericalAngle 0 0) 0 []

estimateProgram :: Program -> State EstimatorContext ()
estimateProgram (Program blocks) = do
  mapM_ estimateBlock blocks

estimateBlock :: Block -> State EstimatorContext ()
estimateBlock (Move motion) = do
  estimateMotion motion
estimateBlock _ = return ()


estimateMotion :: Motion -> State EstimatorContext ()
estimateMotion motion = do
  prevPosition <- gets estimatorContextPosition
  prevEndAngle <- gets estimatorContextAngle

  let pos = case motion of
        RapidMove position -> position
        LinearMove position _ -> position
        ArcMove position _ _ _ -> position
  
  modify $ \s -> s { estimatorContextPosition = pos }

  -- 角度が異なる場合は接続点速度は0となるため、ここまでの時間を計算する
  let nextStartAngle = case motion of
        RapidMove position -> computeLineEndAngles prevPosition position
        LinearMove position _ -> computeLineEndAngles prevPosition position
        ArcMove position centerOffset direction _ -> fst (computeArcAngles prevPosition position centerOffset direction)
  _ <- if nextStartAngle `almostEqual` prevEndAngle
      then do
        return ()
      else do
        segments <- gets estimatorContextSegments
        let totalTime = totalMinTime 360000 segments
        let modifiedTotalTime = if isNaN totalTime || isInfinite totalTime then 0 else totalTime
        prevTotalTime <- gets estimatorContextTotalTime
        modify $ \s -> s { estimatorContextTotalTime = prevTotalTime + modifiedTotalTime }
        modify $ \s -> s { estimatorContextSegments = [] }
        return ()
  
  -- セグメント（経路長と制限速度）と終端角度を保持しておく
  let (segment, endAngle) = case motion of
        RapidMove position -> ((1000, distance3D prevPosition position), computeLineEndAngles prevPosition position)
        LinearMove position (Speed speed) -> ((speed, distance3D prevPosition position), computeLineEndAngles prevPosition position)
        ArcMove position centerOffset direction (Speed speed) -> ((computeArcMaxSpeed 360000 prevPosition position centerOffset speed, arcDistance prevPosition position centerOffset direction), snd (computeArcAngles prevPosition position centerOffset direction))
  
  modify $ \s -> s { estimatorContextSegments = estimatorContextSegments s ++ [segment]}
  modify $ \s -> s { estimatorContextAngle = endAngle }







-- 天頂角と方位角を計算
computeLineEndAngles :: Position -> Position -> SphericalAngle
computeLineEndAngles (Position x1 y1 z1) (Position x2 y2 z2) =
  let
    -- ベクトルの各成分
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1
    -- 距離
    distance = sqrt (dx ** 2 + dy ** 2 + dz ** 2)
    -- 天頂角 θ
    zenith = acos (dz / distance)
    -- 方位角 φ
    azimuth = atan2 dy dx
    normalizedAzimuth = azimuth `mod'` (2 * pi)
  in
    SphericalAngle zenith normalizedAzimuth

computeArcAngles :: Position -> Position -> CenterOffset -> Direction -> (SphericalAngle, SphericalAngle)
computeArcAngles (Position x1 y1 z1) (Position x2 y2 z2) (CenterOffset i j) direction =
  let
    -- 中心座標
    cx = x1 + i
    cy = y1 + j

    -- Z方向の変化量（始点から終点）
    dz = z2 - z1

    -- 接線ベクトル (始点)
    tx1 = if direction == CW then -(y1 - cy) else y1 - cy
    ty1 = if direction == CW then x1 - cx else -(x1 - cx)
    tz1 = dz -- Z方向の変化量を含める

    -- 接線ベクトル (終点)
    tx2 = if direction == CW then -(y2 - cy) else y2 - cy
    ty2 = if direction == CW then x2 - cx else -(x2 - cx)
    tz2 = dz -- Z方向の変化量を含める

    -- 天頂角と方位角を計算するヘルパー関数
    calculateAngles tx ty tz =
      let
        magnitude = sqrt (tx ** 2 + ty ** 2 + tz ** 2)
        zenith = acos (tz / magnitude) -- 天頂角
        azimuth = atan2 ty tx `mod'` (2 * pi) -- 方位角
      in (zenith, azimuth)

    -- 始点と終点の角度
    (zenith1, azimuth1) = calculateAngles tx1 ty1 tz1
    (zenith2, azimuth2) = calculateAngles tx2 ty2 tz2
  in (SphericalAngle zenith1 azimuth1, SphericalAngle zenith2 azimuth2)

distance3D :: Position -> Position -> Double
distance3D (Position x1 y1 z1) (Position x2 y2 z2) =
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)


-- G-Code円弧移動の距離を計算する関数
arcDistance :: Position -- 始点 (x1, y1, z1)
            -> Position -- 終点 (x2, y2, z2)
            -> CenterOffset         -- 中心オフセット (i, j)
            -> Direction                     -- True = 時計回り (G2), False = 反時計回り (G3)
            -> Double                   -- 移動距離
arcDistance (Position x1 y1 z1) (Position x2 y2 z2) (CenterOffset i j) direction =
  let
    -- 円弧の中心座標
    cx = x1 + i
    cy = y1 + j

    -- 半径の計算（始点または終点から中心までの距離）
    r1 = sqrt ((x1 - cx) ** 2 + (y1 - cy) ** 2)
    r2 = sqrt ((x2 - cx) ** 2 + (y2 - cy) ** 2)
    radius = (r1 + r2) / 2 -- 平均で安定性を確保（理論上は等しいはず）

    -- 始点と終点から中心への角度を計算
    theta1 = atan2 (y1 - cy) (x1 - cx)
    theta2 = atan2 (y2 - cy) (x2 - cx)

    -- 角度の差分 (Δθ)
    deltaTheta = if direction == CW
                 then (theta1 - theta2) `mod'` (2 * pi) -- 時計回り: θ1 > θ2
                 else (theta2 - theta1) `mod'` (2 * pi) -- 反時計回り: θ2 > θ1

    -- XY平面上の弧の長さ
    arcLengthXY = radius * deltaTheta

    -- Z方向の直線距離
    zDistance = abs (z2 - z1)

  in
    sqrt (arcLengthXY ** 2 + zDistance ** 2) -- 円弧長とZ方向距離の直線合成


computeArcMaxSpeed ::
  Double -> -- 各軸の最大加速度
  Position -> -- 始点
  Position -> -- 終点
  CenterOffset -> -- 中心オフセット
  Double -> -- 経路の設定速度
  Double -- 上限速度
computeArcMaxSpeed acc (Position _ _ z1) (Position _ _ z2) (CenterOffset i j) speed =
  let
    -- 半径を中心オフセットとZ軸距離から計算
    radius = sqrt (i**2 + j**2 + (z2 - z1)**2)

    -- 各軸の最大加速度を二乗和で統合
    maxAcceleration = sqrt (acc**2 + acc**2 + acc**2)

    -- 遠心加速度に基づく速度の上限
    maxVelocity = sqrt (maxAcceleration * radius)
  in
    min speed maxVelocity


-- 許容誤差で比較する関数
epsilon :: Double
epsilon = pi / 180 * 90 -- 90°未満の角度変更は0までは減速せずに移動すると仮定する

almostEqual :: SphericalAngle -> SphericalAngle -> Bool
almostEqual (SphericalAngle zenith1 azimuth1) (SphericalAngle zenith2 azimuth2)
  = abs (zenith1 - zenith2) < epsilon && abs (azimuth1 - azimuth2) < epsilon

--- 最短時間計算ロジック


-- 区間の開始速度と終了速度を考慮して最短時間を計算
minTimeForSegment :: Double -> Double -> Double -> Double -> Double -> Double
minTimeForSegment alpha vEntry vExit vMax len =
  let
    -- 開始速度から最大速度まで加速する時間
    tAccel = (vMax - vEntry) / alpha
    -- 加速中に移動する距離
    dAccel = vEntry * tAccel + 0.5 * alpha * tAccel**2

    -- 最大速度から終了速度まで減速する時間
    tDecel = (vMax - vExit) / alpha
    -- 減速中に移動する距離
    dDecel = vMax * tDecel - 0.5 * alpha * tDecel**2

    -- 区間の長さが最大速度に到達するのに十分か確認
    in if dAccel + dDecel >= len
       then let
         -- 最大速度に到達しない場合の到達可能な速度 vPeak を計算
         vPeak = sqrt ((2 * alpha * len + vEntry**2 + vExit**2) / 2)
         -- vPeak まで加速する時間
         tToPeak = (vPeak - vEntry) / alpha
         -- vPeak から終了速度まで減速する時間
         tFromPeak = (vPeak - vExit) / alpha
         in tToPeak + tFromPeak
       else
         let
           -- 一定速度で移動する距離
           dConst = len - (dAccel + dDecel)
           -- 一定速度で移動する時間
           tConst = dConst / vMax
         in tAccel + tConst + tDecel  -- この区間の総移動時間

-- 相互依存する速度を持つ区間のリストの総移動時間を計算
totalMinTime :: Double -> [(Double, Double)] -> Double
totalMinTime alpha segments =
  let
    -- 各区間で実現可能な速度を計算する補助関数
    calculateSpeeds :: [(Double, Double)] -> [Double]
    calculateSpeeds segs = reverse $ foldl' backwardPass [] (zip segs (scanl forwardPass 0 segs))
      where
        -- 前進パス: 加速度と区間の長さに基づいて実現可能な最大速度を計算
        forwardPass vPrev (vMax, len) =
          let vAllowed = sqrt (vPrev**2 + 2 * alpha * len)  -- 加速度で到達可能な速度
          in min vMax vAllowed  -- 区間の速度制限を適用

        -- 後退パス: 次の区間の速度にスムーズに減速できるよう速度を調整
        backwardPass vExitList ((_, len), vForward) =
          let vExitPrev = if null vExitList then 0 else head vExitList  -- 次の区間の速度
              vAllowed = sqrt (vExitPrev**2 + 2 * alpha * len)  -- 減速を考慮した実現可能な速度
          in min vForward vAllowed : vExitList  -- 前進パスと減速条件で制約

    -- 計算した速度を使って総移動時間を計算する補助関数
    calculateTime :: [Double] -> [(Double, Double)] -> Double
    calculateTime _ [] = 0  -- 基本ケース: 残り区間なし
    calculateTime (vEntry:vEntries) ((vMax, len):rest) =
      let vExit = case vEntries of
                    [] -> 0
                    a -> head a  -- 現在の区間の終了速度
          tSegment = minTimeForSegment alpha vEntry vExit vMax len  -- この区間の移動時間
      in tSegment + calculateTime vEntries rest  -- 残りの区間を再帰的に処理
    calculateTime _ _ = 0

  -- 実現可能な速度を計算してから総移動時間を計算
  in calculateTime (calculateSpeeds segments) segments

