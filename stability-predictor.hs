{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry
import qualified Data.ByteString.Lazy as BS
import Text.Parsec
import Control.Monad
import Data.List
import Data.Maybe
import Control.Arrow (first, second)
import Graphics.Rendering.Chart
import Data.Functor
import Data.Accessor
import Data.Int
import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import Graphics.Rendering.Chart.Axis.Indexed
import Data.Colour
import Data.Colour.Names
import System.Directory

type Package = String
type Version = String
type Date = EpochTime
type History = [(Date, Version)]
type TimeDiff = EpochTime

readCabalHistories :: FilePath -> IO (M.Map Package History)
readCabalHistories tarfilename = do
    raw <- BS.readFile tarfilename
    return $
        M.map (M.toDescList) $
        M.fromListWith M.union $
        map (\(p,v,d) -> (p,M.singleton d v)) $
        map parseEntry $
        filter (not . ignoredEntry) $
        Tar.foldEntries (:) [] (error "invalid tar") (Tar.read raw)

ignoredEntry :: Entry -> Bool
ignoredEntry (Entry {..}) = fromTarPath entryTarPath `elem` ["preferred-versions"]


parseEntry :: Entry -> (Package, Version, Date)
parseEntry (Entry {..}) = (p, v, entryTime)
  where
    (p,v) = either (error.show) id $ parse pathParser "tar path" (fromTarPath entryTarPath)
    pathParser = do
        p <- manyTill anyChar (char '/')
        v <- manyTill anyChar (char '/')
        p' <- manyTill anyChar (char '.')
        guard (p == p')
        string "cabal"
        eof
        return (p,v)

-- time diff relative to the release in question
type ReleaseHistory = (Version,  TimeDiff, [(TimeDiff, Version)])

releaseHistory :: History -> Maybe ReleaseHistory
releaseHistory ((d1,_) : (d2,v) : past)
    = Just (v, d1 - d2, map (first (\d -> d2 - d)) past)
releaseHistory _ = Nothing

allHistories :: M.Map Package History -> [ReleaseHistory]
allHistories = mapMaybe releaseHistory . concatMap tails . M.elems

pointsToFrequencies :: Int -> Int -> [(Int64, Int64)] -> Image Pixel8
pointsToFrequencies w h pts = runST $ do
    pix <- createMutableImage w h (0::Pixel32)
    forM_ pts $ \(x,y) -> addPix pix (fromIntegral $ (x * (fromIntegral w-1)) `div` maxX)
                                     (fromIntegral $ (fromIntegral h-1) - (y * (fromIntegral h-1)) `div` maxY) 1
    pix' <- freezeImage pix
    let maxP = pixelFold (\m _ _ p -> max m p) 0 pix'

    -- return $ pixelMap (\p -> if p > 0 then 2^8-1 else 0) pix'
    return $ pixelMap (\p -> fromIntegral $ min (p*2^5) (2^8-1)) pix'
    -- return $ pixelMap (\p -> fromIntegral $ (p * (2^8-1)) `div` maxP) pix'

  where maxX = fromIntegral $ maximum $ map fst pts
        maxY = fromIntegral $ maximum $ map snd pts


addPix i x y d = do
    p <- readPixel i x y
    writePixel i x y (p + d)

a_day = 60 * 60 * 24
a_month = 60 * 60 * 24 * 30
a_year = 60 * 60 * 24 * 365

toGroup :: Int64 -> Int
toGroup n | n <     a_day = 1
          | n <   7*a_day = 2
          | n <  30*a_day = 3
          | n <  90*a_day = 4
          | n < 265*a_day = 5
          | otherwise     = 6

avgAndCount :: [Int64] -> (Int, Int)
avgAndCount xs = (fromIntegral $ sum xs `div` fromIntegral l, l)
  where l = length xs

main = do
    dotCabal <- getAppUserDataDirectory "cabal"
    releaseHistories <- allHistories <$> readCabalHistories (dotCabal ++ "/packages/hackage.haskell.org/00-index.tar")

    let points =  [ (x,y)
                  | (_,x,past) <- releaseHistories
                  , x < a_year
                  , (y,_) <- past
                  , y < a_year
                  ]

    BS.writeFile "hackage-release-history-histogram.png" $ encodePng $ pointsToFrequencies 600 600 points


    let histogram = map (\((a,b),c) ->
                let (av,t) = avgAndCount (c [])
                in  (a,b,av,t)   ) $
            M.toList $
            M.fromListWith (.) $
            [ ((toGroup y1, toGroup (y2 - y1)), (x:))
            | (_,x,((y1,_):(y2,_):_)) <- releaseHistories
            ]


    let plot = toPlot $
            area_spots_4d_values ^= histogram $
            area_spots_4d_palette ^= [blend x black white| x <- [0.3,0.35..1]] $
            defaultAreaSpots4D {area_spots_4d_opacity_ = 1}
    let axis = const $ autoIndexAxis ["huh", "< 1 day","< 1 week","< 1 month","< 1 quartal","< 1 year","> 1 year"] [1..6]
    let graph =
            layout1_plots ^= [Right plot] $
            layout1_bottom_axis ^= (
                laxis_title ^= "Last release" $
                laxis_override ^= axis $
                defaultLayoutAxis
            ) $
            layout1_right_axis ^= (
                laxis_title ^= "Second-to-last release" $
                laxis_override ^= axis $
                defaultLayoutAxis
            ) $
            defaultLayout1
    renderableToPNGFile (toRenderable graph) 600 600 "hackage-stability-prediction.png"
