import Data.List (transpose, group, unfoldr, sort, mapAccumL)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Debug.Trace
import Data.Word
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (isJust, fromJust, listToMaybe)
import Text.Printf (printf)
import qualified Data.Map as M
import qualified Text.Show.ByteString as S
import GHC.Float (FFFormat (FFFixed), formatRealFloat)

nearestMillionth :: Double -> Double
nearestMillionth = (/ 1000000.0) . fromIntegral . round . (* 1000000.0)

type HInches = Int
type VInches = Int
type RGB = (Word8,Word8,Word8)

type HPercentage = Double
type VPercentage = Double
data Image = ColorBox   {hPercent :: !HPercentage, vPercent :: !VPercentage, rgb :: !RGB}
     	   | TextBox    {hPercent :: !HPercentage, vPercent :: !VPercentage, text :: String}
     	   | VConcatBox {hPercent :: !HPercentage, vPercent :: !VPercentage, images :: [Image]}
	   | HConcatBox {hPercent :: !HPercentage, vPercent :: !VPercentage, images :: [Image]}
             deriving Show
data ImageFrame = ImageFrame HInches VInches Image deriving Show

colors = concat $ repeat [(64,0,128), (229,40,0), (192,142,0), (25,64,0), (0,200,255)]
clear = (255,255,255)

sliceList d t = take t . drop d

display :: HPercentage -> VPercentage -> [String] -> Image
display stepCount bar words = VConcatBox 1.0 1.0 lines
  where lineLength = 12
	lineCount = fromIntegral $ ceiling (fromIntegral (length words) / fromIntegral lineLength)
  	lines = [VConcatBox 1.0 (1/lineCount) [displayLine dbdata stepCount bar words idx lineLength] | idx <- [0,lineLength..length words - 2]]
	dbdata = M.fromList [lteqgtKV i len | len <- [4,5], i <- [0..(length words - len)]]
	lteqgtKV d t = ((subslice words), lteqgt conn (subslice wordids)) where subslice = sliceList d t
	wordids = map (wordidquery conn) words

displayLine dbdata stepCount bar words offset lineLength = HConcatBox 1.0 1.0 (take (lineLength * 2) (visTweens ++ slugs))
  where visTweens = zipWith (\ v t -> HConcatBox componentSize 1.0 [HConcatBox visSize 1.0 [v], HConcatBox tweenSize 1.0 [t]]) viss tweens
  	tweens = [VConcatBox 1.0 1.0 [VConcatBox 1.0 lineSize [tween dbdata stepCount bar words idx], ColorBox 1.0 gapSize clear] | idx <- [offset .. maxOffset]]
        viss = [VConcatBox 1.0 1.0 [VConcatBox 1.0 lineSize [vis dbdata bar words idx], TextBox 1.0 gapSize (words !! idx)] | idx <- [offset .. maxOffset]]
  	lineSize = 0.6667
	gapSize = 1.0 - lineSize
	visSize = 0.001
	tweenSize = 1.0 - visSize
	componentSize = 1.0 / (fromIntegral lineLength)
	maxOffset = ((offset+lineLength) `min` (length words)) - 1
	slugs = (repeat $ HConcatBox (componentSize*0.5) 1.0 [ColorBox 1.0 1.0 clear])

ease :: Double -> Double
ease x = if x < 0.5 then 8 * x * x * x * x else 1 - 8 * (x - 1) * (x - 1) * (x - 1) * (x - 1)

tween :: M.Map [String] (Maybe (Double, Double)) -> HPercentage -> VPercentage -> [String] -> Int -> Image
tween dbdata stepCount bar words i | i == length words - 1 = ColorBox 1.0 1.0 clear
                                   | otherwise             = HConcatBox 1.0 1.0 [HConcatBox (1.0 / stepCount) 1.0 [nest dbdata step bar words i 5] | step <- map ease (reverse [0.0, 1.0 / stepCount .. 1])]

vis :: M.Map [String] (Maybe (Double, Double)) -> VPercentage -> [String] -> Int -> Image
vis dbdata bar words i = nest dbdata 1.0 bar words i 4

nest :: M.Map [String] (Maybe (Double, Double)) -> VPercentage -> VPercentage -> [String] -> Int -> Int -> Image
nest dbdata tweenScaling bar words i _ | i >= (length words) || i < 0 = error "nesting went too far"
nest dbdata tweenScaling bar words i w | i+w >= (length words) = nest dbdata tweenScaling bar words i (w-1)
nest dbdata tweenScaling bar words i w = VConcatBox 1.0 1.0 [ColorBox 1.0 tweenedBar ci, innards, ColorBox 1.0 tweenedBar ci]
  where ci = colors !! i
	innardHeight = 1 - 2 * tweenedBar
        innards = if w == 0 then ColorBox 1.0 innardHeight clear else VConcatBox 1.0 innardHeight [ColorBox 1.0 tweenedLt clear, VConcatBox 1.0 (1 - tweenedLt - tweenedGt) [nest dbdata 1.0 bar words (i+1) (w-1)], ColorBox 1.0 tweenedGt clear]
        (lt,gt) = findBestFitInWindow dbdata words (i-3) (i+1)
        tweenedBar = bar * tweenScaling
        (tweenedLt, tweenedGt) = (lt * tweenScaling, gt * tweenScaling)


findBestFitInWindow :: M.Map [String] (Maybe (Double, Double)) -> [String] -> Int -> Int -> (VPercentage, VPercentage)
findBestFitInWindow dbdata words b e | b < 0  = findBestFitInWindow dbdata words 0 e
                                     | b >= e = (0.5, 0.5)
                                     | b < e && not (existsGramCached dbdata slice) = findBestFitInWindow dbdata words (b+1) e
                                     | otherwise = fromJust (lteqgtCached dbdata slice)
  where slice = map (words !!) [b .. e]

existsGramCached dbdata = isJust . lteqgtCached dbdata

lteqgtCached dbdata k = (M.lookup k dbdata) >>= id

conn = unsafePerformIO $! connectSqlite3 "grams.db"

lteqgt :: Connection -> [Int] -> Maybe (VPercentage, VPercentage)
lteqgt c wordids = do [lt, eq, gt] <- lteqgtquery c wordids
                      let tot = lt+eq+gt
                      return (lt / tot, gt / tot)

wordidquery :: Connection -> String -> Int
wordidquery c w = maybe 0 (head . map fromSql) id
  where id = listToMaybe $ unsafePerformIO $ quickQuery' c wordIdSql [toSql w]

lteqgtquery :: Connection -> [Int] -> Maybe [VPercentage]
lteqgtquery c words = fmap (map fromSql) $ listToMaybe $ unsafePerformIO $ quickQuery' c (legtSql (length words)) (map toSql words)

wordIdSql = "select id from words where word = ?"
legtSql 5 = "select lt, eq, gt from d5 where a = ? and b = ? and c = ? and d = ? and e = ?"
legtSql 4 = "select lt, eq, gt from d4 where a = ? and b = ? and c = ? and d = ?"
legtSql _ = "select * from sqlite_master limit 0"

main = B.writeFile "shannon.svg" (imageFrameToSVG (ImageFrame 17 23 (display 200 0.1 $! words "The fundamental problem of communication is that of reproducing at one point either exactly or approximately a message selected at another point . Frequently the messages have meaning ; that is they refer to or are correlated according to some system with certain physical or conceptual entities . These semantic aspects of communication are irrelevant to the engineering problem . The significant aspect is that the actual message is one selected from a set of possible messages .")))

imageFrameToSVG :: ImageFrame -> B.ByteString
imageFrameToSVG (ImageFrame x y i) = makeTopSVG x y (imageToFlatSVG (0,0,1,1) (0,0) i)

imageToFlatSVG :: (Double, Double, Double, Double) -> (Double, Double) -> Image -> B.ByteString
imageToFlatSVG (!x,!y,!w,!h) (!xStart,!yStart) (ColorBox xPct yPct rgb) = if rgb==clear || bh < 0.00000001 then "" else makeRect rgb bx by (bw+0.001) bh where (!bx, !by, !bw, !bh) = newBox (x, y, w, h) (xStart, yStart, xPct, yPct)
imageToFlatSVG (!x,!y,!w,!h) (!xStart,!yStart) (TextBox xPct yPct text) = makeText bx by text where (!bx, !by, _, _) = newBox (x,y,w,h) (xStart, yStart, xPct, yPct)
imageToFlatSVG (!x,!y,!w,!h) (!xStart,!yStart) (VConcatBox xPct yPct is) = linearConcat (x,y,w,h) (xStart,yStart,xPct,yPct) is (\ !xA !yA i -> (xA, yA + vPercent i))
imageToFlatSVG (!x,!y,!w,!h) (!xStart,!yStart) (HConcatBox xPct yPct is) = linearConcat (x,y,w,h) (xStart,yStart,xPct,yPct) is (\ !xA !yA i -> (xA + hPercent i, yA))

linearConcat :: (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> [Image] -> (Double -> Double -> Image -> (Double, Double)) -> B.ByteString
linearConcat (!x,!y,!w,!h) (!xStart,!yStart,!xPct,!yPct) is acc = B.concat $ snd $ mapAccumL (\ (!xA,!yA) i -> (acc xA yA i, imageToFlatSVG (bx,by,bw,bh) (xA,yA) i)) (0,0) is where (!bx,!by,!bw,!bh) = newBox (x,y,w,h) (xStart,yStart,xPct,yPct)

-- mapAccum :: (acc -> x -> (acc,y)) -> acc -> x -> (acc,[y])
-- mapAccum f z lst = 

newBox (!x,!y,!w,!h) (!xStart,!yStart,!xPct,!yPct) = (x + w*xStart, y + h*yStart, w*xPct, h*yPct)


floatToPctNumber !f = formatRealFloat FFFixed Nothing (nearestMillionth (f * 100))

makeRect (!r,!g,!b) !x !y !w !h = B.pack $! "<rect x='" ++ floatToPctNumber x ++ "%' y='" ++ floatToPctNumber y ++ "%' width='" ++ floatToPctNumber w ++ "%' height='" ++ floatToPctNumber h ++ "%' fill='rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")'/>\n"
makeText !x !y s = B.pack $! "<text font-family='Verdana' font-size='0.15in' dy='0.15in' y='" ++ floatToPctNumber y ++ "%' x='" ++ floatToPctNumber x ++ "%'>" ++ s ++ "</text>\n"
makeTopSVG !x !y s = B.pack ("<svg width='" ++ show x ++ "in' height='" ++ show y ++ "in' xmlns='http://www.w3.org/2000/svg'><text font-family='Verdana' font-size='0.075in' x='0.0%' y='99.5%' fill='#666666'>Based on the Google Books 4-gram and 5-gram dataset. http://books.google.com/ngrams/datasets  Each word is nested in the previous word at a size proportional to its probability in the largest possible preceding context. The spacing, above and below, corresponds to the probabilities of all words more and less globally popular than that word in the same context.</text>") `B.append` s `B.append` "</svg>"
