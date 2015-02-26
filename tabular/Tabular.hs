--Implementation of R like table object with IO to .tsv in Haskell

import Data.CSV.Conduit
import Data.ByteString.Char8 (ByteString, unpack, pack)
import qualified Data.Vector as V
import Control.Monad
import Control.Exception as E
import Data.List
type Text = ByteString -- Change this to change the underlying datatype
type Tabular = V.Vector(V.Vector Text)
type TabularRow = V.Vector Text

-- File Accessor
fromFileWith' sep quote fp= do
    let myOption = CSVSettings{csvSep=sep, csvQuoteChar=quote}
    csvData <- readCSVFile myOption fp :: IO ( V.Vector([Text]) )
    return (Just ( V.map V.fromList csvData))

fromFileWith sep quote fp = E.catch
    (fromFileWith' sep quote fp)
    (\e -> do 
     let err = show (e::IOException)
     putStrLn $ "Unexpected Error at opening file: "++ (show err)
     return Nothing
    )

fromFile = fromFileWith '\t' Nothing

-- Row accessor, get Nothing in case of error

header' t = t V.! 0
header t = if V.length t > 0 
    then Just (t V.! 0)
    else Nothing

(#!) :: Tabular -> Int -> Maybe TabularRow
t #! i = if i<0 || i> V.length t 
    then Nothing
    else Just (t V.! (i+1))
     --compensate for the header which we assume always is there
(#) :: (Maybe Tabular) -> Int -> Maybe TabularRow
t # i = t >>= (#! i)

-- Column accessor, would like it to get nothing in case of error

(%!) :: Tabular -> Int ->Maybe TabularRow
t %! i = if i < 0 || i > (V.length $ t V.! 0 ) 
    then Nothing
    else let vv = V.map (\x -> (x::TabularRow) V.! i) t in
        if V.length vv > 1 
            then Just (V.tail vv)
            else Nothing

(%) :: (Maybe Tabular) -> Int ->Maybe TabularRow
t % i = t >>= (%! i)


(%%!):: Tabular -> String -> Maybe TabularRow
t %%! s = let hd = header t in case hd of
         Nothing -> Nothing 
         Just h -> let key = (pack s) `V.elemIndex` h in case key of
            Nothing -> Nothing
            Just idx -> t %! idx

(%%):: Maybe(Tabular) -> String -> Maybe TabularRow
t %% s = t >>= (%%! s)

--type coercer, would like it to crash in case of error

toFloat :: TabularRow ->(V.Vector Float)
toFloat xs = V.map (\x -> (read $ unpack x) :: Float) xs
toFloatM :: Maybe TabularRow -> Maybe (V.Vector Float)
toFloatM = fmap toFloat

toString :: TabularRow ->(V.Vector String)
toString xs = V.map (\x -> unpack x) xs
toStringM :: Maybe TabularRow -> Maybe (V.Vector String)
toStringM = fmap toString

toBool :: TabularRow ->(V.Vector Bool)
toBool xs = V.map (\x -> (read $ unpack x) :: Bool) xs
toBoolM :: Maybe TabularRow -> Maybe (V.Vector Bool)
toBoolM = fmap toBool

--modifiers 

setRow' :: Int -> TabularRow -> Tabular ->  Maybe Tabular
setRow' i r t = if i >= V.length t -1 || i < 0 || (V.length r) /= (V.length $ header' t)
            then Nothing
            else Just (t V.// [(i + 1, r)])
setRow :: Int -> Maybe TabularRow -> Tabular ->  Maybe Tabular
setRow _ Nothing _ = Nothing
setRow i (Just r) t = if i >= V.length t -1 || i < 0 || (V.length r) /= (V.length $ header' t)
            then Nothing
            else Just (t V.// [(i + 1, r)])

setCol' :: Int -> TabularRow -> Tabular -> Maybe Tabular
setCol' i c t = if i >= V.length (header' t) || i < 0 || (V.length c) /= V.length t -1
            then Nothing
            else Just ( V.zipWith (\x y -> x V.// [(i,y)]) t ( n `V.cons` c) ) where
                n = header' t V.! i
setCol :: Int ->Maybe TabularRow -> Tabular -> Maybe Tabular
setCol _ Nothing _= Nothing
setCol i (Just c) t = if i >= V.length (header' t) || i < 0 || (V.length c) /= V.length t -1
            then Nothing
            else Just ( V.zipWith (\x y -> x V.// [(i,y)]) t ( n  `V.cons` c) ) where
                n = header' t V.! i

setColN' :: String -> TabularRow -> Tabular -> Maybe Tabular
setColN' n c t = let nn = pack n in case nn `V.elemIndex` (header' t) of 
            Nothing -> Nothing
            Just i -> Just ( V.zipWith (\x y -> x V.// [(i,y)]) t ( nn `V.cons` c) )

setColN :: String ->Maybe TabularRow -> Tabular -> Maybe Tabular
setColN _ Nothing _= Nothing
setColN n (Just c) t = let nn = pack n in case nn `V.elemIndex` (header' t) of 
            Nothing -> Nothing
            Just i -> Just ( V.zipWith (\x y -> x V.// [(i,y)]) t ( nn `V.cons` c) )

--use >>= to modify

--populators
--addRow < 
--addCol (Name separate) <<

main = do
    t <- fromFile "./txts/test_csv.txt"
    let r = t # 0
    let c = t % 0
    let cc = t %% "KEGG"
    putStrLn $ show r 
    putStrLn $ show c
    putStrLn $ show cc
    let tt = t >>= (setRow 3 r)
    putStrLn $ show $ tt#3 
    let ttt = t >>= (setColN "KEGG" c)
    putStrLn $ show $ ttt%%"KEGG"
    return ()
