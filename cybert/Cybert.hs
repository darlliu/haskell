--a simple loader for cybert data
--generates cybert reports and tables
--
--has the following:
--1, a polymorphic and flexible data structure indexed by multiple
--keys.
--2, ability to do set operation, selection filtering and mapping 
--on entries
--3, IO for both human readable format and message passing to other python
--code
--
{-module definitions -}
module Cybert
(
    Cybert_entry(NA, Cybert,probe,genesym,
        sample, collection, mean, bf, bh, sds, 
        pval,ratio,secondaryRefs,secondaryData),
    cybert_entry,
    showCybertEntries,
    entriesByFold,
    entriesByPval,
    entriesBySym,
    entriesByUpDown,
    entriesBySecondaryRef,
    cybertToSet,
    loadCybert,
    exportCybert,
    exportGeneSyms,
    exportProbes
) where
{-end module definitions -}
import Data.Bits
import Data.List
import Data.Maybe
import Data.Char
import System.IO
import System.IO.Error
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

{-basic data types-}
data Cybert_entry = NA | Cybert {
    probe :: String ,
    genesym :: Maybe String ,
    sample :: [String] ,
    collection :: Maybe String , --which dataset
    mean :: Either Float [Float] , --multiple means
    sds :: Either Float [Float],
    pval :: Float, --the pairwise or ANOVA pval
    bf :: Float,
    bh :: Float,
    ratio :: Either Float [Float], --pairwise ratio or numerous ratios
    secondaryRefs :: M.Map String String , --optional secondary refs
    secondaryData :: M.Map String Float , --optional secondary data
    raw :: B.ByteString
}
cybert_entry = Cybert{
    -- default constructor
    probe = "NONE",
    genesym = Nothing,
    sample = [],
    collection = Nothing,
    mean = Right [],
    sds = Right [],
    pval = -1,
    bf = -1,
    bh = -1,
    ratio = Left (-1),
    secondaryRefs = M.fromList [("","")],
    secondaryData = M.fromList [("",0)],
    raw = B.empty
}

hasher :: String -> Int
hasher = foldl (\h c -> 33*h `xor` fromEnum c) 5381
cybertHash :: Cybert_entry -> Int
cybertHash NA = 0
cybertHash Cybert{probe=p, collection=(Nothing)} = hasher $ "NONE"++p
cybertHash Cybert{probe=p, collection=(Just s)} = hasher $ s++p

instance Eq Cybert_entry where
    a == b = (cybertHash a) == (cybertHash b)
instance Ord Cybert_entry where
    compare a b = compare  (cybertHash a)  (cybertHash b)
--hashing
{-end basic data types-}
{-format and show routines-}
showJustS::Maybe String -> String
showJustS Nothing = "N/A"
showJustS (Just x) = x

showEitherF::(Either Float [Float]) -> String
showEitherF (Left x) = show x
showEitherF (Right xxs@(x:[])) = show x
showEitherF (Right xxs@(x:xs)) = (show x) ++"\t"++ (showEitherF (Right xs))

showCybertEntries::[Cybert_entry]->String
showCybertEntries (x:[])= show x
showCybertEntries (x:xs)= (show x) ++ "\n" ++ (showCybertEntries xs)

instance Show Cybert_entry where
    showsPrec _ a s = show a ++ s
    show NA = "NA"
    show Cybert {probe=p, genesym=sym, sample=ss, collection=col, mean=m,
    pval=pv, ratio=r, secondaryRefs=_, secondaryData=_,raw=_} = p++"\t"++
        (showJustS sym)++"\t"++(showJustS col) ++"\t" ++(showEitherF m)++"\t"++(show pv)
        -- for each sample
{-end format and show routines-}
{-set operation and filtering routines-}
entriesBySym :: [Cybert_entry] -> String -> [Cybert_entry]
entriesBySym xs sym = filter (\x -> genesym x == (Just sym)) xs

entryByProbe :: [Cybert_entry] -> String -> Cybert_entry
entryByProbe xs p = head $ filter (\x -> probe x == p) xs

entriesBySecondaryRef :: [Cybert_entry] -> String -> String -> [Cybert_entry]
entriesBySecondaryRef xs tref ref = filter (\x -> (secondaryRefs x) M.! tref == ref) xs
--lookup
entriesByFold :: [Cybert_entry] -> Float -> [Cybert_entry]
entriesByFold xs threshold = filter (\x ->pred $ mean x) xs where
                            pred (Left a) = False
                            pred (Right b)= if length b < 2 then False
                                            else b!!1-b!!0 > threshold

entriesByPval :: [Cybert_entry] -> Float -> [Cybert_entry]
entriesByPval xs threshold = filter (\x -> pval x < threshold) xs

entriesByUpDown :: [Cybert_entry] -> Bool -> [Cybert_entry]
entriesByUpDown xs val = filter (\x -> pred $ mean x) xs where
                          pred (Left a) = False
                          pred (Right b) = if length b < 2 then False
                                           else let bigger = (b!!1-b!!0>0)
                                                in bigger==val
--filtering

cybertToSet :: [Cybert_entry] -> S.Set Cybert_entry
cybertToSet xs = S.fromList xs
--set operations
{-end set operation and filtering routines-}
{-IO routines-}
buildHeaderPrec :: B.ByteString -> [(String , B.ByteString)]
buildHeaderPrec s = let ss = B.split '\t' s
                in map (\x -> (stripQuote $ B.unpack  x, x)) ss where
                   stripQuote ('\"':xs) = map toLower $ take ((length xs) - 1) xs
                   stripQuote xs = map toLower xs

buildHeader :: B.ByteString -> (M.Map String (Maybe Int) )
buildHeader s = let ss = buildHeaderPrec s; sl = B.split '\t' s
                in M.fromList $ map  (\x -> (fst x , (snd x) `elemIndex` sl)) ss
maybeGet :: [B.ByteString] -> Maybe Int -> B.ByteString
[] `maybeGet` _ = B.pack ""
x `maybeGet` Nothing = B.pack ""
x `maybeGet` (Just s)= if length x > s then x !! s
                        else B.pack ""

getText :: (M.Map String (Maybe Int))-> [B.ByteString] -> String -> Maybe String
getText header ss id = if id `M.notMember` header
                         then Nothing
                         else let idx = header M.! id
                              in Just (B.unpack $ ss `maybeGet` idx)
getNum :: (M.Map String (Maybe Int))-> [B.ByteString] -> String -> Float
getNum header ss id = if id `M.notMember` header
                        then -1 :: Float
                        else let idx = header M.! id
                             in read (B.unpack $ ss `maybeGet` idx) :: Float
getNums :: (M.Map String (Maybe Int))-> [B.ByteString] -> [String] -> Either Float [Float]
getNums header ss ids = let nums = map (getNum header ss) ids
                         in if length nums == 1 then Left (nums !! 0)
                              else Right $ filter (not . (== -1)) nums
lineToCybert :: (M.Map String (Maybe Int))-> String  ->B.ByteString -> Cybert_entry
--take a header and an accumulator, then read the line and append the cybert entry
lineToCybert header fname line = readLine line where
    readLine s = let ss = B.split '\t' s 
                  in if length ss /= M.size header then cybert_entry
                     else let cybt= cybert_entry {
                         probe = B.unpack $ ss `maybeGet` (header M.! "probe_id"),
                         --this is a must
                         genesym = getText header ss "gene_sym",
                         --this is of maybe type
                         pval = getNum header ss "pval",
                         bf = getNum header ss "bonferroni",
                         bh = getNum header ss "bh",
                         collection = Just fname,
                         --these are -1 defaulted
                         mean = getNums header ss $ map ( "mean" ++ ) ["c","e","1","2","3","4","5","6","7"],
                         sds = getNums header ss $ map ( "std" ++ ) ["c","e","1","2","3","4","5","6","7"]
                         --these are one or many
                         , raw = s
                         --raw info
                         } in cybt

loadCybert :: String -> IO (Maybe [Cybert_entry])
loadCybert fname = catch
    (withFile fname ReadMode (\handle -> do
        contents <- B.hGetContents handle
        let mylines =  B.split '\n' contents
        if length mylines <= 1 then return Nothing
        else let header = buildHeader (head mylines);
                 output = Just (filter (not.isNothing.collection)
                                $ map (lineToCybert header fname) (drop 1 mylines))
             in if output == (Just []) then return Nothing
                  else return output
    ))
    (\err -> do
            if isEOFError err
            then do
                 putStrLn "File is empty or truncated."
                 return Nothing
            else do
                 putStrLn $ "Unexpected Error at opening file: "++ (show err)
                 return Nothing
    )
-- Loads a cybert table from fname
exportCybert :: [Cybert_entry] -> String -> IO()
exportCybert xs fname = do
    withFile fname WriteMode (\handle -> do
            let contents = showCybertEntries xs
            hPutStr handle contents
            )

exportGeneSyms :: [Cybert_entry] -> String -> IO()
exportGeneSyms xs fname = do
    withFile fname WriteMode (\handle -> do
            let contents = unlines (map showJustS (map genesym xs))
            hPutStr handle contents
            )

exportProbes :: [Cybert_entry] -> String -> IO()
exportProbes xs fname = do
    withFile fname WriteMode (\handle -> do
            let contents = unlines (map probe xs)
            hPutStr handle contents
            )
{-exportRef :: [Cybert_entry] -> String -> String -> IO()-}
{-end IO routines-}
