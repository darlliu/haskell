import Cybert
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Control.Monad
import qualified Control.Exception as E
import System.Directory
import System.IO
import qualified Control.Monad.Parallel as P
{-import Data.Set(fromList, toList, union, empty)-}

cheader' = M.fromList [("Secondary_Data_FDR","fdr"), ("Secondary_Data_FPR","fpr"),("Secondary_Data_Pval1","pval1")]
cheader  = M.union (cheader') (M.adjust (\x->"accession") "probe" cybert_header)
loadCybert' = loadCybert cheader
targetDirs= ["NatData_runs","XlinkData_runs"]

{-utility fctns for writing to file -}
stringify sep xs = foldl1 (\x y ->x++sep++ y) xs

writeString fname content = do
    withFile fname WriteMode (\handle -> do
        hPutStr handle content)

mvroot :: FilePath -> FilePath -> IO Bool
mvroot root cur = do
   E.catch
    (
        do
         createDirectoryIfMissing True (cur ++"/ontargets")
         createDirectoryIfMissing True (cur ++"/scatters")
         createDirectoryIfMissing True (cur ++"/avgs")
         setCurrentDirectory  (cur++"/runs")
         return True
    )
    (\e-> do
        let err = show (e::E.IOException)
        putStr $ stringify "\n" ["Error Chaging to directory\n" , cur,
           "Err:", err]
        return False
    );

{-extractTargets :: [Cybert_entry] -> IO()-}

exportScatter fname xs = do
    let ss = map (\x-> let d = secondaryData x
                        in stringify "\t" [probe x, show (d M.! "pval1"),show (d M.! "fdr")]) xs
    (writeString ("../scatters/"++ fname)) (stringify "\n" ss)

exportTargets fname xs = do
    exportProbes xs ("../ontargets/" ++ fname)
    let ss = map (\x-> let d = secondaryData x
                        in stringify "\t"
                            [probe x, showEitherF (mean x), show (d M.! "pval1"),
                             show (d M.! "fpr"), show (d M.! "fdr")]) xs
    writeString ("../targetdetails/"++ fname) $ 
        stringify "\n" (["accession\tmean\tpvalue(one-tail)\tFPR\tFDR"]++ss)
exportAVG fn xs = do
    let xxs = map fn xs
    let avg = if length xs >=2 then (foldl1 (+) xxs) / (fromIntegral (length xxs))
               else 1.0
    let gname = if length xs == 0 then "Unknown"
                else fromMaybe "Unknown" (collection $ (xs !! 0))
    let ss = gname ++ "\t" ++ (show avg) ++ "\n"
    return ss
subroutine:: (Maybe [Cybert_entry]) -> IO ([String])
subroutine xs = do
    let xxs'= fromMaybe [] xs
    let fname = if length xxs' == 0 then "Nill.dummy"
                else fromMaybe "Nill.dummy" (collection $ xxs' !! 0)
    ; exportScatter fname xxs'
    {-; exportAVG "../avgs/Total_AVG_mean.tsv" mean xxs'-}
    let xxs = filter (\x -> let d = (secondaryData x)
                              in if (d M.! "fdr") < 0.0 then (pval x) < 0.05
                             {-in case where beta fitting fails-}
                                 else (d M.! "pval1") <0.05 
                                 {-&& (d M.! "fdr") < 0.1-}
                     ) (entriesByUpDown True xxs')
    ; putStrLn  (fname ++" , "++(show $ length xxs))
    ; exportTargets fname xxs
    ; tot_p <- exportAVG pval  xxs'
    ; tot_f <- exportAVG (\x->let d = secondaryData x in d M.! "fdr") xxs'
    ; tar_p <- exportAVG pval xxs
    ; tar_f <- exportAVG (\x->let d = secondaryData x in d M.! "fdr") xxs
    return [tot_p,tot_f,tar_p,tar_f]
    {-; exportAVG "../avgs/Targets_AVG_mean.tsv"  xxs'-}

routine :: FilePath -> FilePath -> IO()
routine root cur = do
    flag<-mvroot root cur;
    if flag then do
        filenames' <- getDirectoryContents "."
        let filenames = filter (\x -> "_cybert_result.tsv" `isInfixOf` x) filenames'
        collections <- mapM loadCybert' filenames
        putStrLn $ show filenames
        {-acquire cybert entries -}
        avgs <- P.mapM subroutine collections
        writeString "../avgs/Total_AVG_pval.tsv" (stringify "\n" (map (\x->x!!0) avgs))
        writeString "../avgs/Total_AVG_fdr.tsv" (stringify "\n" (map (\x->x!!1) avgs))
        writeString "../avgs/Targets_AVG_pval.tsv" (stringify "\n" (map (\x->x!!2) avgs))
        writeString "../avgs/Targets_AVG_fdr.tsv" (stringify "\n" (map (\x->x!!3) avgs))
        {-E.evaluate $ runEval $ do-}
            {-parMap (rpar.subroutine) collections-}
            {-parMap rseq parTasks-}
        {-use pval of 0.05 and fpr of 0.1 to filter-}
        putStr $ "\nHandled" ++ cur ++"\n"
        setCurrentDirectory root
    else do
        putStr "\nNothing is done\n"
        setCurrentDirectory root

main = do
{-first get stuff from the "runs" folder-}
     curdir <- getCurrentDirectory;
     mapM (routine curdir) targetDirs

