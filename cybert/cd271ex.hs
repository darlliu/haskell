-- reproduce the analysis for CD271 pvals, functional style
--
import Cybert
import Control.Monad
import Data.Maybe
import Data.Set (fromList, toList, union, empty)
extract:: Maybe [a] -> [a]
extract Nothing = []
extract (Just x) = x
pfind x = map (filter (\y -> probe y == probe x))
    -- find entries with same probeid
countln = foldr (\z acc -> acc + (length z)) 0
    -- count cross lists num
loadCybert' = loadCybert cybert_header
main = do
    all_combined <- loadCybert' "./CyberT_Output/paired_2/CD271_all_together0.txt"
    let all_up = (entriesByPval 0.05) $ (entriesByUpDown True)  (extract all_combined)
        all_down = (entriesByPval 0.05) $ (entriesByUpDown False)  (extract all_combined)
    ; exportCybert (extract all_combined) "all_probes_refs.txt"
    ; exportCybert all_up "all_up_refs.txt"
    ; exportCybert all_down "all_down_refs.txt"
    ; onesM <- mapM loadCybert' $ map ("./CyberT_Output/paired_2/CD271_one_one" ++ ) ["0.txt","1.txt","2.txt","3.txt"]
    ; pairsM <- mapM loadCybert' ["./CyberT_Output/paired_2/CD271_two_two_" ++i ++ j ++ ".txt" | i <- ["1","2","3"], j<-["0","1"] ]
    let ones = map extract (all_combined:onesM)
        pairs = map extract (all_combined:pairsM)
    let up = map  (entriesByPval 0.05)  (map (entriesByUpDown True) pairs)
        down = map  (entriesByPval 0.05)  (map (entriesByUpDown False) pairs)
        five_up = map (filter (\x ->  countln (pfind x up) >= 5 )) up
        five_down =map (filter (\x -> countln (pfind x down) >= 5 )) down
        five_ups = foldl union empty (map cybertToSet five_up)
        five_downs = foldl union empty (map cybertToSet five_down)
        seven_up = map (filter (\x ->  countln (pfind x up) >= 7 )) up
        seven_down =map (filter (\x -> countln (pfind x down) >=7  )) down
        seven_ups = foldl union empty (map cybertToSet seven_up)
        seven_downs = foldl union empty (map cybertToSet seven_down)
    ;  exportCybert (toList five_ups) "five_up_refs.txt" 
    ;  exportCybert (toList five_downs) "five_down_refs.txt" 
    ;  exportCybert (toList seven_ups) "seven_up_refs.txt" 
    ;  exportCybert (toList seven_downs) "seven_down_refs.txt" 
    ;  older' <- loadCybert' "./CyberT_Output/paired_2/CD271_two_two_13.txt"
    ;  xeno' <- loadCybert' "./CyberT_Output/paired_2/CD271_one_one4.txt"
    let older = extract older'
        xeno = extract xeno'
        older_ups = entriesByPval 0.05  (entriesByUpDown True older)
        older_downs = entriesByPval 0.05 (entriesByUpDown False older)
    ;  exportCybert older_ups "older_patients_up.txt"
    ;  exportCybert older_downs "older_patients_down.txt"
    let xeno_ups = entriesByFold 1 $ entriesByUpDown (True) xeno
        xeno_downs = entriesByFold 1 $ entriesByUpDown (False) xeno
    ;  exportCybert xeno_ups "xeno_up.txt"
    ;  exportCybert xeno_downs "xeno_down.txt"
