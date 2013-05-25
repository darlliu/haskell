import Cybert

main = do
    {-
     -let c = cybert_entry{genesym=(Just "Sym"), mean = (Right [1,2,3])}
     -let d = cybert_entry{genesym=(Just "Sym2"), mean = (Right [1,2,3])}
     -}
    ssome@(Just some)<- loadCybert "test.txt"
    exportGeneSyms some "tested.txt"
