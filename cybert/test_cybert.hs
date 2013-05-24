import Cybert

main = do
    let c = cybert_entry{genesym=(Just "Sym"), mean = (Right [1,2,3])}
    let d = cybert_entry{genesym=(Just "Sym2"), mean = (Right [1,2,3])}
    exportCybert [c,d] "tested.txt"
    some <- loadCybert "tested.txt"
    putStrLn (show some)
