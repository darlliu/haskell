-- A program to find out outlier values
-- in a tsv file of Cyber-T input type and then 
-- either impute or delete the outliers
import Data.CSV.Conduit
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Control.Monad
import Control.Exception
import Data.List

-- First, load the tsv as special csv
-- Then, obtain dataframe and 
--
main :: IO ()
main = do
    let myOption = CSVSettings{csvSep='\t', csvQuoteChar=Nothing}
    csvData <- readCSVFile myOption "./txts/test_csv.txt" :: IO (V.Vector (Row Text))
    V.mapM (\x-> putStrLn $ intercalate "," $ map unpack x) csvData
    return ()

