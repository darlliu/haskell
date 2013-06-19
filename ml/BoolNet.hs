module BoolNet 
(
    Feature{
    probe,
    genesym,
    pval,
    b
    },
    extractFeatures,
    extractFeature
) where

import Data.Bool
import Data.Maybe
import Data.Either
import qualified Data.Bits as Bit
import qualified Cybert as C

data Feature = Feature {
    probe :: String ,
    genesym :: Maybe String ,
    pval :: Float ,
    b :: Maybe Bool --true, false, or nothing (insignificant)
} deriving (Show, Read)
feature = Feature {
    probe = "None",
    genesym = Just "None",
    pval = 1,
    b = Nothing
}

extractFeature :: Float -> C.Cybert_entry -> Feature
extractFeature threshold cybt = feature {
    probe = C.probe cybt,
    genesym = C.genesym cybt,
    pval = C.pval cybt,
    b = if (C.pval cybt) > threshold then Nothing
        else either (\x -> Nothing)
                    (\x -> if (length x) < 2 then Nothing
                            else Just( x!!0 > x!!1))
                    (C.mean cybt)
    }
extractFeatures = map extractFeature

