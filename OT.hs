module OT

-- This file handles handles example OT Grammar, pair distributions, and the
-- tableaus we'll use for the EDPRL algorithms. 

type Constraint = String

-- Optimality Theory Grammar (OTG)
data OTG = OTGWith [Constraint]

-- Pair Distribution
type PairDistribution = [(String, String, Int)]
--              (underlying form, output form, frequency count)


-- Example from Tesar & Smolensky (2000)
TSOTGrammar :: OTG
TSOTGrammar = OTGWith [ "WSP",
                        "FtNnf",
                        "Iambi",
                        "Parse",
                        "FtBin",
                        "WFL",
                        "WFR",
                        "MainL",
                        "MainR",
                        "AFL",
                        "AFR",
                        "Nonfn" ]

TSPairDistribution :: PairDistribution
TSPairDistribution = [ ("L-L", "L1-L", 1),
                       ("L-H", "L1-H", 1),
                       ("H-L", "H1-L", 1) ]

