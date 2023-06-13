module EDPRL where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import PRG
import OT



-- Throughout this file, we will implement two versions of the Expectation
-- Driven Pairwise Ranking Learner (ODPRL)
-- 1) Batch version
-- 2) Online version

-- Some constants to be used. Can be configured as well.
sampleSize = 10

-- Some general helper functions used by both versions

-- Sample stochastically selects a total order, based on the PRG
-- It might be more accurate if this returned the actual order, instead
-- of the just the updated PRG with the set values


-- Batch Expectation Driven Pairwise Ranking Learner (Batch EDPRL)

-- Step 1: Initialize expected values

-- Step 2: The "E" Step
getDatum :: PairDistribution 

getAllConsPairs :: PRG -> [(Constraint, Constraint)]


estep :: PairDistribution -> PRG -> PRG
estep (d:ds) prg =
    let constraintPairs = getAllConsPairs prg
        -- 1) Temporarily set A >> B
        -- 2) Find matches
        -- 3) Temporarily set B >> A
        -- 4) Find matches
        -- 5) Update expectation.
        -- 6) Iterate for next pair of "unset" constraints
    in 




-- Step 3: The "M" Step
updateParams :: PRG -> PRG
updateParams prg = prg

-- Iterate once steps 1-3
iterate :: Int -> PRG -> PRG
iterate n prg = prg

-- Iterate n times steps 1-3


-- Online Expectation Driven Pairwise Ranking Learner (Online EDPRL)

