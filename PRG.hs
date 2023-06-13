module PRG where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

-- Some type synonyms for convenience
type Prob = Float
type Constraint = String


-- ==============================
-- Pairwise Ranking Grammar (PRG)
-- ==============================
-- The outer-layer Map's Constraint key is mapped to an inner-layer Map.
-- The inner-layer Map's Constraint key maps to a pairwise ranking probability
-- of the outer-layer's Constaint ranking above the inner-layer's Constraint.
-- 
-- If you're looking at Figure 1/Figure 2's style of tables, the outer-layer
-- selects from the vertical axis, and the inner-layer looks at that row's
-- probability values.
--
type PRG = Map Constraint (Map Constraint Prob)


-- This function Looks up the pairwise ranking probability of the first 
-- Constraint ranking above the second Constraint.
--
-- Example usage:
--     > cell figure1PRG ("A", "B")
--     > Just 1 
--     > cell figure2PRG ("C", "B")
--     > Just 0.3
cell :: PRG -> (Constraint, Constraint) -> Prob
cell prg (con1, con2) =
    let Just innerMap = Map.lookup con1 prg
        Just prob = Map.lookup con2 innerMap
    in prob

-- Updates the pairwise ranking probability of a cell and its inverse cell
-- with the respective value
updateCell :: PRG -> (Constraint, Constraint) -> Float -> PRG
updateCell prg (con1, con2) newProb =
    let Just innerMap = Map.lookup con1 prg
        updatedInnerMap = Map.adjust (\v -> newProb) con2 innerMap
        updatedOuterMap = Map.adjust (\v -> updatedInnerMap) con1 prg

        Just inverseInnerMap = Map.lookup con2 updatedOuterMap
        updatedInverseInnerMap = Map.adjust (\v -> 1 - newProb) con1 inverseInnerMap
        updatedInverseOuterMap = Map.adjust (\v -> updatedInverseInnerMap) con2 updatedOuterMap

    in updatedInverseOuterMap

-- Similar to above, but do so with the pairwise ranking of a constraint from a list vs
-- the second constraint
updateCellList :: PRG -> ([Constraint], Constraint) -> Prob -> PRG

updateCellList prg ([], con2) newProb = prg

updateCellList prg ((c:cs), con2) newProb =
    let Just innerMap = Map.lookup c prg
        updatedInnerMap = Map.adjust (\v -> newProb) con2 innerMap
        updatedOuterMap = Map.adjust (\v -> updatedInnerMap) c prg

        Just inverseInnerMap = Map.lookup con2 updatedOuterMap
        updatedInverseInnerMap = Map.adjust (\v -> 1 - newProb) c inverseInnerMap
        updatedInverseOuterMap = Map.adjust (\v -> updatedInverseInnerMap) con2 updatedOuterMap
    in updateCellList updatedInverseOuterMap (cs, con2) newProb


-- Find the possible total orderings of a set PRG in (highest ranked, ..., lowest ranked) order.
findTotalOrderings :: PRG -> [[Constraint]]
findTotalOrderings prg = []

-- Returns a list of Constraint pairs that are "unset" (i.e. pairwise ranking
-- probability is not 0 and not 1) 
findUnsetPairs :: PRG -> [(Constraint, Constraint)]
findUnsetPairs prg =
    [(con1, con2) | (con1, innerMap) <- Map.toList prg, 
                          (con2, prob) <- Map.toList (Map.filter (\prob -> and [prob /= 0, prob /= 1]) innerMap)]

-- Sets the selected cell in a PRG based on the respective pairwise ranking
-- probability, and returns the updated PRG
-- Note: this function does not yet perform transivity tests (IT WILL LATER THROUGH RECURSION I PROMISE)
setCell :: PRG -> (Constraint, Constraint) -> PRG
setCell prg (con1, con2) =
    let rand = 0.5 -- TODO install random package: applyAtomicGen (uniformR (0, 1)) globalStdGen
        currentCell = cell prg (con1, con2)
        updatedPRG = updateCell prg (con1, con2) (if currentCell >= rand then 1 else 0)
    in updatedPRG


-- Checks for transivity relations that need to be updated in relation to a given cell.
-- Updates those rankings and return the new PRG
checkTransitivity :: PRG -> (Constraint, Constraint) -> PRG
checkTransitivity prg (con1, con2) =
    let aboveCon1 = rankedAbove prg con1
        belowCon2 = rankedBelow prg con2
        aboveRankUpdatedPRG = updateCellList prg (aboveCon1, con2) 1
        belowRankUpdatedPRG = updateCellList aboveRankUpdatedPRG (belowCon2, con1) 0
        -- TODO: what does the author mean in "Finally, any constraints just ranked above M must be..."
        -- TODO: do I need to recurse here? probably
    in belowRankUpdatedPRG


-- Some helper functions for checking rankings/orderings

-- Returns list of constraints ranked above the given constraint
rankedAbove :: PRG -> Constraint -> [Constraint]
rankedAbove prg con =
    let Just innerMap = Map.lookup con prg
        filteredMap = Map.filter (\v -> v == 0) innerMap
    in [c | (c, _) <- Map.toList filteredMap]

-- Returns list of constraints ranked below the given constraint
rankedBelow :: PRG -> Constraint -> [Constraint]
rankedBelow prg con = 
    let Just innerMap = Map.lookup con prg
        filteredMap = Map.filter (\v -> v == 1) innerMap
    in [c | (c, _) <- Map.toList filteredMap]

-- This is the "main" sample function that encompasses all the helper
-- functions above, and will be used in EDPRL.hs
-- It sets all unset cells.
sample :: PRG -> PRG 
sample prg =
    let unsetPairs = findUnsetPairs prg
    in case unsetPairs of 
        (p:ps) -> sample (setCell prg p)
        [] -> prg

-- Example PRG (from Figure 1)
figure1PRG :: PRG
figure1PRG = Map.fromList [ ("A", Map.fromList [ ("B", 1  ),
                                                 ("C", 1  ),
                                                 ("D", 0  ) ]),
                            ("B", Map.fromList [ ("A", 0  ),
                                                 ("C", 0.7),
                                                 ("D", 0  ) ]),
                            ("C", Map.fromList [ ("A", 0  ),
                                                 ("B", 0.3),
                                                 ("D", 0  ) ]), 
                            ("D", Map.fromList [ ("A", 1  ),
                                                 ("B", 1  ),
                                                 ("C", 1  ) ]) ]

-- Another Example PRG (from Figure 2)
figure2PRG :: PRG
figure2PRG = Map.fromList [ ("A", Map.fromList [ ("B", 1  ),
                                                 ("C", 1  ),
                                                 ("D", 1  ) ]),
                            ("B", Map.fromList [ ("A", 0  ),
                                                 ("C", 0.5),
                                                 ("D", 0.5) ]),
                            ("C", Map.fromList [ ("A", 0  ),
                                                 ("B", 0.5),
                                                 ("D", 1  ) ]), 
                            ("D", Map.fromList [ ("A", 0  ),
                                                 ("B", 0.5),
                                                 ("C", 0  ) ]) ]

