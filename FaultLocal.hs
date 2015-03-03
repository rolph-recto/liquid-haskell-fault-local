module FaultLocal (
  Constraint(..),
  ConstraintMap,
  constraintLocs,
  constraintWeight, constraintSum,
  rankLocs, getFaultLocal
) where

import Data.List
import Ast

-- constraints are just strings for now
type Constraint = String
-- map constraints to list of program locations
type ConstraintMap = [(Constraint, [Expr])]

-- mapping of constraints to program locations
constraintLocs :: ConstraintMap -> Constraint -> Maybe [Expr]
constraintLocs ctl c = lookup c ctl

-- weighting function for constraint
constraintWeight :: ConstraintMap -> Constraint -> Double
constraintWeight ctl c =
  case constraintLocs ctl c of
    Nothing -> 0.0
    Just locs -> fromIntegral (sum $ map exprSize locs) :: Double

-- total weight for a set of constraints
constraintSum :: ConstraintMap -> [Constraint] -> Double
constraintSum ctl cs = sum $ map (constraintWeight ctl) cs


-- tf-idf weighting metric for prog locs
-- boolean tf is used because prog locs 
-- can only occur once
tf :: [Expr] -> Expr -> Double
tf doc term = if term `elem` doc then 1.0 else 0.0

idf :: ConstraintMap -> [Expr] -> Expr -> Double
idf corpus doc term = log (n / nDocsWithTerm)
  where n             = fromIntegral (length corpus) :: Double
        docs          = map (\(_,doc) -> doc) corpus
        docsWithTerm  = filter (\doc -> term `elem` doc) docs
        nDocsWithTerm = fromIntegral (length docsWithTerm) :: Double

tfIdf :: ConstraintMap -> [Expr] -> Expr -> Double
tfIdf corpus doc term = (tf doc term) * (idf corpus doc term)

-- heuristic #1: only consider locs in all constraints of minset
intersectLocs :: [[Expr]] -> [Expr]
intersectLocs cs = filter inAll locs
  where locs     = nub $ cs >>= id
        inAll    = \loc -> all (\c -> loc `elem` c) cs

-- heuristic #2: rank prog locs by tf-idf
rankTfIdf :: ConstraintMap -> [Expr] -> [(Expr,Double)]
rankTfIdf ctl locs = sortBy sortPair rankedPairs
  where tfIdfPair   = \l -> (l,tfIdf ctl locs l)
        rankedPairs = map tfIdfPair locs
        sortPair    = \(_,tfidf1) (_,tfidf2) -> compare tfidf1 tfidf2


-- rank locations by tf-idf
-- basically, applies heuristics 1 and 2
rankLocs :: ConstraintMap -> [Constraint] -> [(Expr,Double)]
rankLocs ctl minset = reverse $ rankTfIdf ctl intersect
  where minsetPairs = filter (\(doc,_) -> doc `elem` minset) ctl
        cs          = map (\(_,val) -> val) minsetPairs
        intersect   = intersectLocs cs


-- returns the most likely fault location
getFaultLocal :: ConstraintMap -> [Constraint] -> Expr
getFaultLocal ctl minset = fst $ head $ rankLocs ctl minset
