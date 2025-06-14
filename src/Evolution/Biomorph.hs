module Evolution.Biomorph
    ( createRandomGenotype
    , calculateStems
    , renderBiomorph
    , mutateBiomorph
    , biomorphFromGenotype
    , createSimpleTestBiomorph
    , renderCreature
    , Gene(..)
    , Point(..)
    , Segment(..)
    , Stems(..)
    , Biomorph(..)
    , Genotype
    ) where

import System.Random
import Control.Monad (replicateM)
import Data.List (transpose)
import qualified Data.Set as Set

import Evolution.Types

-- Create a random gene within bounds
randomGene :: RandomGen g => (Int, Int) -> g -> (Gene, g)
randomGene (minVal, maxVal) gen =
    let (val, newGen) = randomR (minVal, maxVal) gen
    in (Gene val minVal maxVal, newGen)

-- Generates n distinct random integers in the range [minVal, maxVal]
generateDistinctIndices :: RandomGen g => Int -> (Int, Int) -> g -> ([Int], g)
generateDistinctIndices n (minVal, maxVal) gen
    | n < 0 = error "generateDistinctIndices: n cannot be negative"
    | n == 0 = ([], gen)
    | n > (maxVal - minVal + 1) = error "generateDistinctIndices: Cannot generate n distinct numbers from the given range"
    | otherwise = go Set.empty n gen
  where
    go selectedIndices count g
        | count == 0 = (Set.toList selectedIndices, g)
        | otherwise =
            let (idx, g') = randomR (minVal, maxVal) g
            in if Set.member idx selectedIndices
               then go selectedIndices count g' -- Try again
               else go (Set.insert idx selectedIndices) (count - 1) g'

-- Create a list of genes
createGenesList :: RandomGen g => (Int, Int) -> Int -> g -> ([Gene], g)
createGenesList range count g =
    foldr (\_ (acc, g_fold) -> let (gene_item, g_fold') = randomGene range g_fold in (gene_item:acc, g_fold')) ([], g) [1..count]

-- Create a random genotype with 16 genes and 7 active skeletal indices
createRandomGenotype :: RandomGen g => g -> (Genotype, [Int], g)
createRandomGenotype gen =
    let (skeletalGenes, gen1) = createGenesList (-9, 9) 15 gen
        (lengthGene, gen2) = randomGene (2, 12) gen1
        fullGenotype = skeletalGenes ++ [lengthGene]
        (activeIndexSelection, gen3) = generateDistinctIndices 7 (0, 14) gen2 -- Select 7 distinct indices from 0..14
    in (fullGenotype, activeIndexSelection, gen3)

-- Calculate the 8 directional stems from genotype using active indices
calculateStems :: Genotype -> [Int] -> Stems
calculateStems genes activeIndices =
    let getActiveGeneVal idxInActiveList = geneValue $ genes !! (activeIndices !! idxInActiveList)
    in Stems -- Uses 7 distinct active genes, indexed 0 to 6 from activeIndices list
        [ Point 0 (fromIntegral $ getActiveGeneVal 0)
        , Point (fromIntegral $ getActiveGeneVal 1) (fromIntegral $ getActiveGeneVal 2)
        , Point (fromIntegral $ getActiveGeneVal 3) 0
        , Point (fromIntegral $ getActiveGeneVal 4) (fromIntegral $ getActiveGeneVal 5)
        , Point 0 (-(fromIntegral $ getActiveGeneVal 6)) -- 7th active gene
        , Point (-(fromIntegral $ getActiveGeneVal 4)) (-(fromIntegral $ getActiveGeneVal 5)) -- Reuse 5th, 6th
        , Point (-(fromIntegral $ getActiveGeneVal 3)) 0 -- Reuse 4th
        , Point (-(fromIntegral $ getActiveGeneVal 1)) (fromIntegral $ getActiveGeneVal 2) -- Reuse 2nd, 3rd
        ]

-- Internal function to render biomorph segments from genotype and active indices
renderBiomorph :: Genotype -> [Int] -> [Segment]
renderBiomorph genotype activeIndices =
    let stems = calculateStems genotype activeIndices
        len = geneValue $ last genotype -- Last gene is always length gene
    in renderCreature len stems 0 (Point 0 0) 0

-- Helper function for recursive rendering with depth control
renderCreature :: Int -> Stems -> Int -> Point -> Int -> [Segment]
renderCreature len (Stems stemPoints) dir oldPos depth
    | len <= 0 = []
    | depth >= 10 = []
    | otherwise =
        let newDir = ((dir `mod` 8) + 8) `mod` 8
            stemVector = stemPoints !! newDir
            newPos = Point
                (x oldPos + fromIntegral len * x stemVector)
                (y oldPos + fromIntegral len * y stemVector)
            thisSegment = Segment oldPos newPos
            leftBranch = if len > 1
                then renderCreature (len - 1) (Stems stemPoints) (dir + 1) newPos (depth + 1)
                else []
            rightBranch = if len > 1
                then renderCreature (len - 1) (Stems stemPoints) (dir - 1) newPos (depth + 1)
                else []
        in thisSegment : (leftBranch ++ rightBranch)

-- Mutate a single gene in the genotype
mutateGene :: RandomGen g => Gene -> g -> (Gene, g)
mutateGene (Gene val minVal maxVal) gen =
    let (direction, newGen) = random gen
        delta = if direction then 1 else -1
        newVal = max minVal (min maxVal (val + delta))
    in (Gene newVal minVal maxVal, newGen)

-- Mutate a biomorph by changing one random gene; active indices remain the same
mutateBiomorph :: RandomGen g => Biomorph -> g -> (Biomorph, g)
mutateBiomorph (Biomorph oldGenotype activeIdx _ _) gen =
    let (indexToMutate, gen1) = randomR (0, length oldGenotype - 1) gen
        (mutatedGene, gen2) = mutateGene (oldGenotype !! indexToMutate) gen1
        newGenotype = take indexToMutate oldGenotype ++ [mutatedGene] ++ drop (indexToMutate + 1) oldGenotype
        newSegments = renderBiomorph newGenotype activeIdx -- Re-render with new genotype
    in (Biomorph newGenotype activeIdx newSegments Nothing, gen2)

-- Create a biomorph from genotype and active indices
biomorphFromGenotype :: Genotype -> [Int] -> Biomorph
biomorphFromGenotype gt activeIdx =
    let segs = renderBiomorph gt activeIdx
    in Biomorph gt activeIdx segs Nothing

-- Create a simple test biomorph
createSimpleTestBiomorph :: Biomorph
createSimpleTestBiomorph =
    let initialGenes = replicate 15 (Gene 0 (-9) 9) ++ [Gene 5 2 12] -- 16 genes
        fixedActiveIndices = [0, 1, 2, 3, 4, 5, 6]
        testGeno = [ Gene 1 (-9) 9, Gene 1 (-9) 9, Gene 1 (-9) 9, Gene 1 (-9) 9
                   , Gene 1 (-9) 9, Gene 1 (-9) 9, Gene 1 (-9) 9
                   , Gene 0 (-9) 9, Gene 0 (-9) 9, Gene 0 (-9) 9
                   , Gene 0 (-9) 9, Gene 0 (-9) 9, Gene 0 (-9) 9
                   , Gene 0 (-9) 9, Gene 0 (-9) 9
                   , Gene 5 2 12 ]
    in biomorphFromGenotype testGeno fixedActiveIndices