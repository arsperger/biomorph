module Evolution.Evolution 
    ( runEvolution
    ) where

import System.Random
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Codec.Picture (Image, Pixel8)

import Evolution.Types
import Evolution.Biomorph
import Evolution.Fitness

-- Generate population of random biomorphs
generateInitialPopulation :: RandomGen g => g -> Int -> ([Biomorph], g)
generateInitialPopulation gen size =
    let go 0 g_acc acc_biomorphs = (reverse acc_biomorphs, g_acc)
        go n g_acc acc_biomorphs =
            let (genotype, activeIdx, g_new) = createRandomGenotype g_acc
                biomorph = biomorphFromGenotype genotype activeIdx
            in go (n-1) g_new (biomorph : acc_biomorphs)
    in go size gen []

-- Generate offspring from a parent biomorph
generateOffspring :: RandomGen g => g -> Int -> Biomorph -> ([Biomorph], g)
generateOffspring gen numOffspring parent =
    foldr (\_ (offspring_acc, g_fold) ->
        let (child, g_fold') = mutateBiomorph parent g_fold
        in (child:offspring_acc, g_fold'))
        ([], gen) [1..numOffspring]

-- Evaluate fitness of a population against target
evaluatePopulation :: Image Pixel8 -> [Biomorph] -> [Biomorph]
evaluatePopulation targetImg = map (evaluateBiomorph targetImg)

-- Select best biomorph from population
selectBest :: [Biomorph] -> Biomorph
selectBest = maximumBy (comparing (fromMaybe 0 . fitnessValue))

-- Check if evolution has stagnated
hasStagnated :: Int -> [EvolutionStats] -> Bool
hasStagnated maxStagnantGens stats
    | length stats < maxStagnantGens = False
    | otherwise =
        let recentStats = take maxStagnantGens stats
            bestFitness = maximum $ map fitness recentStats
            worstFitness = minimum $ map fitness recentStats
            improvement = bestFitness - worstFitness
        in improvement < 0.0001  -- Very small improvement threshold

-- Run one generation of evolution
evolveGeneration :: RandomGen g => g -> Int -> Image Pixel8 -> [Biomorph] -> (g, [Biomorph], Double)
evolveGeneration gen populationSize targetImg population =
    let -- Evaluate current population
        evaluatedPopulation = evaluatePopulation targetImg population
        
        -- Select best individual as parent
        parent = selectBest evaluatedPopulation
        parentFitness = fromMaybe 0 (fitnessValue parent)
        
        -- Generate offspring (populationSize - 1 to keep parent in new population)
        (offspring, newGen) = generateOffspring gen (populationSize - 1) parent
        
        -- New population is parent + offspring
        newPopulation = parent : offspring
    in (newGen, newPopulation, parentFitness)

-- Run the full evolutionary process
runEvolution :: RandomGen g => g -> Int -> Image Pixel8 -> (Biomorph, [EvolutionStats])
runEvolution initialGen populationSize targetImg =
    let -- Generate initial population
        (initialPopulation, gen1) = generateInitialPopulation initialGen populationSize
        
        -- Evaluate initial population
        evaluatedInitialPop = evaluatePopulation targetImg initialPopulation
        initialBest = selectBest evaluatedInitialPop
        initialFitness = fromMaybe 0 (fitnessValue initialBest)
        
        -- Initial stats
        initialStats = [EvolutionStats 0 initialFitness evaluatedInitialPop 0]
        
        -- Main evolution loop
        evolveLoop gen stats currentPop =
            let currentGen = length stats
                (newGen, newPop, newFitness) = evolveGeneration gen populationSize targetImg currentPop
                
                -- Check if fitness improved
                prevFitness = fitness (head stats)
                stagnantCount = if newFitness > prevFitness + 0.0001
                               then 0
                               else stagnantGenerations (head stats) + 1
                
                -- New stats
                newStats = EvolutionStats currentGen newFitness (evaluatePopulation targetImg newPop) stagnantCount : stats
            in
                -- Check termination criteria
                if stagnantCount >= 10 || currentGen >= 50  -- Add max generation limit
                then (selectBest (evaluatePopulation targetImg newPop), reverse newStats)
                else evolveLoop newGen newStats newPop
    in
        evolveLoop gen1 initialStats initialPopulation