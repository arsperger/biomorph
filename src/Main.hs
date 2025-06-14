module Main where

import Control.Monad (unless, forM_)
import System.Environment (getArgs)
import System.Random (newStdGen)
import System.IO (hFlush, stdout)
import Codec.Picture (Image, Pixel8)
import Codec.Picture.Types (imageWidth, imageHeight)

import Evolution.Types (Biomorph(..), EvolutionStats(..), Point(..), Segment(..))
import Evolution.Biomorph (createSimpleTestBiomorph)
import Evolution.Render (saveImage, loadImage, saveBiomorphGeneration, renderBiomorphToImage)
import Evolution.Fitness ()
import Evolution.Evolution (runEvolution)

-- Helper function to print debug info and flush stdout
debug :: String -> IO ()
debug msg = do
    putStrLn $ "DEBUG: " ++ msg
    hFlush stdout

main :: IO ()
main = do
    args <- getArgs
    debug $ "Arguments: " ++ show args

    case args of
        ["--test", outputPath] -> do
            debug "Running in test mode with simple biomorph"
            let testBiomorph = createSimpleTestBiomorph -- From Evolution.Biomorph
            debug $ "Test biomorph genotype: " ++ show (genotype testBiomorph)
            debug $ "Test biomorph active indices: " ++ show (activeSkeletalIndices testBiomorph)
            debug $ "Test biomorph has " ++ show (length (segments testBiomorph)) ++ " segments"

            let image = renderBiomorphToImage 150 150 testBiomorph -- From Evolution.Render
            debug $ "Test image dimensions: " ++ show (imageWidth image) ++ "x" ++ show (imageHeight image)
            saveImage outputPath image
            debug "Test image saved"

        [targetPath, outputPath, populationSizeStr] -> do
            let populationSize = read populationSizeStr :: Int
            debug $ "Population size: " ++ show populationSize

            debug $ "Loading target image from: " ++ targetPath
            targetImgResult <- loadImage targetPath
            case targetImgResult of
                Left err -> do
                    putStrLn $ "Error loading target image: " ++ err
                    debug "Image loading failed"

                Right img -> do
                    debug $ "Target image loaded successfully, dimensions: " ++
                           show (imageWidth img) ++ "x" ++ show (imageHeight img)

                    gen <- newStdGen
                    debug "Random generator initialized"

                    putStrLn "Starting evolution..."
                    let (bestBiomorph, stats) = runEvolution gen populationSize img
                    debug $ "Evolution completed with " ++ show (length stats) ++ " generations"
                    debug $ "Best biomorph has " ++ show (length (segments bestBiomorph)) ++ " segments"
                    debug $ "Best fitness: " ++ show (fitnessValue bestBiomorph) -- Directly use fitnessValue

                    debug "Rendering best biomorph to 150x150 image"
                    let finalImage = renderBiomorphToImage 150 150 bestBiomorph
                    debug $ "Final image dimensions: " ++ show (imageWidth finalImage) ++ "x" ++ show (imageHeight finalImage)

                    debug $ "Saving image to: " ++ outputPath
                    saveImage outputPath finalImage
                    debug "Image saved"

                    putStrLn $ "Evolution completed after " ++ show (length stats) ++ " generations"
                    putStrLn $ "Final fitness: " ++ show (fitnessValue bestBiomorph) -- Use fitnessValue
                    putStrLn $ "Output saved to: " ++ outputPath

                    debug "Saving final generation biomorphs"
                    -- Accessing last generation's biomorphs from EvolutionStats
                    let finalPopBiomorphs = Evolution.Types.lastGeneration (head stats) -- Assuming stats are in reverse chronological order
                    saveBiomorphGeneration "output/final_generation" finalPopBiomorphs
                    debug "Final generation saved"

        _ -> putStrLn "Usage: evolution <target-image> <output-image> <population-size> OR evolution --test <output-path>"