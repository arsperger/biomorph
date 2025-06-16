module Evolution.Fitness 
    ( calculateFitness
    , evaluateBiomorph
    ) where

import Codec.Picture
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import qualified Data.List as L

import Evolution.Types
import Evolution.Render (renderBiomorphToImage, scaleImage)

import Debug.Trace
import Statistics.Sample (mean, variance)

-- Calculate similarity between two images using normalized cross-correlation
normalizedCrossCorrelation :: Image Pixel8 -> Image Pixel8 -> Double
normalizedCrossCorrelation img1 img2 =
    let (width1, height1) = (imageWidth img1, imageHeight img1)
        (width2, height2) = (imageWidth img2, imageHeight img2)
        
        -- Make sure images have same dimensions
        img1' = if width1 /= width2 || height1 /= height2
                then scaleImage img1 width2 height2
                else img1
                
        -- Convert images to vectors for faster computation
        vec1 = imageData img1'
        vec2 = imageData img2
        
        -- Calculate correlation
        -- Convert to Double before calculating mean
        mean1 = fromIntegral (V.sum vec1) / fromIntegral (V.length vec1)
        mean2 = fromIntegral (V.sum vec2) / fromIntegral (V.length vec2)
        
        -- Ensure all calculations are done with Double values
        vec1Centered = V.map (\p -> fromIntegral p - mean1) vec1
        vec2Centered = V.map (\p -> fromIntegral p - mean2) vec2
        
        numerator = V.sum $ V.zipWith (*) vec1Centered vec2Centered
        denominator = sqrt (V.sum (V.map (^2) vec1Centered)) * 
                      sqrt (V.sum (V.map (^2) vec2Centered))
    in if denominator == 0 then 0 else numerator / denominator

-- Calculate Euclidean distance between two images
euclideanDistance :: Image Pixel8 -> Image Pixel8 -> Double
euclideanDistance img1 img2 =
    let (width1, height1) = (imageWidth img1, imageHeight img1)
        (width2, height2) = (imageWidth img2, imageHeight img2)
        
        -- Make sure images have same dimensions
        img1' = if width1 /= width2 || height1 /= height2
                then scaleImage img1 width2 height2
                else img1
                
        -- Convert images to vectors for faster computation
        vec1 = V.map fromIntegral $ imageData img1'
        vec2 = V.map fromIntegral $ imageData img2

        -- Calculate squared differences
        squaredDiffs = V.zipWith (\a b -> (a - b)^2) vec1 vec2
        --sumSquaredDiff = trace ("!!VEC1 " ++ show vec1) (V.sum squaredDiffs)
        sumSquaredDiff = V.sum squaredDiffs

        -- Normalize by number of pixels
        normalizedDist = if V.null vec1 then 0 else sqrt (sumSquaredDiff) / fromIntegral (V.length vec1)
    in if V.length vec1 == 0 then 1.0 else 1.0 - (normalizedDist / 255.0)

-- Calculate SSIM between two images
ssim :: Image Pixel8 -> Image Pixel8 -> Double
ssim img1 img2 =
    let pixels1 = V.map fromIntegral $ imageData img1  -- Vector Double
        pixels2 = V.map fromIntegral $ imageData img2
        mu1 = mean pixels1
        mu2 = mean pixels2
        var1 = variance pixels1
        var2 = variance pixels2
        covar = V.sum (V.zipWith (*) pixels1 pixels2) / n - mu1 * mu2
        n = fromIntegral $ V.length pixels1
        c1 = (0.01 * 255)^2  -- Константы для стабилизации
        c2 = (0.03 * 255)^2
    in (2*mu1*mu2 + c1) * (2*covar + c2) / ((mu1^2 + mu2^2 + c1) * (var1 + var2 + c2))

-- Calculate fitness of a biomorph against target image
calculateFitness :: Image Pixel8 -> Biomorph -> Double
calculateFitness targetImg biomorph =
    let biomorphImg = renderBiomorphToImage 150 150 biomorph
    --let biomorphImg = trace ("!!Segments count is:\n" ++ show (length (segments biomorph)) ++ " indices is:" ++ show(activeSkeletalIndices biomorph)) (renderBiomorphToImage 150 150 biomorph)
        --similarity = euclideanDistance targetImg biomorphImg
        similarity = ssim targetImg biomorphImg
    in similarity

-- Evaluate a biomorph's fitness against target and cache the result
evaluateBiomorph :: Image Pixel8 -> Biomorph -> Biomorph
evaluateBiomorph targetImg biomorphToEval@(Biomorph genotype activeIdx segs _) =
    let fitness = calculateFitness targetImg biomorphToEval
    in Biomorph genotype activeIdx segs (Just fitness)