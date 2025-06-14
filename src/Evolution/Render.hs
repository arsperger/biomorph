module Evolution.Render
    ( renderBiomorphToImage
    , saveImage
    , loadImage
    , saveBiomorphGeneration
    , drawLine
    , scaleSegments
    , Evolution.Render.lastGeneration
    , calculateBounds
    , renderCombinedBiomorphs
    , scaleImage
    ) where

import Codec.Picture
import Codec.Picture.Types
import System.FilePath ((</>), (<.>), takeDirectory, takeExtension)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Control.Monad (forM_, when, forM, unless)
import Control.Monad.ST (ST, runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Directory (createDirectoryIfMissing, getDirectoryContents, canonicalizePath)
import Control.Exception (catch, SomeException)
import Data.Maybe (catMaybes)

import Evolution.Types
import Evolution.Biomorph (createSimpleTestBiomorph, Biomorph(..), Genotype, Segment(..), Point(..))

-- Calculate bounds of a biomorph
calculateBounds :: [Segment] -> (Point, Point)
calculateBounds segments =
    let allPoints = concatMap (\(Segment start finish) -> [start, finish]) segments
        minX = minimum $ map x allPoints
        minY = minimum $ map y allPoints
        maxX = maximum $ map x allPoints
        maxY = maximum $ map y allPoints
    in (Point minX minY, Point maxX maxY)

-- Scale segments to fit in the image
scaleSegments :: Int -> Int -> [Segment] -> [Segment]
scaleSegments width height segments =
    let (Point minX minY, Point maxX maxY) = calculateBounds segments
        rangeX = max 1 (maxX - minX)
        rangeY = max 1 (maxY - minY)
        
        -- Scale factor to fit in image with some padding
        scaleX = (fromIntegral width * 0.8) / rangeX
        scaleY = (fromIntegral height * 0.8) / rangeY
        scale = min scaleX scaleY
        
        -- Center offset
        centerX = fromIntegral width / 2
        centerY = fromIntegral height / 2
        
        -- Center of biomorph
        bioCenterX = (minX + maxX) / 2
        bioCenterY = (minY + maxY) / 2
        
        -- Transform function for a point
        transformPoint (Point px py) = Point
            (centerX + (px - bioCenterX) * scale)
            (centerY + (py - bioCenterY) * scale)
            
        -- Transform a segment
        transformSegment (Segment s e) = Segment
            (transformPoint s)
            (transformPoint e)
    in map transformSegment segments

-- Draw a line segment on the image using Bresenham's algorithm
drawLine :: PrimMonad m => MutableImage (PrimState m) Pixel8 -> Point -> Point -> m ()
drawLine img (Point x1 y1) (Point x2 y2) = do
    let width = mutableImageWidth img
        height = mutableImageHeight img
        
        -- Convert to integer coordinates
        ix1 = round x1
        iy1 = round y1
        ix2 = round x2
        iy2 = round y2
        
        -- Bresenham's line algorithm parameters
        dx = abs (ix2 - ix1)
        dy = abs (iy2 - iy1)
        
        sx = if ix1 < ix2 then 1 else -1
        sy = if iy1 < iy2 then 1 else -1
        
        initialErr = dx - dy -- Initial error term

        -- Helper to draw a point if within image bounds
        drawPoint' x yVal = 
            when (x >= 0 && x < width && yVal >= 0 && yVal < height) $
                writePixel img x yVal 255 -- Draw white pixel

    -- Bresenham's line drawing loop
    let loop currentX currentY currentErr = do
            drawPoint' currentX currentY
            unless (currentX == ix2 && currentY == iy2) $ do
                let e2 = 2 * currentErr
                -- Store next values
                let (nextX, errAfterX) = if e2 >= -dy then -- Condition for X step (using -dy as dy is positive)
                                           (currentX + sx, currentErr - dy)
                                         else
                                           (currentX, currentErr)
                
                let (nextY, finalErr) = if e2 <= dx then -- Condition for Y step (using dx)
                                          (currentY + sy, errAfterX + dx)
                                        else
                                          (currentY, errAfterX)
                
                -- Ensure progress is made to avoid infinite loops on single points after rounding
                if currentX == nextX && currentY == nextY && currentErr == finalErr && not (currentX == ix2 && currentY == iy2) then
                    return ()
                else
                    loop nextX nextY finalErr

    loop ix1 iy1 initialErr

-- Render a biomorph to a black and white image
renderBiomorphToImage :: Int -> Int -> Biomorph -> Image Pixel8
renderBiomorphToImage width height biomorph =
    let segs = segments biomorph
        segCount = length segs
        maxSegments = 1000
        limitedSegs = if segCount > maxSegments
                      then take maxSegments segs
                      else segs
    in
    runST $ do
        img <- createMutableImage width height 0 -- Black background
        let scaledSegmentsToDraw = scaleSegments width height limitedSegs
        forM_ scaledSegmentsToDraw $ \(Segment start finish) ->
            drawLine img start finish
        freezeImage img

-- Save image to file with better error handling
saveImage :: FilePath -> Image Pixel8 -> IO ()
saveImage path img = do
    putStrLn $ "Saving image: " ++ path ++ " with dimensions " ++ 
              show (imageWidth img) ++ "x" ++ show (imageHeight img)
    
    -- Check if the image is valid
    if imageWidth img == 0 || imageHeight img == 0
        then putStrLn "WARNING: Attempting to save an empty image!"
        else putStrLn $ "Image has " ++ show (V.length (imageData img)) ++ " pixels"
    
    -- Create directory if it doesn't exist
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    
    -- Use writePng with proper error handling
    putStrLn $ "Writing image to " ++ path
    catch (do
        writePng path img  -- Remove the ImageY8 wrapper
        putStrLn $ "Successfully saved image to " ++ path)
        (\e -> do
            putStrLn $ "Error saving image: " ++ show (e :: SomeException))

-- Load image from file
loadImage :: FilePath -> IO (Either String (Image Pixel8))
loadImage path = do
    result <- readImage path
    return $ case result of
        Left err -> Left err
        Right dynamicImg -> Right (convertToGrayscale dynamicImg)
  where
    -- Convert any dynamic image to grayscale (Pixel8)
    convertToGrayscale :: DynamicImage -> Image Pixel8
    convertToGrayscale img = 
        case img of
            ImageY8 y8 -> y8
            ImageYA8 ya8 -> pixelMap dropTransparency ya8
            ImageRGB8 rgb8 -> pixelMap rgbToGray rgb8
            ImageRGBA8 rgba8 -> pixelMap rgbaToGray rgba8
            ImageYCbCr8 ycbcr -> pixelMap (\(PixelYCbCr8 y _ _) -> y) ycbcr
            ImageCMYK8 cmyk -> pixelMap rgbToGray (convertRGB8 img)
            ImageYF yf -> pixelMap (\y -> round (255 * y)) yf
            ImageRGBF rgbf -> pixelMap rgbfToGray rgbf
            _ -> pixelMap rgbToGray (convertRGB8 img)
    
    -- RGB to grayscale conversion using luminance formula
    rgbToGray :: PixelRGB8 -> Pixel8
    rgbToGray (PixelRGB8 r g b) = 
        round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
    
    -- RGBA to grayscale (dropping alpha)
    rgbaToGray :: PixelRGBA8 -> Pixel8
    rgbaToGray (PixelRGBA8 r g b _) = 
        round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
    
    -- RGBF to grayscale
    rgbfToGray :: PixelRGBF -> Pixel8
    rgbfToGray (PixelRGBF r g b) =
        round (255 * (0.299 * r + 0.587 * g + 0.114 * b))
    
    -- Remove alpha channel from YA8
    dropTransparency :: PixelYA8 -> Pixel8
    dropTransparency (PixelYA8 y _) = y

-- Save a generation of biomorphs to individual files
saveBiomorphGeneration :: FilePath -> [Biomorph] -> IO ()
saveBiomorphGeneration dirPath biomorphsToSave = do
    createDirectoryIfMissing True dirPath
    forM_ (zip [0..] biomorphsToSave) $ \(idx, biomorphItem) -> do
        let filename = dirPath </> "biomorph_" ++ show idx <.> "png"
        let img = renderBiomorphToImage 150 150 biomorphItem
        saveImage filename img

lastGeneration :: FilePath -> IO [Biomorph]
lastGeneration dirPath = do
    canonPath <- canonicalizePath dirPath
    allFiles <- getDirectoryContents canonPath
    let pngFiles = filter ((== ".png") . takeExtension) allFiles
    loadedBiomorphs <- forM pngFiles $ \file -> do
        let fullPath = canonPath </> file
        result <- loadImage fullPath
        case result of
            Left err -> do
                putStrLn $ "Error loading " ++ fullPath ++ ": " ++ err
                return Nothing
            Right _ -> do
                return $ Just createSimpleTestBiomorph
    let successfulLoads = catMaybes loadedBiomorphs
    return successfulLoads

-- scaleImage function using bilinear interpolation
scaleImage :: Image Pixel8 -> Int -> Int -> Image Pixel8
scaleImage img newWidth newHeight = generateImage getPixel newWidth newHeight
  where
    (width, height) = (imageWidth img, imageHeight img)

    getPixel x y =
        let srcX = if newWidth == 0 then 0 else fromIntegral x * fromIntegral width / fromIntegral newWidth
            srcY = if newHeight == 0 then 0 else fromIntegral y * fromIntegral height / fromIntegral newHeight
            x1 = floor srcX
            y1 = floor srcY
            -- Ensure coordinates are within the source image bounds
            x2 = min (width - 1) (x1 + 1)
            y2 = min (height - 1) (y1 + 1)
            safeX1 = max 0 x1
            safeY1 = max 0 y1
            safeX2 = max 0 x2
            safeY2 = max 0 y2

            dx = srcX - fromIntegral x1
            dy = srcY - fromIntegral y1

            p11 = if width == 0 || height == 0 then 0 else fromIntegral $ pixelAt img safeX1 safeY1
            p21 = if width == 0 || height == 0 then 0 else fromIntegral $ pixelAt img safeX2 safeY1
            p12 = if width == 0 || height == 0 then 0 else fromIntegral $ pixelAt img safeX1 safeY2
            p22 = if width == 0 || height == 0 then 0 else fromIntegral $ pixelAt img safeX2 safeY2

            -- Bilinear interpolation
            p1 = p11 * (1 - dx) + p21 * dx
            p2 = p12 * (1 - dx) + p22 * dx
            p = p1 * (1 - dy) + p2 * dy
        in round p

-- Render combined biomorphs into a single image
renderCombinedBiomorphs :: Int -> Int -> [Biomorph] -> Image Pixel8
renderCombinedBiomorphs width height biomorphsToCombine =
    if null biomorphsToCombine then
        generateImage (\_ _ -> 0) width height
    else
        let combineBounds :: (Point, Point) -> (Point, Point) -> (Point, Point)
            combineBounds (Point minX1 minY1, Point maxX1 maxY1) (Point minX2 minY2, Point maxX2 maxY2) =
                (Point (min minX1 minX2) (min minY1 minY2),
                 Point (max maxX1 maxX2) (max maxY1 maxY2))

            validBiomorphs = filter (not . null . segments) biomorphsToCombine

            _overallBounds = if null validBiomorphs
                                 then (Point 0 0, Point (fromIntegral width) (fromIntegral height))
                                 else foldr1 combineBounds (map (calculateBounds . segments) validBiomorphs)

            allSegments = concatMap (\bm -> scaleSegments width height (segments bm)) validBiomorphs
        in
        runST $ do
            img <- createMutableImage width height 0
            forM_ allSegments $ \(Segment start finish) ->
                drawLine img start finish
            freezeImage img