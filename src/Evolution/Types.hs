module Evolution.Types where

import Data.Word (Word8)
import Codec.Picture (Image, Pixel8)

-- Gene has a value and valid range
data Gene = Gene 
    { geneValue :: Int
    , geneMin   :: Int
    , geneMax   :: Int
    } deriving (Show, Eq)

-- Genotype is a collection of genes
type Genotype = [Gene]

-- Point in 2D space
data Point = Point 
    { x :: Double
    , y :: Double 
    } deriving (Show, Eq)

-- Line segment between two points
data Segment = Segment 
    { start  :: Point
    , finish :: Point 
    } deriving (Show, Eq)

-- Biomorph contains genotype and its rendered segments
data Biomorph = Biomorph
    { genotype              :: Genotype
    , activeSkeletalIndices :: [Int]
    , segments              :: [Segment]
    , fitnessValue          :: Maybe Double
    } deriving (Show)

-- Direction vectors for 8 possible directions
data Stems = Stems [Point]

-- Statistics for evolutionary process
data EvolutionStats = EvolutionStats
    { generation :: Int
    , fitness :: Double
    , lastGeneration :: [Biomorph]
    , stagnantGenerations :: Int
    } deriving (Show)