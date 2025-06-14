# Biomorph Evolution Simulator

This program simulates an evolutionary process inspired by Richard Dawkins' "The Blind Watchmaker".<br/>
It generates biomorphs, which are geometric figures based on a genotype and evolves them.

## Project Description

-   **Biomorphs**: Geometric figures composed of connected segments, exhibiting characteristics derived from their genotype.
-   **Genotype**: A sequence of 16 genes.
    -   15 skeletal genes (values -9 to +9): Define the biomorph's structure. 7 of these are randomly chosen as active for generation.
    -   1 length gene (values 2 to 12): Defines the biomorph's overall length/scale.
-   **Evolution**:
    -   Starts with a randomly generated population of N biomorphs.
    -   Mutations: In each generation, one gene of an offspring is altered by +/- 1.
    -   Selection: Fitness is determined by similarity to a target image (e.g., Euclidean distance).
    -   Termination: Evolution stops if fitness doesn't improve for 10 generations or a maximum generation count is reached.
-   **Output**: Biomorphs are rendered as 150x150 pixel black and white images.

## Building the Project

This project uses Cabal.

```bash
cabal build
```

## Running the Application

Evolution Mode

To run the evolution simulation:

```bash
cabal run evolution -- <target-image-path> <output-image-path> <population-size>
```

## Arguments:

- **target-image-path**: Path to the image the biomorphs will evolve towards (e.g., test-images/circle.png).
- **output-image-path**: Path where the image of the best evolved biomorph will be saved (e.g., output/evolved_biomorph.png).
- **population-size**: The number of biomorphs in each generation (e.g., 20).

## Example:

```bash
cabal run evolution -- test-images/circle.png output/result.png 50
```

Test Mode

To generate and save a single, predefined test biomorph:

```bash
cabal run evolution -- --test <output-image-path>
```
