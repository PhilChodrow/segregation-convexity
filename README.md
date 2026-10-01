# About 

This repository contains code and $\LaTeX$ source for generating the article "Measuring Diversity and Segregation with Convex Functions" by Phil Chodrow. 

## Requirements

Running individual scripts in this repository requires a recent installation of `R` with packages `tidyverse`, `systemfonts`, `sf`, `patchwork`, `tidycensus`, `tigris`, `ggtern`, and `nleqslv`. Modification to `scripts/style.R` may be necessary for users whose systems do not include the `Avenir` font family. A recent installation of $\LaTeX$ and the `latexmk` utility are also required. 

After configuring the `R` intallation, users with experience with `snakemake` may choose to run `snakemake --cores 4` to build the project start-to-finish. Users unfamiliar with `snakemake` may consult the `snakefile`, which describes the dependency structure between scripts and outputs. 

## Compilation

With `.vscode/tasks.json` configured, running `shift+cmd+b` will result in a call to `snakemake`. 

