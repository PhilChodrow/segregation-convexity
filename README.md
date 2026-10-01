# About 

This repository contains code and $\LaTeX$ source for generating the article "Measuring Diversity and Segregation with Convex Functions" by Phil Chodrow. 

## Abstract

The problem of *segregation measurement* is to quantitatively describe the interaction of population demographics (such as race, ethnicity, education level, or income) with some kind of separating distinction (such as organizational rank, spatial location, or school district). 
This article gives an opinionated tour of one mathematical approach to measuring segregation. 
We begin with some basic intuitions about diversity and formalize these through the unifying framework of convex functions on the probability simplex. 
Formalizing segregation as a local-to-global comparison of diversity measures, resulting in the general class of Jensen informations as segregation measurements. 
We describe and computationally illustrate three ways to incorporate spatial structure into segregation measurements: spatial smoothing, aggregation, and local Jensen information. 
We close with several suggestions for future work. 




## Requirements

Running individual scripts in this repository requires a recent installation of `R` with packages `tidyverse`, `systemfonts`, `sf`, `patchwork`, `tidycensus`, `tigris`, `ggtern`, and `nleqslv`. Modification to `scripts/style.R` may be necessary for users whose systems do not include the `Avenir` font family. A recent installation of $\LaTeX$ and the `latexmk` utility are also required. 

After configuring the `R` intallation, users with experience with `snakemake` may choose to run `snakemake --cores 4` to build the project start-to-finish. **This also requires an installation of ImageMagick.** Users unfamiliar with `snakemake` may consult the `snakefile`, which describes the dependency structure between scripts and outputs. Users who wish to use `snakemake` without ImageMagick may do so by deleting lines beginning with `magick` from the `snakefile`. 
