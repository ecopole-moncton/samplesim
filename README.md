
<!-- README.md is generated from README.Rmd. Please edit that file -->

# samplesim <img src="man/figures/hexsticker.png" height="120" align="right"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/ecopole-moncton/samplesim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ecopole-moncton/samplesim/actions/workflows/R-CMD-check.yaml)
[![Website
deployment](https://github.com/ecopole-moncton/samplesim/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ecopole-moncton/samplesim/actions/workflows/pkgdown.yaml)
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![LifeCycle](man/figures/lifecycle/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of the R package `samplesim` is to estimate sample size effects
in stable isotope mixing solutions.

## System requierements

The package `samplesim` requires the freeware
[JAGS](https://mcmc-jags.sourceforge.io/).

This package will require either the packages
[MixSIAR](https://cran.r-project.org/web/packages/MixSIAR/index.html),
[simmr](https://cran.r-project.org/web/packages/simmr/) or
[siar](https://github.com/AndrewLJackson/siar) (which is now defunct and
thus not recommended any more. If you insist we recommend installing it
using the following command:

``` r
remotes::install_github("AndrewLJackson/siar@v.4.2.4"))
```

## Installation

You can install the stable version of `samplesim` with:

``` r
## Install stable version of 'samplesim' ----
install.packages("samplesim")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
## Install 'remotes' package (if not already installed) ----
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## Install development version of 'samplesim' ----
remotes::install_github("ecopole-moncton/samplesim")
```

Then you can attach the package `samplesim`:

``` r
library("samplesim")
```

## Overview

Please read the
[Vignette](https://ecopole-moncton.github.io/samplesim/articles/samplesim.html)

## Citation

Please cite this package as:

> Lecomte N., Ehrich D., Casajus N., Berteaux D., Cameron C., and Yoccoz
> N.G. How many is enough? An R package for evaluating the effect of
> sample size on estimates and precision of stable isotope mixing
> solutions. Submitted to *Methods in Ecology and Evolution*.

Or alternatively:

> Casajus N., Cameron C., Ehrich D., and Lecomte N. (2021) samplesim: An
> R package to investigate sample size effects in stable isotope mixing
> solutions. R package version 1.0. URL:
> <https://github.com/ecopole-moncton/samplesim>.

## Code of Conduct

Please note that the `samplesim` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
