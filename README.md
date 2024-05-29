
<!-- README.md is generated from README.Rmd. Please edit that file -->

# snapr

<img src="man/figures/snapr-hex.png"  align="right" height="300" style="float:right; height:300px;">

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end --> snapr is a proof of concept R package to interact
with [The ESA Sentinel Applications Platform
(SNAP)](https://step.esa.int/main/toolboxes/snap/) from R. SNAP provides
an impressive set of tools for processing satellite data, many of which
are not available in the wider R ecosystem. snapr is a wrapper for
SNAP’s Graph Processing Tool (command line interface) and shares
similar ambitions to the
[SNAPISTA](https://github.com/snap-contrib/snapista) python library.

## Installation

You can install the development version of snapr like so:

``` r
#install.packages("pak")
pak::pkg_install("Permian-Global-Research/snapr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(snapr)
## basic example code
```
