
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iSEEindex

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/iSEE/iSEEindex)](https://github.com/iSEE/iSEEindex/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/iSEE/iSEEindex)](https://github.com/iSEE/iSEEindex/pulls)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check-bioc](https://github.com/iSEE/iSEEindex/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/iSEE/iSEEindex/actions)
[![Codecov test
coverage](https://codecov.io/gh/iSEE/iSEEindex/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iSEE/iSEEindex?branch=main)
<!-- badges: end -->

The goal of `iSEEindex` is to …

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `iSEEindex` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("iSEEindex")
```

And the development version from
[GitHub](https://github.com/iSEE/iSEEindex) with:

``` r
BiocManager::install("iSEE/iSEEindex")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("iSEEindex")
## basic example code
```

## Citation

Below is the citation output from using `citation('iSEEindex')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEindex**.

``` r
print(citation('iSEEindex'), bibtex = TRUE)
#> It is recommended to use 'given' instead of 'middle'.
#> 
#> To cite package 'iSEEindex' in publications use:
#> 
#>   Kevin Christophe Rue (2022). iSEEindex: iSEE extension for a landing
#>   page to a custom collection of data sets. R package version 0.99.0.
#>   https://github.com/iSEE/iSEEindex
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {iSEEindex: iSEE extension for a landing page to a custom collection of data sets},
#>     author = {Kevin Christophe Rue},
#>     year = {2022},
#>     note = {R package version 0.99.0},
#>     url = {https://github.com/iSEE/iSEEindex},
#>   }
```

Please note that the `iSEEindex` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `iSEEindex` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

-   Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through *[usethis](https://CRAN.R-project.org/package=usethis)*,
    *[remotes](https://CRAN.R-project.org/package=remotes)*, and
    *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)*
    customized to use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    *[BiocCheck](https://bioconductor.org/packages/3.15/BiocCheck)*.
-   Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and
    *[covr](https://CRAN.R-project.org/package=covr)*.
-   The [documentation website](http://isee.github.io/iSEEindex) is
    automatically updated thanks to
    *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
-   The code is styled automatically thanks to
    *[styler](https://CRAN.R-project.org/package=styler)*.
-   The documentation is formatted thanks to
    *[devtools](https://CRAN.R-project.org/package=devtools)* and
    *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.15/biocthis)*.

## Code of Conduct

Please note that the iSEEindex project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
