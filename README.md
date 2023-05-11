
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

The goal of `iSEEindex` is to provide an interface to any collection of
data sets, hosted anywhere, within a single iSEE web-application.

The main functionality of this package is to define a custom landing
page for iSEE web-applications where app maintainers can list entirely
custom collections of data sets hosted on virtually any locally or
remotely accessible network.

A number of built-in methods are implemented, providing access to common
types of resources, e.g.:

- Local filesystem
- HTTPS URIs
- Amazon S3 buckets

Each type of resource is identified by the scheme component of its URI.
Standard schemes can be used alongside custom-made ones, e.g.:

- `localhost://` for local files.
- `https://` for files downloaded over the HTTPS protocol.
- `s3://` for files downloaded from Amazon S3 buckets.

The `iSEEindex` framework enables app maintainers to independently
define new methods for their own choice of standard and custom-made URI
schemes. More information is available in the vignette *Implementing
custom iSEEindex resources*.

The resulting landing page presents end-users of the web-applications
with the predefined choice of data sets and initial configuration states
(specific to each data set). After selecting a data set and – optionally
– an initial configuration, launching the main app fetches resources
from their respective URI and caches them using the
*[BiocFileCache](https://bioconductor.org/packages/3.16/BiocFileCache)*
package. Finally, data sets and configurations are loaded from the cache
into the main `iSEE` application, for interactive exploration.

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

This is a basic example which shows you how to launch an application
that lists publicly available data sets hosted on
[zenodo.org](https://zenodo.org/record/7304331):

``` r
library("iSEEindex")
library("BiocFileCache")

bfc <- BiocFileCache(cache = tempdir())

dataset_fun <- function() {
    x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
    x$datasets
}

initial_fun <- function() {
    x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
    x$initial
}

app <- iSEEindex(bfc, dataset_fun, initial_fun)

if (interactive()) {
    shiny::runApp(app, port = 1234)
}
```

## Citation

Below is the citation output from using `citation('iSEEindex')` in R.
Please run this yourself to check for any updates on how to cite
**iSEEindex**.

``` r
print(citation('iSEEindex'), bibtex = TRUE)
#> 
#> To cite package 'iSEEindex' in publications use:
#> 
#>   Rue-Albrecht K (2022). _iSEEindex: iSEE extension for a landing page
#>   to a custom collection of data sets_. R package version 0.99.0,
#>   <https://github.com/iSEE/iSEEindex>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {iSEEindex: iSEE extension for a landing page to a custom collection of data sets},
#>     author = {Kevin Rue-Albrecht},
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

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.16/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation website](http://isee.github.io/iSEEindex) is
  automatically updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.16/biocthis)*.

<!-- Links -->
