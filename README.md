Le premier commit de ce répertoire a été effectué le 16/04/2026 à 9h12

# Réponse à l'exo:
git checkout master
git commit -m "commit A"
git -b branch1
git commit -m "commit B"
git commit -m "commit C"
git checkout master
git merge branch1
git commit -m "commit D"

# ncaa.analytics.royer

<!-- badges: start -->
<!-- badges: end -->

The goal of ncaa.analytics.royer is to ...

## Installation

You can install the development version of ncaa.analytics.royer from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("royertsara/ncaa_analytics.royer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ncaa.analytics.royer)
## basic example code
```

