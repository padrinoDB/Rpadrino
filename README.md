
[![R-CMD-check](https://github.com/padrinoDB/Rpadrino/workflows/R-CMD-check/badge.svg)](https://github.com/padrinoDB/Rpadrino/actions)
[![Codecov test
coverage](https://codecov.io/gh/padrinoDB/Rpadrino/branch/main/graph/badge.svg)](https://codecov.io/gh/padrinoDB/Rpadrino?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/Rpadrino)](https://cran.r-project.org/package=Rpadrino)
[![DOI](https://zenodo.org/badge/124245125.svg)](https://zenodo.org/badge/latestdoi/124245125)

## Rpadrino

`Rpadrino` is a package for interacting with the PADRINO Integral
Projection Model database. Currently, an alpha release of PADRINO exists
as a set of version controlled csv files hosted in a GitHub repository.
The longer term plan is to migrate it to a set of static,
version-controlled tables that are hosted on Zenodo (or similar). For
now, you can access it by installing the `Rpadrino` *R* package from
this repository (see below for instructions).

## Installation

CRAN version:

``` r
install.packages("Rpadrino")
```

Or the development version (may not be stable!):

``` r
if(!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("padrinoDB/Rpadrino")
```

## Scope

The goal of this package is basic data management, exploration, and
porting PADRINO’s internal syntax to
[`ipmr`’s](https://levisc8.github.io/ipmr/) syntax. One can combine
models from here with their own models for synthesis work, using the
`proto_ipm` as a common data structure to power the analysis.

## Usage

The data included in this package are quality checked (see below) and
ready for use. This is not the complete database - that can be
downloaded using the `pdb_download` function. See the table below for
information on the number of models in each.

| Data Source           | \# of Species | \# of Publications | \# of IPM id’s |
|:----------------------|--------------:|-------------------:|---------------:|
| Internal Data         |            36 |                 22 |             77 |
| Full PADRINO Database |            56 |                 40 |            280 |

| Kingdom  | \# of Species | \# of Publications | \# of IPM id’s |
|:---------|--------------:|-------------------:|---------------:|
| Animalia |            16 |                 14 |             22 |
| Plantae  |            40 |                 26 |            258 |

Above are the current number of unique species, unique publications, and
unique `ipm_id`s that are in PADRINO and `Rpadrino`’s internal dataset.
Table 2 provides a breakdown of how these are distributed amongst
taxonomic kingdoms in the **full** database (i.e. not the internal data
set). All of the models included in both the internal data set and full
database have been quality checked for accuracy. Quality checked means
that for deterministic models, the asymptotic per-capita growth rate
(![\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda "\lambda"))
is within
![\\pm 0.03](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpm%200.03 "\pm 0.03")
of the published point estimate value. For stochastic models, we check
for approximately the same stochastic population growth rate
(![\\lambda_s](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_s "\lambda_s")),
but do not try to replicate the analysis, as this usually requires too
many computing resources to be feasible.

A small-ish copy of the Padrino Database is included as a package data
set. You can download the full database as well. You can access it with
the following code:

``` r
library(Rpadrino)

data("pdb") # Internal copy, contains a subset of plant species in PADRINO

# Downloads the complete database. The models contained in here have 
# been tested for accuracy/reasonable results.  
pdb <- pdb_download(save = FALSE) 
```

The next step is to identify the models we are interested in building.
There are accessor functions that allow you to extract metadata columns
from the `pdb` object, which may be helpful in this step. See
`?pdb_species_accepted` for a list of those. Additionally, the
[shiny_pdb](https://sam-levin.shinyapps.io/shiny_pdb/) allows for
interactive exploration and subsetting of data.

Once, we’ve identified the model(s) we want, we can build a list of
[`proto_ipm`’s](https://levisc8.github.io/ipmr/articles/proto-ipms.html).
This is an intermediate step between the database representation and a
set of kernels. Once we have the list of `proto_ipm`’s, we can build an
actual model. Below, we extract a specific study, and construct an IPM
from the database object.

``` r
# We can construct a single IPM at a time, or make a list of many IPMs

proto_list   <- pdb_make_proto_ipm(pdb, 
                                   ipm_id = c("aaa310", "ddddd7", "aaaa17"),
                                   det_stoch = "det")
```

Once the list of `proto_ipm`’s is generated, you can either append your
own IPMs to it, or you can go ahead to the next chunk. Note that
`pdb_make_ipm` will almost always work, but there may still be bugs
lurking!

``` r
ipm_list <- pdb_make_ipm(proto_list)

lams     <- lambda(ipm_list)
```

### Finding help

The package is documented [here](https://padrinoDB.github.io/Rpadrino/).

### Contributing

Please note that the Rpadrino project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
