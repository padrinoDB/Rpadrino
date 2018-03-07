
<!-- README.md is generated from README.Rmd. Please edit that file -->
RPadrino
--------

`RPadrino` is a package for interacting with the demography data base of the same name. It will likely consist of wrappers to an SQL data base that itself houses a suite of all known, published integral projection models (IPMs). Additionally, we'll build in some basic tools to re-create the IPMs *as they were created in the original publication*.

We will create a separate package (`IPMr`) that is dedicated to analyzing and manipulating the data as well, but that is a separate task that does not fall under the scope of this particular project.

### Installation

You can install RPadrino from github with:

``` r
# install.packages("devtools")
devtools::install_github("levisc8/RPadrino")
```

### Contributing

Feel free to add additional contributors to this project if you think they will be able to make substantive contributions. The goal for this package is to create a data management tool, **not** a data analysis tool (that will come in `IPMr`), so keep that in mind when designing your functions.
