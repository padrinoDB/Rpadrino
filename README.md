
[![R-CMD-check](https://github.com/levisc8/RPadrino/workflows/R-CMD-check/badge.svg)](https://github.com/levisc8/RPadrino/actions)
[![Codecov test
coverage](https://codecov.io/gh/levisc8/RPadrino/branch/master/graph/badge.svg)](https://codecov.io/gh/levisc8/RPadrino?branch=master)

## RPadrino

`RPadrino` is a package for interacting with the Padrino Integral
Projection Model database. This database really only exists as an
internal dataset in this package right now. The longer term plan is to
migrate it to a set of static, version-controlled tables that are hosted
on Zenodo (or similar). For now, you can access it by installing the
package, and running `data(pdb)`.

Data have had some quality checking, but some (in fact, most) models
will still definitely break with very strange looking error messages. A
genuine, quality checked release will be tagged when it is actually
ready to go. I really don’t suggest using this yet, except to get a feel
for how the database looks and perhaps give some feedback on things you
do or do not like.

## Scope

The goal of this package is basic data management, exploration, and
porting Padrino’s internal syntax to
[`ipmr`’s](https://levisc8.github.io/ipmr/) syntax. Ideally, one could
combine models from here with their own models for synthesis work, using
the `proto_ipm` as a common data structure to power the analysis.

### Installation

You can install RPadrino from github with:

``` r
if(!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("levisc8/RPadrino")
```

### Usage

A small-ish copy of the Padrino Database is included as a package data
set. You can access it with the following code:

``` r
library(RPadrino)

data("pdb")
```

The next step is to identify the models we are interested in building.
At the moment, there is no functionality to do that - you’ll just have
to explore the *Metadata* table on your own. In the not so distant
future, there will be a set of functions that actually help query
specific columns of interest so the process of exploring the database is
less painful.

Once, we’ve identified the model(s) we want, we can build a list of
[`proto_ipm`’s](https://levisc8.github.io/ipmr/articles/proto-ipms.html).
This is an intermediate step between the database representation and a
set of kernels. Once we have the list of `proto_ipm`’s, we can build an
actual model. Below, we extract a specific study, and construct an IPM
from the database object.

``` r
# We can construct a single IPM at a time, or make a list of many IPMs

proto_list   <- pdb_make_proto_ipm(pdb, 
                                   ipm_id = c("aaaa55", "aaa341", "aaa342"),
                                   det_stoch = "det")
```

Once the list of `proto_ipm`’s is generated, you can either append your
own IPMs to it, or you can go ahead to the next chunk. Note that
`pdb_make_ipm` is still pretty unstable, and it’ll probably mean that
error messages aren’t the most informative.

``` r
ipm_list <- pdb_make_ipm(proto_list)

lams     <- lambda(ipm_list)
```

### Finding help

The package is documented [here](https://levisc8.github.io/RPadrino/).

### Contributing

Please note that the RPadrino project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
