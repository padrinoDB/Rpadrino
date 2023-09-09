# Rpadrino 0.0.5

This contains an update to keep `RPadrino` on CRAN. Namely, it checks for internet before trying to download `PADRINO`, and returns the internal `pdb` object if there is not internet available. This adds [`curl`](https://jeroen.r-universe.dev/curl) as a dependency.

# Rpadrino 0.0.4

This contains new features, and a couple changes for compatibility with `ipmr 0.0.5`. 

## Features

- Accessor functions for the Metadata table now have `ipm_id`s as names. This enables patterns for subsetting like this:

```
spps <- pdb_species_accepted(pdb)
ids  <- names(spps)[spps %in% c("Carpobrotus_spp", "Geum_radiatum")]
my_proto_ipms <- pdb_make_proto_ipm(pdb, ipm_id = ids)

```

- New vignettes on working with stochastic IPMs, and on the messages that are produced while building IPMs with `pdb_make_proto_ipm()` and `pdb_make_ipm()`.

- `pdb_report()` now contains equations for each IPM using `ipmr`'s `make_ipm_report_body()`. 

    + NB: This is currently unavailable for [parameter set indexed IPMs](https://padrinoDB.github.io/ipmr/articles/index-notation.html).

# Rpadrino 0.0.3

- Fixed some issues from two of CRAN's Linux machines.

# Rpadrino 0.0.2

- Fixed some issues from CRAN's Solaris machine.

# Rpadrino 0.0.1

This is the first stable release of `Rpadrino` and is available on CRAN. It includes functionality to download the [PADRINO IPM Database](https://github.com/padrinoDB/Padrino), explore and subset data, and rebuild IPMs. Additionally, it wraps [ipmr](https://padrinoDB.github.io/ipmr/reference/index.html) methods to get and set vital rate expressions, IPM sub-kernel formulae, parameter values, and more. Explore all the features in the [vignettes](https://padrinodb.github.io/Rpadrino/articles/padrino-intro.html) and report bugs on the Issue tracker in this repository.
