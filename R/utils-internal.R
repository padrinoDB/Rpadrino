#' @noRd

# Utility function to weed out parameter assignment operations. These are an
# unfortunate legacy in the database from when I wasn't sure how to work with
# rlang.

.is_the_same <- function(formula) {
  temp <- strsplit(formula, '=') %>%
    unlist() %>%
    trimws()

  isTRUE(identical(temp[1], temp[2]))
}

#' @importFrom utils globalVariables

utils::globalVariables(c(".",
                         "wrld_map",  "long", "lat",
                         "group", "coords", "lon"), add = FALSE)

#' @importFrom utils getFromNamespace
#' @importFrom purrr flatten

.flatten_to_depth <- utils::getFromNamespace(".flatten_to_depth", 'ipmr')

#' @noRd
# Function to recursively extract argument names from text representations
# of an expression. This returns a character vector of all arguments a call,
# and does not track what the function call actually is.

.args_from_txt <- utils::getFromNamespace(".args_from_txt", "ipmr")


#' @noRd
#  Generates var-covar matrix for a given set of values

sig_mat <- function(...) {

  vals <- c(...)

  dims <- sqrt(length(vals))

  out  <- matrix(vals, nrow = dims, ncol = dims, byrow = TRUE)

  return(out)

}

#' @noRd
# Utitlity to create functions for pdb metadata accessors

.make_pdb_accessor <- function(col) {

  force(col)

  fun <- function(pdb, ipm_id = NULL) {

    if(is.null(ipm_id)) {

      ipm_id <- unique(pdb[[1]]$ipm_id)

    }

    out <- pdb[[1]][ , col][pdb[[1]]$ipm_id %in% ipm_id]

    return(out)
  }


  return(fun)
}
