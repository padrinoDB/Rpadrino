#' @noRd
# creates named list of text expressions. names correspond to kernel or vital
# rate names, and entries are the right hand sides of the formulae

.rm_brackets <- function(form) {

  temp <- lapply(form, function(x) {
    temp <- gsub('\\s*\\[[^\\]]+\\]',
                 '',
                 x,
                 perl = TRUE)
  })


  out_nms <- vapply(temp, function(x) strsplit(x, '=')[[1]][1],
                    character(1L))
  out     <- lapply(temp, function(x) strsplit(x, '=')[[1]][2])

  names(out) <- trimws(out_nms)
  return(out)
}


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

utils::globalVariables(c("."), add = FALSE)

#' @importFrom utils getFromNamespace
#' @importFrom purrr flatten

.flatten_to_depth <- getFromNamespace(".flatten_to_depth", 'ipmr')
