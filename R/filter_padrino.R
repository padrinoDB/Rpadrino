#' @title Subset a Padrino database object
#'
#' @description currently only works by passing \code{ipm_id}. Will extend later
#' to use any variable.
#'
#' @param db The database object to subset
#' @param ... Logical conditions to use for subsetting
#'
#' @return An object of class \code{PadrinoDB}
#'
#' @author Sam Levin
#'
#' @importFrom rlang enquos !!! eval_tidy
#'
#' @export
#'


padrino_filter <- function(db, ...) {

  condition <- rlang::enquo(...)

  output <- lapply(seq_along(db),
                   function(x){
                     sub <- rlang::eval_tidy(condition, data = db[[x]])
                     db[[x]][sub, ]
    }
  )

  return(output)

}
