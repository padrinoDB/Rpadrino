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
#' @importFrom dplyr filter select
#' @importFrom rlang enquos !!!
#'
#' @export
#'


padrino_filter <- function(db, ...) {

  conditions <- rlang::enquos(...)

  output <- lapply(seq_len(length(db)),
                   function(x){
                     dplyr::filter(db[[x]], !!! conditions)
    }
  )

  return(output)

}
