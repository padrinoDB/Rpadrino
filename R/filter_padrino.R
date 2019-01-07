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
#' @importFrom rlang enquos !!!
#' @importFrom dplyr filter
#'
#' @export
#'


padrino_filter <- function(db, ...) {

  cond <- rlang::enquos(...)

  output <- lapply(db,
                   function(x){
                     dplyr::filter(x, !!! cond)
    }
  )

  return(output)

}
