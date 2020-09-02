#' @title Subset a Padrino database object
#'
#' @param pdb A Padrino database object.
#' @param ipm_ids The \code{ipm_id}'s to subset the database to.
#'
#' @return A new Padrino database object containing only the models specified in
#' \code{ipm_ids}.
#'
#' @details Currently, the only variable to subset with is the \code{ipm_id}.
#' Eventually, subsetting based on other variables will be possible with syntax
#' similar to \code{subset}. At the moment, users will need to create a vector
#' of \code{ipm_id}s based on searching and then pass that to subset. See
#' Examples
#'
#' @examples
#'
#' \dontrun{
#'
#' data(pdb)
#'
#' poa_ind <- pdb$Metadata$ipm_id[pdb$Metadata$tax_family == "Poaceae"]
#'
#' poa_db  <- pdb_subset(pdb, ipm_ids = poa_ind)
#'
#' }
#'
#' @export

pdb_subset <- function(pdb, ipm_ids) {

  out <- lapply(pdb,
                function(x, ind) {
                  x[x$ipm_id %in% ind, ]
                },
                ind = ipm_ids)

  class(out) <- c("pdb", "list")

  return(out)

}
