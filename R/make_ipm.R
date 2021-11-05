#' @title Generate IPMs from Padrino objects
#'
#' @description This function generates complete IPMs from objects created with
#' \code{pdb_make_proto_ipm}.
#'
#' @param proto_ipm_list Output from \code{pdb_make_proto_ipm}.
#' @param addl_args A named list of additional arguments to pass to
#' \code{\link[ipmr]{make_ipm}}.
#'
#' @return A list of IPMs.
#'
#' @details The format of \code{addl_args} should be a nested list. The names
#' of the outermost level should correspond to the \code{ipm_id} that the arguments
#' apply to. Each entry of the outermost level should itself then be a named list
#' where the names correspond to arguments to \code{\link[ipmr]{make_ipm}}, and the
#' values are the values for each argument. See examples.
#'
#' @examples
#' \dontrun{
#'
#' data("pdb_ex")
#'
#' proto <- pdb_make_proto_ipm(pdb_ex, ipm_id = "aaa341", det_stoch = "det")
#'
#' ipm   <- pdb_make_ipm(proto)
#'
#' proto <- pdb_make_proto_ipm(pdb_ex,
#'                             ipm_id     = "aaaa55",
#'                             det_stoch  = "stoch",
#'                             kern_param = "kern")
#'
#' args  <-list(
#'
#'  # The names in the outermost list should be ipm_id's
#'
#'  aaaa55 = list(
#'
#'    # The names in the inner list should be arguments to make_ipm()
#'
#'    report_progress = TRUE,
#'    iterate         = TRUE,
#'    iterations      = 100,
#'    kernel_seq      = sample(2004:2014, 100, replace = TRUE)
#'  )
#')
#'
#' ipm   <- pdb_make_ipm(proto, addl_args = args)
#'}
#'
#' @export

pdb_make_ipm <- function(proto_ipm_list, addl_args = list()) {

  .check_make_ipm_args(proto_ipm_list, addl_args)

  made_calls <- .make_ipm_calls(proto_ipm_list, addl_args)

  # generates the actual ipm

  out        <- lapply(made_calls, eval)

  class(out) <- c("pdb_ipm", "list")

  return(out)

}


