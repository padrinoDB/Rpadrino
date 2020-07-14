#' @noRd
# Functions to construct a single proto_ipm. This will take a complete pdb
# object and a single id, and construct a proto_ipm for it.

.make_proto <- function(db_tabs, id) {

  # Subset to a single IPM

  use_tabs <- lapply(db_tabs, function(x, id) {

    x[x$ipm_id %in% id, ]

  },
  id = id)

  # Split out individual tables

  sv_tab <- use_tabs[[2]]  # All state variables
  ds_tab <- use_tabs[[3]]  # discrete state variables
  dt_tab <- use_tabs[[4]]  # discrete transitions (e.g. markov mat)
  cd_tab <- use_tabs[[5]]  # continuous domains
  ir_tab <- use_tabs[[6]]  # integration rules
  ps_tab <- use_tabs[[7]]  # pop trait distrib vectors/functions
  ik_tab <- use_tabs[[8]]  # ipm kernel exprs
  vr_tab <- use_tabs[[9]]  # vital rate exprs
  pv_tab <- use_tabs[[10]] # paramter values
  es_tab <- use_tabs[[11]] # environmental vars/exprs
  he_tab <- use_tabs[[12]] # hierarchical vars/exprs
  un_tab <- use_tabs[[13]] # uncertainty (currently not available)

  kern_ids <- use_tabs$IpmKernels$kernel_id

  for(i in seq_along(kern_ids)) {

    # First, get state variable info. Not sure we really need
    # the discrete state var names for ipmr, so dropping those for now.

    states <- sv_tab$state_variable[sv_tab$discrete == 'f']

    # Next, drop self assignment formulae. This won't be necessary after the
    # the database is corrected internally, but needs to happen for the
    # current version

    vr_tab <- vr_tab[! vapply(vr_tab$formula,
                              FUN = function(x) .is_the_same(x),
                              logical(1L)), ]





  }


}

#' @noRd
# removes bracket notation to create an expression that can actually be evaluated

.prep_exprs <- function(form) {

  out <- .rm_brackets(form) %>%
    unlist() %>%
    rlang::parse_expr()

  return(out)

}

#' @noRd
#' @importFrom stats setNames
# Generates an environment to sub various PDFs into an actual function call.

.make_pdf_env <- function() {

  pdfs <- c(
    "Norm",
    "F",
    "Gamma",
    "t",
    "Beta",
    "ChiSq",
    "Cauchy"
  )

  pdf_list <- setNames(as.list(paste("d", tolower(pdfs), sep = "")), pdfs)

  pdf_env <- list2env(as.list(pdf_list))

  return(pdf_env)

}

#' @noRd
#' @importFrom rlang call_name call_args parse_expr

.prep_dens_fun <- function(dens_call, sv_2) {

  fun_call <- rlang::call_name(dens_call) %>%
    rlang::parse_expr()

  current_args <- rlang::call_args(dens_call)

  d_sv <- paste(sv_2, "_2", sep = "") %>%
    rlang::parse_expr()

  sub_call <- eval(fun_call, envir = .make_pdf_env())

  out <- paste(sub_call, "(", d_sv, ',',
               paste(current_args, collapse = ", "),
               ")", sep = "")

  return(out)

}
