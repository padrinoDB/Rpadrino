#' @noRd
#' @importFrom ipmr init_ipm
# Functions to construct a single proto_ipm. This will take a complete pdb
# object and a single id, and construct a proto_ipm for it.

.make_proto <- function(db_tabs, id, det_stoch, kern_param) {

  # Subset to a single IPM

  use_tabs <- lapply(db_tabs, function(x, id) {

    x[x$ipm_id %in% id, ]

  },
  id = id)

  # Split out individual tables

  # All state variables -> define_kernel(states = list(c(these)))
  sv_tab <- use_tabs[[2]]

  # discrete state variables -> define_kernel(family = CC||CD||DC, formula = these)
  ds_tab <- use_tabs[[3]]

  # discrete transitions (e.g. markov mat) ->
  # define_k(n_these_t_1 = dt_tab$value) (I actually don't think this is necessary)
  # and perhaps this table isn't even necessary. This will be captured
  # by the IPM_FULL designated kernels (I think, need to review that format).
  dt_tab <- use_tabs[[4]]

  # continuous domains ->
  # define_domains(rlang::list2(!!nm := list(lower = these, upper = these)))
  cd_tab <- use_tabs[[5]]

  # integration rules -> define_impl(rlang::list2(!! nm := list(int_rule = these)))
  ir_tab <- use_tabs[[6]]

  # pop trait distrib vectors/functions. I think these are all pretty much empty,
  # so will need to initialize w/ some random distributions.
  ps_tab <- use_tabs[[7]]

  # ipm kernel exprs -> define_kernel(formula = !! these)
  ik_tab <- use_tabs[[8]]

  # vital rate exprs -> define_kernel(!!! these)
  vr_tab <- use_tabs[[9]]

  # paramter values -> define_kernel(data_list = as.list(these))
  pv_tab <- use_tabs[[10]]

  # environmental vars/exprs -> define_env_state(these)
  es_tab <- use_tabs[[11]]

  # hierarchical vars/exprs - >
  # define_kernel(has_hier_effs = ifelse(dim(this), TRUE, FALSE), levs = list(these))
  he_tab <- use_tabs[[12]]

  # uncertainty (currently not available)
  un_tab <- use_tabs[[13]]


  # Get kernel IDs. Figure out if we're simple/general, and assign the model
  # class to the initial proto.

  kern_ids <- use_tabs$IpmKernels$kernel_id

  sim_gen  <- ifelse(dim(dt_tab)[1] > 0, "general", "simple")

  if(det_stoch == "det") kern_param <- NULL

  model_cls <- paste(c(sim_gen, "di", det_stoch, kern_param), collapse = "_")

  # For now, no age size IPMs in Padrino anyway. Will alter this accordingly
  # when that variable is created.

  has_age <- FALSE

  out <- ipmr::init_ipm(model_class = model_cls, has_age = has_age)

  for(i in seq_along(kern_ids)) {

    out <- .define_single_kernel(out,
                                 kern_ids[i],
                                 sv_tab,
                                 ds_tab,
                                 dt_tab,
                                 cd_tab,
                                 ir_tab,
                                 ps_tab,
                                 ik_tab,
                                 vr_tab,
                                 pv_tab,
                                 es_tab,
                                 he_tab,
                                 un_tab)


  }


  return(out)

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
    "Lognorm",
    "F",
    "Gamma",
    "t",
    "Beta",
    "ChiSq",
    "Cauchy"
  )

  pdf_list <- setNames(as.list(paste("d", tolower(pdfs), sep = "")), pdfs)
  pdf_list$Lognorm <- 'dlnorm'

  pdf_env <- list2env(as.list(pdf_list))

  return(pdf_env)

}

#' @noRd
#' @importFrom rlang call_name call_args parse_expr
# TO-FIX: This breaks unless the dens fun is the ONLY call in the expression.
# Must get all calls and *only* sub the density fun, then re-construct the complete
# call. Alternatively - just create new rows for each dens fun and then
# use a variable name in the nested expression

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
