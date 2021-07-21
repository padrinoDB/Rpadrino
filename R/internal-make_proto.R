#' @noRd
#' @importFrom ipmr init_ipm

# Functions to construct a single proto_ipm. This will take a complete pdb
# object and a single id, and construct a proto_ipm for it.

.make_proto <- function(db_tabs, id, det_stoch, kern_param, stop_on_failure) {

  # Subset to a single IPM

  use_tabs <- lapply(db_tabs, function(x, id) {

    x[x$ipm_id %in% id, ]

  },
  id = id)

  .check_proto_args(use_tabs, det_stoch, kern_param, stop_on_failure)


  # Split out individual tables
  md_tab <- use_tabs$Metadata

  # All state variables -> define_kernel(states = list(c(these)))
  sv_tab <- use_tabs$StateVariables

  # discrete state variables -> Not totally sure why these are needed, but
  # may be useful for... something?
  ds_tab <- use_tabs$DiscreteStates

  # continuous domains ->
  # define_domains(rlang::list2(!!nm := list(lower = these, upper = these)))
  cd_tab <- use_tabs$ContinuousDomains

  # integration rules -> define_impl(rlang::list2(!! nm := list(int_rule = these)))
  ir_tab <- use_tabs$IntegrationRules

  # pop trait distrib vectors/functions. I think these are all pretty much empty,
  # so will need to initialize w/ some random distributions.
  ps_tab <- use_tabs$StateVectors

  # ipm kernel exprs -> define_kernel(formula = !! these)
  ik_tab <- use_tabs$IpmKernels

  # vital rate exprs -> define_kernel(!!! these)
  vr_tab <- use_tabs$VitalRateExpr

  # paramter values -> define_kernel(data_list = as.list(these))
  pv_tab <- use_tabs$ParameterValues

  # environmental vars/exprs -> define_env_state(these)
  es_tab <- use_tabs$EnvironmentalVariables

  # parameter set  vars/exprs - >
  # define_kernel(uses_par_sets = ifelse(dim(this), TRUE, FALSE), levs = list(these))
  he_tab <- use_tabs$HierarchTable

  # uncertainty (currently not available)
  un_tab <- use_tabs$UncertaintyTable

  if(!is.na(md_tab$remark)) {

    message("'ipm_id' ", id, " has the following notes that require your attention:\n",
            .ipmr_strwrap(md_tab$remark, id))

  }

  # Get kernel IDs. Figure out if we're simple/general, and assign the model
  # class to the initial proto.

  kern_ids <- use_tabs$IpmKernels$kernel_id

  if(any(sv_tab$discrete) || nrow(sv_tab) > 1) {

    sim_gen <- "general"

  } else {

    # Added d_z's to all CC, CD, and DC kernels in Padrino at some point.
    # We don't need these for CC kernels in simple models because ipmr appends
    # those automatically. Keeping them would do a double integration, which we
    # don't want!

    sim_gen <- "simple"

    ik_tab <- .rm_dz_simple_ipm(ik_tab, cd_tab)

  }

  if(det_stoch == "det") {

    if(nrow(es_tab) > 0) {
      det_stoch <- "stoch"
      kern_param <- "param"

      message("'ipm_id' ", id," has resampled parameters, resetting 'det_stoch'",
              " to 'stoch' and \n'kern_param' to 'param'!\n",
              "Default number of iterations for 'pdb_make_ipm' will be 50. Modify ",
              "this behavior \nwith the 'addl_args' argument of 'pdb_make_ipm'.")

    } else {

      kern_param <- NULL

    }

  }

  di_dd <- ifelse(md_tab$has_dd, "dd", "di")

  # If missing, assume "di" for now
  if(is.na(di_dd)) di_dd <- "di"


  # There shouldn't be any of these models in Padrino yet anyway, but they're
  # implemented in ipmr, so they could theoretically be digitized now

  uses_age <- md_tab$has_age

  out <- ipmr::init_ipm(sim_gen    = sim_gen,
                        di_dd      = di_dd,
                        det_stoch  = det_stoch,
                        kern_param = kern_param,
                        uses_age   = uses_age)

  for(i in seq_along(kern_ids)) {

    out <- .define_kernel(out,
                          kern_ids[i],
                          md_tab,
                          sv_tab,
                          ds_tab,
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

  out <- .define_impl(
    out,
    ir_tab,
    ik_tab
  )

  out <- .define_domains(
    out,
    cd_tab,
    ps_tab
  )

  out <- .define_pop_state(
    out,
    det_stoch,
    ps_tab,
    he_tab
  )

  # Turning off stoch_param for now
  if(nrow(es_tab) > 0) {
    out <- .define_env_state(
      out,
      es_tab
    )
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
#' @importFrom rlang new_function pairlist2 :=

.prep_sample_fun <- function(data_list, out_nm, call_info) {

  # Get the string form of the function call. Since sample is a special case,
  # this is always just going to be "sample"

  call_str <- call_info[1]

  # Sample size should always be 1, and x corresponds to the first argument
  # to sample. This function gets called every model iteration, so no need
  # to pre-generate a vector of selected values.

  args     <- list(x    = eval(parse(text = data_list)),
                   size = 1)

  body     <- rlang::call2(rlang::parse_expr(call_str), !!! args)

  # fun_1 is the actual expression we want to evaluate. it generates a value
  # from the expression stored in the database.

  fun_1    <- rlang::new_function(args = rlang::pairlist2(... = ),
                                  body = body)

  # Next, we need to wrap this expression so that the returned value is in a list
  # and its name corresponds to that which is used in the VitalRateExpr

  out_fun  <- rlang::new_function(args = rlang::pairlist2(nm = out_nm,
                                                          f = fun_1),
                                  body = quote(rlang::list2(!!nm := f())))

  return(out_fun)

}

#' @noRd
.new_env_fun <- function(env_expr, out_nm, temp_dl, expr_type) {


    switch(expr_type,
           "Substituted" = .make_ran_fun(env_expr, out_nm, temp_dl),
           "Evaluated"   = .make_eval_fun(env_expr, out_nm))
}

#' @noRd
# Generates an environment to sub various random number generators into an
# actual function call.

.make_ran_fun <- function(ran_expr, out_nm, data_list) {

  # Create a symbol that will evaluate in the .args_list() environment,
  # and then retrieve the set of arguments that Padrino functions can accept

  pdb_call  <- rlang::parse_expr(rlang::call_name(ran_expr))
  call_info <- rlang::eval_tidy(pdb_call, env = .args_list())

  # This is the actual R function name, as translated by the args_list environment
  ran_call  <- rlang::parse_expr(call_info[1])

  # These are the formal arguments provided to the R function
  arg_nms   <- call_info[-1]

  if(!rlang::is_empty(data_list)){
    fml_args <- c(n = 1, rlang::syms(names(data_list)))
  } else {
    fml_args <- c(n = 1, rlang::syms(.args_from_txt(rlang::expr_text(ran_expr))))
  }
  names(fml_args) <- arg_nms[seq_along(fml_args)]

  .new_ran_call(ran_call, fml_args, out_nm, vals = data_list)


}

#' @noRd
#' @importFrom truncdist rtrunc
#
# Creates a function given a set of formal argument names, symbols representing
# variables in padrino, and a name to set for the output (which is used in the
# vital rate/kernel expressions)

.new_ran_call <- function(ran_call,
                          fml_args,
                          out_nm,
                          vals) {

  force(ran_call)
  force(fml_args)

  temp <- rlang::call2(ran_call,
                       !!! fml_args)

  .make_eval_fun(temp, out_nm)

}

#' @noRd

.make_eval_fun <- function(env_expr, out_nm) {

  rlang::list2(!!out_nm := rlang::quo(!! env_expr))

}

#' @noRd
#' @importFrom mvtnorm rmvnorm dmvnorm

.args_list <- function() {

  args <- list(
    Norm      = c("stats::rnorm","n", "mean", "sd"),
    Lognorm   = c("stats::rlnorm","n", "meanlog", "sdlog"),
    F_dist    = c("stats::rf","n", "df1", "df2"),
    Gamma     = c("stats::rgamma","n", "shape", "rate", "scale"),
    T_dist    = c("stats::rt","n", "df", "ncp"),
    Beta      = c("stats::rbeta","n", "shape1", "shape2", "ncp"),
    Chi       = c("stats::rchisq","n", "df", "ncp"),
    Cauchy    = c("stats::rcauchy","n", "location", "scale"),
    Expo      = c("stats::rexp", "n","rate"),
    Binom     = c("stats::rbinom", "n","size", "prob"),
    Bernoulli = c("stats::rbinom", "n","size", "prob"),
    Geom      = c("stats::rgeom", "n","prob"),
    Hgeom     = c("stats::rhyper", "nn","m", "n", "k"),
    Multinom  = c("stats::rmultinom","n", "size", "prob"),
    Negbin    = c("stats::rnbinom", "n","size", "prob", "mu"),
    Pois      = c("stats::rpois", "n","lambda"),
    Weib      = c("stats::rweibull", "n","shape", "scale"),
    MVN       = c("mvtnorm::rmvnorm", "n","mean", "sigma"),
    Unif      = c("stats::runif", "n","min", "max"),
    sample    = c("sample", "size", "x"),

    # Truncated distributions - mostly for define_env_state

    TNorm      = c("truncdist::rtrunc", "n", "mean", "sd", "a", "b"),
    TLognorm   = c("truncdist::rtrunc", "n", "meanlog", "sdlog", "a", "b"),
    TF_dist    = c("truncdist::rtrunc", "n", "df1", "df2", "a", "b"),
    TGamma     = c("truncdist::rtrunc", "n", "shape", "rate", "scale", "a", "b"),
    TT_dist    = c("truncdist::rtrunc", "n", "df", "ncp", "a", "b"),
    TBeta      = c("truncdist::rtrunc", "n", "shape1", "shape2", "ncp", "a", "b"),
    TChi       = c("truncdist::rtrunc", "n", "df", "ncp", "a", "b"),
    TCauchy    = c("truncdist::rtrunc", "n", "location", "scale", "a", "b"),
    TExpo      = c("truncdist::rtrunc", "n", "rate", "a", "b"),
    TBinom     = c("truncdist::rtrunc", "n", "size", "prob", "a", "b"),
    Ternoulli  = c("truncdist::rtrunc", "n", "size", "prob", "a", "b"),
    TGeom      = c("truncdist::rtrunc", "n", "prob", "a", "b"),
    THgeom     = c("truncdist::rtrunc", "n", "m", "n", "k", "a", "b"),
    TMultinom  = c("truncdist::rtrunc", "n", "size", "prob", "a", "b"),
    TNegbin    = c("truncdist::rtrunc", "n", "size", "prob", "mu", "a", "b"),
    TPois      = c("truncdist::rtrunc", "n", "lambda", "a", "b"),
    TWeib      = c("truncdist::rtrunc", "n", "shape", "scale", "a", "b"),
    TMVN       = c("truncdist::rtrunc", "n", "mean", "sigma", "a", "b")
  )

  math_ops <- as.list(.math_ops()) %>%
    setNames(.math_ops())

  args <- c(args, math_ops)

  list2env(args)
}

#' @noRd

.math_ops <- function() {
  c("+", "-", "*", "/")
}

#' @noRd
# Generates an environment to sub various PDFs into an actual function call.

.make_pdf_env <- function() {

  pdfs <- list(
    Norm      = "stats::dnorm",
    Lognorm   = "stats::dlnorm",
    F_dist    = "stats::df",
    Gamma     = "stats::dgamma",
    T_dist    = "stats::dt",
    Beta      = "stats::dbeta",
    Chi       = "stats::dchisq",
    Cauchy    = "stats::dcauchy",
    Expo      = "stats::dexp",
    Binom     = "stats::dbinom",
    Bernoulli = "stats::dbinom",
    Geom      = "stats::dgeom",
    Hgeom     = "stats::dhyper",
    Multinom  = "stats::dmultinom",
    Negbin    = "stats::dnbinom",
    Pois      = "stats::dpois",
    Unif      = "stats::dunif",
    Weib      = "stats::dweibull",
    MVN       = "mvtnorm::dmvnorm"

  )

  pdf_env <- list2env(as.list(pdfs))

  return(pdf_env)

}

#' @noRd
#' @importFrom rlang call_name call_args parse_expr call2
# dens_call: Must be a quosure
# sv_2: The name of the state variable without _1 or _2 appended. This is handled
# internally.
#

.prep_dens_fun <- function(dens_call, sv_2) {

  fun_call <- rlang::call_name(dens_call) %>%
    rlang::parse_expr()

  current_args <- rlang::call_args(dens_call)
  sub_call <- eval(fun_call, envir = .make_pdf_env())


  if("first_arg" %in% names(current_args) && isTRUE(current_args$first_arg)) {

    # call_text <- rlang::expr_text(dens_call)
    # d_sv      <- rlang::expr_text(rlang::call_args(dens_call)[[1]])

    current_args$first_arg <- NULL

    out <- paste(sub_call, "(",
                 paste(current_args, collapse = ", "),
                 ")", sep = "")

  } else {

    d_sv <- paste(sv_2, "_2", sep = "")
    out <- paste(sub_call, "(", d_sv, ', ',
                 paste(current_args, collapse = ", "),
                 ")", sep = "")

  }

  return(out)

}

.prep_sub_fun <- function(dens_funs, states) {

  if(length(dens_funs) == 0) return(NULL)

  states <- unique(unlist(states))

  if(length(states) > 1) warning("found multiple states for single kernel",
                                 ". check results")

  if(length(states) == 0) stop("no state info found for single kernel.",
                               " check Padrino.")

  lhs_rhs        <- vapply(dens_funs,
                           function(x) unlist(strsplit(x, ' = ')) %>% trimws(),
                           character(2L))

  if(ncol(lhs_rhs) > 1) {

    rhs            <- rlang::parse_exprs(lhs_rhs[2, ]) %>%
      unlist()

  } else {

    rhs <- list(rlang::parse_expr(lhs_rhs[2, ]))

  }

  dens_fun_exprs <- vapply(
    rlang::quos(!!! rhs),
    .prep_dens_fun,
    character(1L),
    sv_2 = states
  )

  out <- lapply(dens_fun_exprs, rlang::parse_expr)

  names(out) <- lhs_rhs[1, ]

  return(out)

}


#' @noRd
# Appends par_sets suffixes to pop_state vars in iteration_procedure calls when
# the model is deterministic. This needs to happen because otherwise, ipmr::make_ipm
# will not be able to generate deterministic simulations for each level of the grouping
# variable, which is probably what the user wants if they select a model with
# par_sets and specify the deterministic format.

.append_pop_state_suffixes <- function(textprs, ps_tab, he_tab, det_stoch) {

  # if it's a stochastic model, then the suffix-less format is correct. If
  # there are no par_sets, then there are no suffixes to append. In either or
  # both cases, return early and skip the rest.

  if(det_stoch == "stoch" || dim(he_tab)[1] == 0) return(textprs)

  # Now, generate the base suffix. This is just the combination of the different
  # grouping variable names collapsed into a single string. Next, append those
  # to the various population states.

  base_suff   <- paste(he_tab$vr_expr_name, collapse = "_")
  for_replace <- paste(ps_tab$expression, base_suff, sep = "_")

  # We are now ready to substitute. Going with fuzzy matching gsub for now
  # but this could come back and bite me if it turns out some var names match
  # each other somehow (e.g. something like n_b -> n_b_yr and n_b1 -> n_b_yr1)

  for(i in seq_along(for_replace)) {

    textprs <- gsub(ps_tab$expression[i], for_replace[i], textprs)

  }

  return(textprs)

}

#' @noRd

.rm_dz_simple_ipm <- function(ik_tab, cd_tab) {

  for(i in seq_len(dim(ik_tab)[1])) {

    sv <- c(ik_tab$domain_start[i], ik_tab$domain_end[i])

    use_sv <- unique(sv) %>%
      .[. %in% cd_tab$state_variable]

    if(all(is.na(use_sv))) next

    use_sv <- use_sv[!is.na(use_sv)]

    d_z <- paste(" \\* d_", use_sv, sep = "")

    ik_tab$formula[i] <- gsub(d_z, "", ik_tab$formula[i])

  }

  return(ik_tab)

}

#' @noRd

.can_be_number <- function(x) {

  out <- suppressWarnings(is.na(as.numeric(x)))

  return(!out)

}

.quote_marks <- function(x) {
  paste("'", x, "'", sep = "")
}

.ipmr_strwrap <- function(x, ipm_id) {

  strwrap(
    paste(ipm_id, ": ",
          .quote_marks(x),
          sep = ""),
    prefix = "\n",
    initial = "",
    width = 85
  )

}
