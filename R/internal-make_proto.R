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
  md_tab <- use_tabs[[1]]

  # All state variables -> define_kernel(states = list(c(these)))
  sv_tab <- use_tabs[[2]]

  # discrete state variables -> Not totally sure why these are needed, but
  # may be useful for... something?
  ds_tab <- use_tabs[[3]]

  # continuous domains ->
  # define_domains(rlang::list2(!!nm := list(lower = these, upper = these)))
  cd_tab <- use_tabs[[4]]

  # integration rules -> define_impl(rlang::list2(!! nm := list(int_rule = these)))
  ir_tab <- use_tabs[[5]]

  # pop trait distrib vectors/functions. I think these are all pretty much empty,
  # so will need to initialize w/ some random distributions.
  ps_tab <- use_tabs[[6]]

  # ipm kernel exprs -> define_kernel(formula = !! these)
  ik_tab <- use_tabs[[7]]

  # vital rate exprs -> define_kernel(!!! these)
  vr_tab <- use_tabs[[8]]

  # paramter values -> define_kernel(data_list = as.list(these))
  pv_tab <- use_tabs[[9]]

  # environmental vars/exprs -> define_env_state(these)
  es_tab <- use_tabs[[10]]

  # parameter set  vars/exprs - >
  # define_kernel(uses_par_sets = ifelse(dim(this), TRUE, FALSE), levs = list(these))
  he_tab <- use_tabs[[11]]

  # uncertainty (currently not available)
  un_tab <- use_tabs[[12]]

  if(!is.na(md_tab$remark)) {

    message("'ipm_id' ", id, " has the following notes that require your attention:\n",
            .quote_marks(md_tab$remark))

  }

  # Get kernel IDs. Figure out if we're simple/general, and assign the model
  # class to the initial proto.

  kern_ids <- use_tabs$IpmKernels$kernel_id

  if(any(sv_tab$discrete)) {

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
#' @importFrom mvtnorm rmvnorm dmvnorm
#
# Generates call to r*pr_fun(1, other_args). Used to sample parameter distributions
# and/or environmental variables in *_stoch_param models

.prep_ran_fun <- function(data_list, out_nm, call_info) {

  arg_nms <- formals(call_info[[1]]) %>%
    names(.)[2:length(.)]
  args     <- c(1, data_list)
  n_nm     <- ifelse(call_str == "Hgeom", "nn", "n")

  names(args) <- c(n_nm, arg_nms)

  # fun_1 is the actual expression we want to evaluate. it generates a value
  # from the expression stored in the database.

  fun_1    <- rlang::new_function(args = rlang::pairlist2(... = ),
                                  body = call_info)

  # Next, we need to wrap this expression so that the returned value is in a list
  # and its name corresponds to that which is used in the VitalRateExpr

  out_fun  <- rlang::new_function(args = rlang::pairlist2(nm = out_nm,
                                                          f = fun_1),
                                  body = quote(rlang::list2(!!nm := f())))

  return(out_fun)

}

.ran_fun_body <- function(ran_call, ev_tab) {

  # Get PADRINO version of the r* function, and the name that it's supposed
  # to create.

  pdb_name <- ev_tab$vr_expr_name[ev_tab$env_function == ran_call]

  ran_call <- rlang::parse_expr(ran_call)

  fun_call <- rlang::call_name(ran_call)

  # Pull out the arguments from PADRINO

  current_args <- rlang::call_args(ran_call)

  if(fun_call %in% .math_ops()) {

    out_fun      <- fun_call

  } else if(fun_call %in% names(.make_ran_env())) {

    out_fun      <- eval(rlang::sym(fun_call),
                         envir = .make_ran_env())

    # We don't want to set the name of first argument to out_fun because
    # that comes later (thanks rhyper!),
    # but we do want to grab the others formal names. This strategy below assumes
    # they are entered in PADRINO in the same order that they would appear
    # in the formal argument list for the R function. Methinks this a dubious
    # proposition, but will update accordingly when things break

    formal_args         <- formals(eval(rlang::parse_expr(out_fun)))
    names(current_args) <- names(formal_args)[2:(length(current_args) + 1)]

  }

  # Most of the environmental functions are truncated distributions
  # We need to format these correctly:
  # rtrunc(spec = out_fun[-"r" predix],
  #        n = 1,
  #        a = env_range[1],
  #        b = env_range[2],
  #        !!! current_args)
  # eval(fun_call, .make_ran_env()) returns a vector w length >= 2 for rtrunc
  # calls with the names of the possible !!! current_args.

  if(out_fun[1] == "rtrunc") {

    names(current_args) <- out_fun[2:length(out_fun)]

    out_fun <- out_fun[1]

    # Get the spec name. This removes the "T" prefix, then
    # gets the distribution abbreviation from ran_env, then removes
    # the "r" prefix to create the correct format of "spec".

    spec    <- rlang::call_name(ran_call) %>%
      substr(x = ., start = 2, stop = nchar(.)) %>%
      rlang::parse_expr() %>%
      eval(., envir = .make_ran_env()) %>%
      substr(x = ., start = 2, stop = nchar(.))

    current_args <- c(current_args, list(spec = spec))

    # Finally, we need to get the truncation interval. This is stored in the same
    # line as the sampling function in env_range, with the format: L;U (i.e.
    # a semi-colon split).

    t_interval <- ev_tab$env_range[ev_tab$env_function == rlang::expr_text(ran_call)]

    a_b        <- strsplit(t_interval, ';') %>%
      lapply(trimws) %>%
      unlist() %>%
      as.numeric()

    a          <- a_b[1]
    b          <- a_b[2]

    current_args <- c(current_args,
                      list(a = a,
                           b = b))

  }

  # For some reason, the rhyper function uses "nn" to specify the
  # number of samples from the distribution. This makes sure that name
  # is set correctly.

  if(fun_call == "Hgeom" || fun_call == "THgeom") {

    body_args    <- c(list(nn = 1),
                      current_args)

    out_fun <- rlang::parse_expr(out_fun)

  } else if(fun_call %in% names(.make_ran_env())) {

    body_args    <- c(list(n = 1),
                      current_args)

    out_fun <- rlang::parse_expr(out_fun)

  } else {

    # Math_opserations. These almost universally use the names
    # x,y for their arguments.

    body_args <- current_args
    names(body_args) <- c("x", "y")

    out_fun   <- rlang::sym(out_fun)

  }

  fun_temp     <- call2(eval(out_fun),
                      !!! body_args)

  fun_body     <- .new_env_fun(fun_temp, body_args, pdb_nm)

  return(fun_body)

}

.math_ops <- function() {

  c("+", "-", "*", "/")

}

#' @noRd

.ast_from_txt <- function(txt) {

  exprr <- rlang::parse_expr(txt)

  lapply(as.list(exprr),
         function(x) {
           ipmr:::.switch_expr(typeof(x),
                               "constant" = as.character(x),
                               "symbol"   = as.character(x),
                               "call" = {
                                 .ast_from_txt(as.character(x))
                               })
         })


}

#' @noRd

.group_ran_calls <- function(call_tab, call_asts) {

  any_nested <- matrix(nrow = length(call_asts),
                       ncol = length(call_asts),
                       dimnames = list(names(call_asts),
                                       names(call_asts)))

  for(i in seq_along(call_asts)) {

    any_nested[i, ] <- purrr::map_lgl(
      .x = call_asts,
      .f = function(.x, targ) {
        any(targ %in% .x)
      },
      targ = names(call_asts)[i]
    )

  }

  if(all(!any_nested)) {

    call_tab$model_group <- letters[1:nrow(call_tab)]

    return(call_tab)

  }

  children_expr_nms <- .nest_all_env_exprs(any_nested, call_asts)

}


#'@ noRd

.nest_all_env_exprs <- function(lgl_mat, asts) {

  out <- vector("list", length(asts)) %>%
    setNames(names(asts))

  # first pass
  for(i in seq_len(nrow(lgl_mat))) {

    out[[i]] <- dimnames(lgl_mat)[[1]][lgl_mat[, i]]

  }

  out <- Filter(function(x) !rlang::is_empty(x), out)

  stp_lgl <- TRUE

  while(stp_lgl) {

    nms <- names(out)

    for(i in seq_along(out)) {

      if(any(nms %in% out[[i]])) {

        add      <- which(nms %in% out[[i]])

        out[[i]] <- c(out[[i]], out[[add]])
      }

    }

    stp_lgl <- any(
      vapply(
        out, rlang::is_empty,
        logical(1L)
      )
    )

    out <- Filter(function(x) !rlang::is_empty(x), out)

  }

  out <- purrr::map2(
    .x = out,
    .y = names(out),
    ~ c(.y, .x)
  )

  out <- .drop_duplicated_env_index(out)

  return(out)

}

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

  fml_args <- c(n = 1, rlang::syms(names(data_list)))

  names(fml_args) <- arg_nms

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

  d_sv <- paste(sv_2, "_2", sep = "") %>%
    rlang::parse_expr()

  sub_call <- eval(fun_call, envir = .make_pdf_env())

  out <- paste(sub_call, "(", d_sv, ', ',
               paste(current_args, collapse = ", "),
               ")", sep = "")

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
