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

  # hierarchical vars/exprs - >
  # define_kernel(has_hier_effs = ifelse(dim(this), TRUE, FALSE), levs = list(these))
  he_tab <- use_tabs[[11]]

  # uncertainty (currently not available)
  un_tab <- use_tabs[[12]]


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

    ik_tab <- .rm_dz_simple_ipm(ik_tab)

  }

  if(det_stoch == "det") kern_param <- NULL

  di_dd <- ifelse(md_tab$has_dd, "dd", "di")

  # If missing, assume "di" for now
  if(is.na(di_dd)) di_dd <- "di"


  # There shouldn't be any of these models in Padrino yet anyway, but they're
  # implemented in ipmr, so they could theoretically be digitized now

  has_age <- md_tab$has_age

  out <- ipmr::init_ipm(sim_gen    = sim_gen,
                        di_dd      = di_dd,
                        det_stoch  = det_stoch,
                        kern_param = kern_param,
                        has_age    = has_age)

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
  if(nrow(es_tab) > 0 && FALSE) {
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
#' @importFrom mvtnorm rmvnorm dmvnorm
#
# Generates call to r*pr_fun(1, other_args). Used to sample parameter distributions
# and/or environmental variables in *_stoch_param models

.prep_ran_fun <- function(ran_calls, data_list, ev_tab) {

  out <- fun_bodies <- list()

  arg_env <- list2env(data_list)

  for(i in seq_along(ran_calls)) {

    fun_bodies[[i]] <- .ran_fun_body(ran_calls[i], ev_tab)

    temp_args       <- .args_from_txt(ran_calls[i])

    fun_def_args    <- rlang::env_get_list(arg_env,
                                           nms = temp_args,
                                           default = NULL)


    out[[i]] <- rlang::new_function(
      args = fun_def_args,
      body = fun_bodies[[i]]
    )

  }

  return(out)

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

.new_env_fun <- function(body, args, out_nms) {

  f <- rlang::new_function(
    args = args,
    body = body
  )

  out <- function(f, out_nms, args){

    .env_name_wrapper(f(args), out_nms)
  }

  return(list(use_fun = out,
              env_fun = f))

}

#' @noRd
# Helper that makes sure env_fun always returns a list with the right
# names

.env_name_wrapper <- function(fun, nms, fun_args) {

  stats::setNames(object = fun(fun_args), nm = nms)

}

#' @noRd
#' @importFrom truncdist rtrunc
# Generates an environment to sub various random number generators into an
# actual function call.


.make_ran_env <- function() {

  rans <- list(
    Norm      = "stats::rnorm",
    Lognorm   = "stats::rlnorm",
    F_dist    = "stats::rf",
    Gamma     = "stats::rgamma",
    T_dist    = "stats::rt",
    Beta      = "stats::rbeta",
    Chi       = "stats::rchisq",
    Cauchy    = "stats::rcauchy",
    Expo      = "stats::rexp",
    Binom     = "stats::rbinom",
    Bernoulli = "stats::rbinom",
    Geom      = "stats::rgeom",
    Hgeom     = "stats::rhyper",
    Multinom  = "stats::rmultinom",
    Negbin    = "stats::rnbinom",
    Pois      = "stats::rpois",
    Weib      = "stats::rweibull",
    MVN       = "mvtnorm::rmvnorm",

    # Truncated distributions - mostly for define_env_state

    TNorm      = c("truncdist::rtrunc", "mean", "sd"),
    TLognorm   = c("truncdist::rtrunc", "meanlog", "sdlog"),
    TF_dist    = c("truncdist::rtrunc", "df1", "df2"),
    TGamma     = c("truncdist::rtrunc", "shape", "rate", "scale"),
    TT_dist    = c("truncdist::rtrunc", "df", "ncp"),
    TBeta      = c("truncdist::rtrunc", "shape1", "shape2", "ncp"),
    TChi       = c("truncdist::rtrunc", "df", "ncp"),
    TCauchy    = c("truncdist::rtrunc", "location", "scale"),
    TExpo      = c("truncdist::rtrunc", "rate"),
    TBinom     = c("truncdist::rtrunc", "size", "prob"),
    Ternoulli  = c("truncdist::rtrunc", "size", "prob"),
    TGeom      = c("truncdist::rtrunc", "prob"),
    THgeom     = c("truncdist::rtrunc", "m", "n", "k"),
    TMultinom  = c("truncdist::rtrunc", "size", "prob"),
    TNegbin    = c("truncdist::rtrunc", "size", "prob", "mu"),
    TPois      = c("truncdist::rtrunc", "lambda"),
    TWeib      = c("truncdist::rtrunc", "shape", "scale"),
    TMVN       = c("truncdist::rtrunc", "mean", "sigma"),
    sample     = c("stats::runif", "min", "max")
  )

  ran_env <- list2env(as.list(rans))

  return(ran_env)

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
# Appends hier_effs suffixes to pop_state vars in iteration_procedure calls when
# the model is deterministic. This needs to happen because otherwise, ipmr::make_ipm
# will not be able to generate deterministic simulations for each level of the grouping
# variable, which is probably what the user wants if they select a model with
# hier_effs and specify the deterministic format.

.append_pop_state_suffixes <- function(textprs, ps_tab, he_tab, det_stoch) {

  # if it's a stochastic model, then the suffix-less format is correct. If
  # there are no hier_effs, then there are no suffixes to append. In either or
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

.rm_dz_simple_ipm <- function(ik_tab) {

  for(i in seq_len(dim(ik_tab)[1])) {

    sv <- c(ik_tab$domain_start[i], ik_tab$domain_end[i])

    use_sv <- unique(sv)

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
