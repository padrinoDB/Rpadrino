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

  # discrete transitions (e.g. markov mat) ->
  # define_k(n_these_t_1 = dt_tab$value) (I actually don't think this is necessary)
  # and perhaps this table isn't even necessary. This will be captured
  # by the IPM_FULL designated kernels (I think, need to review that format).
  # dt_tab <- use_tabs[[4]]

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

  if(dim(ds_tab)[1] > 0) {

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

  model_cls <- paste(c(sim_gen, "di", det_stoch, kern_param), collapse = "_")

  # For now, no age size IPMs in Padrino anyway. Will alter this accordingly
  # when that variable is created.

  has_age <- FALSE

  out <- ipmr::init_ipm(model_class = model_cls, has_age = has_age)

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

  # If there is a "K" kernel, or it's a potentially stochastic model, we
  # need to call ipmr::define_k(). We need to re-arrange a bunch of stuff
  # though, so this is wrapped in .define_k

  k_fams <- c("IPM", "iteration_procedure")


  if(any(k_fams %in% ik_tab$model_family)) {

    kern_ids <- ik_tab$kernel_id[ik_tab$model_family %in% k_fams]

    out <- .define_k(out,
                     kern_ids,
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
                     un_tab,
                     det_stoch)

  }

  out <- .define_impl(
    out,
    ir_tab,
    ik_tab
  )

  out <- .define_domains(
    out,
    cd_tab,
    ps_tab,
    ik_tab
  )

  out <- .define_pop_state(
    out,
    det_stoch,
    ps_tab,
    he_tab
  )

  if(!is.null(kern_param) && kern_param == "param") {
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
    "Cauchy",
    "Expo"
  )

  pdf_list <- setNames(as.list(paste("d", tolower(pdfs), sep = "")), pdfs)
  pdf_list$Lognorm <- 'dlnorm'
  pdf_list$Expo    <- "dexp"

  pdf_env <- list2env(as.list(pdf_list))

  return(pdf_env)

}

#' @noRd
#' @importFrom rlang call_name call_args parse_expr call2
#
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
                           function(x) unlist(strsplit(x, '=')) %>% trimws(),
                           character(2L))

  rhs            <- rlang::parse_expr(lhs_rhs[2, ])

  dens_fun_exprs <- vapply(
    rlang::quos(!!rhs),
    .prep_dens_fun,
    character(1L),
    sv_2 = states
  )

  out <- lapply(dens_fun_exprs, rlang::parse_expr)

  names(out) <- lhs_rhs[1, ]

  return(out)

}

#' @noRd
# Splits semi-colon separated model_iteration model families for the ...
# in define_k

.prep_k_dots <- function(ik_tab, ps_tab, he_tab, det_stoch) {

  # If no iteration procedure specified, then we can just build the iteration
  # kernel(s) with a single expression. This will be defined using
  # model_family == "IPM"

  if(! "iteration_procedure" %in% ik_tab$model_family) {

    textpr    <- ik_tab$formula[ik_tab$model_family == "IPM"]

    text_list <- strsplit(textpr, "=")

    nm        <- trimws(text_list[[1]][1])
    textpr    <- trimws(text_list[[1]][2])

    out       <- rlang::list2(!! nm := rlang::parse_expr(textpr))

    return(out)
  }

  textprs <- ik_tab$formula[ik_tab$model_family == "iteration_procedure"]

  textprs <- .append_pop_state_suffixes(textprs, ps_tab, he_tab, det_stoch)

  temp    <- strsplit(textprs, ";") %>%
    lapply(FUN = function(x) {

      strsplit(x, "=")

    }) %>%
    .flatten_to_depth(1L)

  out        <- lapply(temp, function(x) rlang::parse_expr(x[2]))
  names(out) <- vapply(temp, function(x) trimws(x[1]), character(1L))

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


