# functions that wrap their ipmr equivalents

#' @noRd
#' @importFrom ipmr define_kernel define_k define_impl define_pop_state
#' define_env_state define_domains make_ipm

.define_kernel <- function(proto_ipm,
                           kernel_id,
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
                           un_tab) {


  # We don't want to define_k here, that comes next .make_proto

  skips <- c("iteration_procedure", "IPM")

  current_fam <- ik_tab$model_family[ik_tab$kernel_id == kernel_id]

  if(any(current_fam %in% skips)) return(proto_ipm)

  # Going through ipmr::define_kernel() arguments in order of appearance
  # Skip the "name" argument because that's just kernel_id. This leads to formula

  kern_text <- ik_tab$formula[ik_tab$kernel_id == kernel_id] %>%
    strsplit(x = ., split = "=") %>%
    unlist() %>%
    .[2] %>%
    trimws()

  kern_form <- rlang::parse_expr(kern_text)

  # Next, need family

  mod_fam <- ik_tab$model_family[ik_tab$kernel_id == kernel_id]

  # Next, we have ... = vital rate expression. This has two parts, because of
  # the density function stuff we have going on. These are flagged w/
  # "substituted" in the model_type column. The others should be readily
  # parse-able. The first step is to drop all the idiot self-assignment stuff
  # we implemented years ago, before I knew how to use rlang

  vr_tab <- vr_tab[! vapply(vr_tab$formula,
                            FUN = function(x) .is_the_same(x),
                            logical(1L)), ]

  vr_tab <- vr_tab[grepl(kernel_id, vr_tab$kernel_id), ]

  # Next, split out evaluated exprs - these are pretty much ready to go

  eval_fun_exprs <- vr_tab$formula[vr_tab$model_type == "Evaluated"]

  if(length(eval_fun_exprs) == 0) {

    kern_dots_eval <- NULL

  } else {

    kern_dots_text <- strsplit(eval_fun_exprs, " = ")

    kern_dots_nms  <- vapply(kern_dots_text,
                             function(x) trimws(x[1]),
                             character(1L))

    eval_fun_forms <- vapply(kern_dots_text,
                              function(x) trimws(x[2]),
                              character(1L))

    kern_dots_eval        <- lapply(eval_fun_forms, rlang::parse_expr)
    names(kern_dots_eval) <- kern_dots_nms

  }

  # get state variable info. Not sure we really need
  # the discrete state var names for ipmr, so dropping those for now.
  # We need the states to append them to the call to prep_dens_fun

  states <- list(c(sv_tab$state_variable[!sv_tab$discrete]))

  dens_fun_exprs <- vr_tab$formula[vr_tab$model_type == "Substituted"]
  kern_dots_subs <- .prep_sub_fun(dens_fun_exprs, states)

  all_kern_dots <- c(kern_dots_eval, kern_dots_subs)

  # Next, we need to create the data_list argument. This will be from the pv_tab
  # We're just going to pass every parameter to each kernel, as I'm not convinced
  # the penalty of passing extra data outweighs the giant pain in the ass it will
  # be to match parameters to kernels without kernel ids in the pv_tab

  par_list        <- as.list(pv_tab$parameter_value)
  names(par_list) <- pv_tab$parameter_name

  # Next, hierarchical effects. These get a list if present. If not, just keep
  # it empty

  if(dim(he_tab)[1] > 0) {

    has_hier_effs         <- any(grepl(kernel_id, he_tab$kernel_id))
    levs_hier_effs        <- lapply(he_tab$range,
                                    function(x) eval(parse(text = x)))

    names(levs_hier_effs) <- he_tab$vr_expr_name

  } else {

    has_hier_effs  <- FALSE
    levs_hier_effs <- list()

  }

  # Finally, add eviction if it's used, and construct the call to that
  # function. Since eviction is defined at the metadata row level rather
  # than the kernel level, we need to make sure kernels that don't have
  # an eviction expression are skipped when the proto_ipm is getting built

  ev_cor <- md_tab$eviction_used && length(dens_fun_exprs) > 0

  if(ev_cor) {

    ev_fun <- md_tab$evict_type

    if(ev_fun == "stretched_domain") {

      ev_call <- NULL
      ev_cor <- FALSE

    } else {

      use_state <- unique(c(ik_tab$domain_start[grepl(kernel_id, ik_tab$kernel_id)],
                            ik_tab$domain_end[grepl(kernel_id, ik_tab$kernel_id)]))

      # Remove NAs introduced by CD and DC kernels. Otherwise, too many arguments
      # to whichever function call we end up constructing.

      if(any(is.na(use_state))) {
        use_state <- use_state[!is.na(use_state)]
      }

      ev_target <- vapply(dens_fun_exprs,
                          function(x) strsplit(x, '=')[[1]][1] %>% trimws(),
                          character(1L),
                          USE.NAMES = FALSE)

      ev_target <- .add_ev_args(ev_fun,
                                ev_target,
                                dens_fun_exprs,
                                use_state)

      ev_call <- rlang::call2(ev_fun, !!! ev_target)

    }

  } else {

    ev_call <- NULL

  }

  out <- ipmr::define_kernel(
    proto_ipm        = proto_ipm,
    name             = kernel_id,
    formula          = !! kern_form,
    family           = mod_fam,
    !!! all_kern_dots,
    data_list        = par_list,
    states           = states,
    has_hier_effs    = has_hier_effs,
    levels_hier_effs = levs_hier_effs,
    evict_cor        = ev_cor,
    evict_fun        = !! ev_call
  )

  return(out)

}

#' @noRd
# Defines the iteration kernel and the iteration procedure, and handles all
# of the transformations to ipmr format

.define_k <- function(proto_ipm,
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
                      det_stoch) {

  # Get name for define_k. There really should only be *1* unique name
  # per model.
  # I don't htink this can ever happen - but just checking.

  if(length(unique(kern_ids)) > 1) {

    out_nm <- unique(kern_ids)[grepl("K", unique(kern_ids))][1]
    warning("Found multiple names for K in model: ", md_tab$ipm_id,
            "\nUsing the first name. Please contact database maintainers to make",
            " sure this is not an error.",
            call. = FALSE)

  } else {

    out_nm <- unique(kern_ids)

  }

  # Hard code this, as it really can't be anything else without breaking
  # ipmr

  out_fam <- "IPM"

  out_dots <- .prep_k_dots(ik_tab, ps_tab, he_tab, det_stoch)

  out_par_list <- proto_ipm$params[[1]]$params

  out_states <- as.list(unique(c(ik_tab$domain_start, ik_tab$domain_end))) %>%
    Filter(f = Negate(is.na), x = .)

  if(dim(he_tab)[1] > 0){

    out_has_he <- any(grepl(out_nm, he_tab$kernel_id))

  } else {

    out_has_he <- FALSE

  }

  if(out_has_he) {

    out_lev_he <- lapply(he_tab$range,
                         function(x) eval(parse(text = x)))

    names(out_lev_he) <- he_tab$vr_expr_name

  } else {

    out_lev_he <- list()

  }

  out <- ipmr::define_k(
    proto_ipm        = proto_ipm,
    name             = out_nm,
    family           = out_fam,
    !!! out_dots,
    data_list        = out_par_list,
    states           = out_states,
    has_hier_effs    = out_has_he,
    levels_hier_effs = out_lev_he,
    evict_cor        = FALSE,
    evict_fun        = NULL
  )

  return(out)

}

#' @noRd
# Wrapper for ipmr::define_impl()

.define_impl <- function(proto_ipm,
                         ir_tab,
                         ik_tab) {


  impl_list <- list()

  for(i in seq_along(ik_tab$kernel_id)) {

    use_id <- ik_tab$kernel_id[i]

    temp <- ipmr::make_impl_args_list(
      kernel_names = use_id,
      int_rule     = ifelse(any(grepl(use_id, ir_tab$kernel_id)),
                            ir_tab$integration_rule,
                            NA_character_),
      dom_start    = ifelse(is.na(ik_tab$domain_start[i]),
                            NA_character_,
                            ik_tab$domain_start[i]),
      dom_end      = ifelse(is.na(ik_tab$domain_end[i]),
                            NA_character_,
                            ik_tab$domain_end[i])
      )

    impl_list <- c(impl_list, temp)
  }


  out <- define_impl(proto_ipm, impl_list)

  return(out)
}

#' @noRd
# wrapper for ipmr::define_domains

.define_domains <- function(proto_ipm,
                            cd_tab,
                            ps_tab,
                            ik_tab) {

  n_svs <- dim(cd_tab)[1]

  dom_list <- list()

  for(i in seq_len(n_svs)) {

    dom_list[[i]] <- c(cd_tab$lower[i],
                       cd_tab$upper[i],
                       ps_tab$n_bins[grepl(cd_tab$state_variable[i],
                                           ps_tab$expression)])

    names(dom_list)[i] <- cd_tab$state_variable[i]

  }

  out <- define_domains(proto_ipm,
                        !!! dom_list)

  return(out)

}

#' @noRd
# wrapper for ipmr::define_pop_state

.define_pop_state <- function(proto_ipm,
                              det_stoch,
                              ps_tab,
                              he_tab) {

  # Fail if bin information simply isn't entered.

  if(any(is.na(ps_tab$n_bins))) {

    msg <- paste("Could not find number of bins for model: ",
                 unique(ps_tab$ipm_id),
                 sep = "")

    msg <- paste(msg, collapse = "\n")

    stop(msg,
         "\nContact database developers for further help.", call. = FALSE)

  } else if(dim(ps_tab)[1] == 0) {

    # Otherwise, warn that no iteration based methods available, and just
    # make a deterministic model.

    warning("No population state functions defined for model: ",
            unique(ps_tab$ipm_id),
            "\nDeterministic analysis with eigenvalues is only possible option.")

    sim_gen <- strsplit(class(proto_ipm), '_')[[1]][1]

    new_cls <- paste(sim_gen, "_di_det", sep = "")

    class(proto_ipm)[1] <- new_cls

    return(proto_ipm)

  }

  tot_pop <- sum(ps_tab$n_bins)

  pop_vecs <- list()

  # Generate a suffix for deterministic models with hier_effs to append to
  # population state names. This way, ipmr generates a different iteration
  # population state for every level of grouping var combinations, and can
  # iterate the models correctly.

  if((det_stoch == "det") && (dim(he_tab)[1] > 0)) {

    all_suf <- paste(he_tab$vr_expr_name, collapse = "_")
    nm_sep  <- "_"

  } else {

    # If this isn't present, then just create dummies for the names so
    # we can proceed normally

    all_suf <- NULL
    nm_sep  <- ""
  }

  for(i in seq_along(ps_tab$expression)) {

    pop_vecs[[i]]      <- rep(1 / tot_pop,
                              times = ps_tab$n_bins[i])

    names(pop_vecs)[i] <- paste(ps_tab$expression[i], all_suf, sep = nm_sep)

  }

  out <- define_pop_state(
    proto_ipm,
    pop_vectors = pop_vecs
  )

  return(out)

}

#' @noRd
#
.define_env_state <- function(proto_ipm, ev_tab) {


  return(proto_ipm)
}
