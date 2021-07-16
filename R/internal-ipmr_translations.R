# functions that wrap their ipmr equivalents

#' @noRd
#' @importFrom ipmr define_kernel define_impl define_pop_state
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

  vr_tab <- vr_tab[.kernel_ids(kernel_id, vr_tab$kernel_id), ]

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

    .check_eval_fun_forms(eval_fun_forms, unique(vr_tab$ipm_id))

    kern_dots_eval        <- lapply(eval_fun_forms, rlang::parse_expr)
    names(kern_dots_eval) <- kern_dots_nms

  }

  # get state variable info. At the kernel level, this is comprised of
  # domain start, and domain end. For now, we have to assume all probability
  # density functions generate z'. I cannot imagine a case where that isn't true
  # (except maybe demographic stochasticity?)

  all_states     <- ik_tab[ik_tab$kernel_id == kernel_id, c("domain_start",
                                                            "domain_end")]
  sub_fun_state  <- all_states[2]
  if("age_x_size" %in% class(proto_ipm)) sub_fun_state <- gsub("(_age)",
                                                               "",
                                                               sub_fun_state)


  dens_fun_exprs <- vr_tab$formula[vr_tab$model_type == "Substituted"]
  kern_dots_subs <- .prep_sub_fun(dens_fun_exprs, sub_fun_state)

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

    uses_par_sets          <- any(grepl(kernel_id, he_tab$kernel_id) &
                                  !he_tab$vr_expr_name %in% c("age", "max_age"))

    if(uses_par_sets) {

      use_par_sets <- he_tab[!he_tab$vr_expr_name %in% c("age", "max_age"), ]

      par_set_indices <- lapply(
        use_par_sets$range,
        function(x) eval(parse(text = x))
      )

      names(par_set_indices) <- use_par_sets$vr_expr_name

    } else {

      par_set_indices <- list()
    }

  } else {

    uses_par_sets   <- FALSE
    par_set_indices <- list()

  }

  if("age_x_size" %in% class(proto_ipm)) {

    use_rows <- he_tab[he_tab$vr_expr_name %in% c("age", "max_age"), ]
    age_inds <- lapply(
      use_rows$range,
      function(x) eval(parse(text = x))
    )

    names(age_inds) <- use_rows$vr_expr_name

  } else {

    age_inds <- list()
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

      use_state <- eval(unlist(all_states, use.names = FALSE)[2])

      if("age_x_size" %in% class(proto_ipm)) use_state <- gsub("(_age)", "",
                                                               use_state)


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
    states           = all_states,
    uses_par_sets    = uses_par_sets,
    par_set_indices  = par_set_indices,
    age_indices      = age_inds,
    evict_cor        = ev_cor,
    evict_fun        = !! ev_call
  )

  return(out)

}


#' @noRd
# Wrapper for ipmr::define_impl()

.define_impl <- function(proto_ipm,
                         ir_tab,
                         ik_tab) {


  impl_list <- list()

  use_id <- unique(ir_tab$ipm_id)

  if(length(use_id) > 1) {
    stop("internal error - too many 'ipm_id's passed to .define_impl().")
  }

  if(is.na(ir_tab$integration_rule[ir_tab$ipm_id == use_id])) {
    stop("No integration rule found for model: ", use_id,
         call. = FALSE)
  }

  # Need to remove all K rows from the IpmKernels table eventually,
  # but this should keep them from causing any trouble in the meantime

  ik_tab <- ik_tab[!ik_tab$kernel_id %in% "K", ]

  for(i in seq_along(ik_tab$kernel_id)) {

    kern_id <- ik_tab$kernel_id[i]

    temp <- ipmr::make_impl_args_list(
      kernel_names = kern_id,
      int_rule     = ir_tab$integration_rule[grepl(kern_id, ir_tab$kernel_id)],
      state_start  = ik_tab$domain_start[i],
      state_end    = ik_tab$domain_end[i])

    impl_list <- c(impl_list, temp)
  }


  out <- define_impl(proto_ipm, impl_list)

  return(out)
}

#' @noRd
# wrapper for ipmr::define_domains

.define_domains <- function(proto_ipm,
                            cd_tab,
                            ps_tab) {

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

  # Fail if bin information or trait distribution information isn't entered.

  if(any(is.na(ps_tab$n_bins))) {

    msg <- paste("Could not find number of bins for model: ",
                 unique(ps_tab$ipm_id),
                 sep = "")

    msg <- paste(msg, collapse = "\n")

    stop(msg,
         "\nContact database developers for further help.", call. = FALSE)

  } else if(dim(ps_tab)[1] == 0) {

    stop("No population trait distribution information found.", call. = FALSE)

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
#' @importFrom stats setNames

.define_env_state <- function(proto_ipm, ev_tab) {

  ran_calls <- ev_tab[ev_tab$model_type %in% c("Substituted",
                                               "Evaluated"), ]

  par_ind <- (ev_tab$model_type == "Parameter" | ev_tab$env_range != "NULL")
  par_ind <- par_ind & !is.na(par_ind)

  # Call eval(parse()) to convert to numerics. data will technically be
  # strings because of the NULLs
  data_list <- as.list(ev_tab$env_range[par_ind]) %>%
    setNames(ev_tab$vr_expr_name[par_ind]) %>%
    lapply(function(x) {
      eval(parse(text = x))
    })


  env_funs <- list()

  for(i in seq_along(ran_calls)) {

    expr_type <- ran_calls$model_type[i]
    env_var   <- ran_calls$env_variable[i]
    out_nm    <- ran_calls$vr_expr_name[i]
    ran_expr  <- rlang::parse_expr(ran_calls$env_function[i])

    temp_dl  <- data_list[names(data_list) %in%
                            ev_tab[ev_tab$env_variable == env_var,
                                   "vr_expr_name", drop = TRUE]]

    # temp_dl  <- Filter(function(x) x != "NULL", temp_dl)

    env_funs <- c(env_funs, .new_env_fun(ran_expr, out_nm, temp_dl, expr_type))

  }


  proto_ipm <- define_env_state(
    proto_ipm = proto_ipm,
    !!! env_funs,
    data_list = data_list
  )

  return(proto_ipm)
}

.kernel_ids <- function(target_id, kernel_ids) {

  out <- vapply(kernel_ids,
                       function(x, target) {
                         temp <- strsplit(x, ';') %>%
                           unlist() %>%
                           trimws()
                           any(temp == target)
                       },
                       target = target_id,
                logical(1L))

  return(out)

}

# checks vr_exprs for functions that ipmr doesn't currently support
# Currently, just GAM, but others should probably go in here too.

#' @noRd
.check_eval_fun_forms <- function(forms, ipm_id) {

  bad_forms <- c("GAM")

  if(any(bad_forms %in% forms)) {
    stop("Model ID ", ipm_id, " contains functional forms that are not currently",
         " supported by ipmr.\nRe-run without these 'ipm_id's.")
  }

  invisible(TRUE)

}

