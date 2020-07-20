# functions for .define_single_kernel()
#' @noRd
#' @importFrom ipmr define_kernel define_k define_impl define_pop_state
#' define_env_state define_domains make_ipm

.define_single_kernel <- function(proto_ipm,
                                  kernel_id,
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
                                  un_tab) {


  # First, get state variable info. Not sure we really need
  # the discrete state var names for ipmr, so dropping those for now.g

  states <- sv_tab$state_variable[sv_tab$discrete == 'f']

  # Next, drop self assignment formulae. This won't be necessary after the
  # the database is corrected internally, but needs to happen for the
  # current version

  vr_tab <- vr_tab[! vapply(vr_tab$formula,
                            FUN = function(x) .is_the_same(x),
                            logical(1L)), ]

}
