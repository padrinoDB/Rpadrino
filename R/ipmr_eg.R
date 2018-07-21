library(rlang)

#
my_pars <- list(a = 0, b = 1, s1 = 1)
my_expr <- exprs(mu = a + b * x, sd = s1, G = dnorm(x_, mu, sd))
D1_vals <- expand.grid(x_ = 1:10, x = 1:10)

#
D1 <- child_env(.parent = global_env())
#
env_bind(D1, !!! D1_vals)
#
env_names(D1)


#
K1 <- child_env(.parent = D1)
#
env_bind(.env = K1, !!! my_pars)
# subject to possible rename: https://github.com/r-lib/rlang/issues/448
env_bind_exprs(.env = K1, # <- where to bind the names-values
               !!! my_expr, # <- expressions to evaluate
               .eval_env = K1) # <- evaluation context
#
env_names(K1)
#
K1$a
K1$b
K1$mu
K1$s1
K1$G
