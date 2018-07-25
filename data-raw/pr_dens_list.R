# create internal probability density function table

internal_data <- list(pr_dens_dictionary = list(Beta = 'dbeta',
                                                Binom = 'dbinom',
                                                Bernoulli = 'dbinom',
                                                Cauchy = 'dcauchy',
                                                Chi = 'dchisq',
                                                Expo = 'dexp',
                                                F_dist = 'df',
                                                Gamma = 'dgamma',
                                                Geom = 'dgeom',
                                                Hgeom = 'dhyper',
                                                Lognorm = 'dlnorm',
                                                Multinom = 'dmultinom',
                                                Negbin = 'dnbinom',
                                                Norm = 'dnorm',
                                                Pois = 'dpois',
                                                T_Dist = 'dt',
                                                Unif = 'dunif',
                                                Weib = 'dweibull'))

devtools::use_data(internal_data, internal = TRUE, overwrite = TRUE)
