####
## Stan models
####

## just a simple Poisson first to work on overall structure and to check for obvious errors 
if (stan.model_which == "Simple_Poisson") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_pois_simp.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
 ))

## Compare these to an lme4 mixed model
test.nb.s <- glmer(
  vl_cases ~ year + mean_temp + mean_precip + (1 | munip_name)
  , offset = mean_pop
  , data = (VL.year %>% filter(munip_name %in% rand_locs) %>% mutate(year = scale(year, scale = F)))
  , family = "poisson")

print(
cbind(
 summary(stan.fit)[[1]][c(1, 4, 2, 3), 1]
, fixef(test.nb.s))
)

plot(
cbind(
  summary(stan.fit)[[1]][
    grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] * summary(stan.fit)[[1]][5, 1]
, getME(test.nb.s, "b")[, 1]
  ))

## Slightly more complicated model to check for slope random effect convergence
} else if (stan.model_which == "Simple2_Poisson") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_pois_simp2.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))

test.nb.s <- glmer(
  vl_cases ~ year + mean_temp + mean_precip + (1 + year | munip_name)
  , offset = mean_pop
  , data = (VL.year %>% filter(munip_name %in% rand_locs) %>% mutate(year = scale(year, scale = F)))
  , family = "poisson")

print(
cbind(
  summary(stan.fit)[[1]][c(1, 4, 2, 3), 1]
, fixef(test.nb.s))
)

plot(
cbind(
  summary(stan.fit)[[1]][
    grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] * 
    summary(stan.fit)[[1]][5, 1]
, getME(test.nb.s, "b")[seq(1, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)

plot(
cbind(
  summary(stan.fit)[[1]][
    grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] * 
    summary(stan.fit)[[1]][
    grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
, getME(test.nb.s, "b")[seq(2, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)
  
} else if (stan.model_which == "Simple2_NB") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_nb_simp2.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))

test.nb.s <- glmer.nb(
  vl_cases ~ year + mean_temp + mean_precip + (1 + year | munip_name)
  , offset = mean_pop
  , data = (VL.year %>% filter(munip_name %in% rand_locs) %>% mutate(year = scale(year, scale = F))))

print(
cbind(
  summary(stan.fit)[[1]][c(1, 4, 2, 3), 1]
, fixef(test.nb.s))
)

plot(
cbind(
  summary(stan.fit)[[1]][
    grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
    summary(stan.fit)[[1]][6, 1]
, getME(test.nb.s, "b")[seq(1, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)

plot(
cbind(
  summary(stan.fit)[[1]][
    grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
    summary(stan.fit)[[1]][
    grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
, getME(test.nb.s, "b")[seq(2, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)
  
} else if (stan.model_which == "Simple2_Poisson.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_pois_zero_inf_simp2.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))

hist(
  summary(stan.fit)[[1]][
    grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
  summary(stan.fit)[[1]][
    grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
  , breaks = 30
)

hist(
  summary(stan.fit)[[1]][
    grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
  summary(stan.fit)[[1]][
    grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
  , breaks = 30
)
  
} else if (stan.model_which == "Simple2_NB.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_nb_zero_inf_simp2.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))

# hist(rnbinom(10000, mu = exp(summary(stan.fit)[[1]][1640, 1]), size = summary(stan.fit)[[1]][6, 1]), breaks = 500)

hist(
  summary(stan.fit)[[1]][
    grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
  summary(stan.fit)[[1]][
    grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
  , breaks = 30
)

hist(
  summary(stan.fit)[[1]][
    grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , ][, 1] *
  summary(stan.fit)[[1]][
    grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
    , 1]
  , breaks = 30
)

} else if (stan.model_which == "Simple3_NB.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_nb_zero_inf_simp3.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  )) 
  
} else if (stan.model_which == "Simple4_NB.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/VL_sep_nb_zero_inf_simp4.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))
  
}
