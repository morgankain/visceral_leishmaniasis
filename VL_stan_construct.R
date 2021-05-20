####
## Stan models from group 1 through 3 (see VL_stan.R)
####

## just a simple Poisson first to work on overall structure and to check for obvious errors 
if (stan.model_which == "Simple_Poisson") {
  
stan.fit <- stan(
  file    = "stan_models/Simple_Poisson.stan"
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
  vl_cases ~ year + temp + precip + pop + (1 | munip_name)
  , data = lme4.data
  , family = "poisson")

print(
cbind(
  fixef(test.nb.s)
, summary(stan.fit)[[1]][c(1, 4, 2, 3, 5), 1]
  )
)

plot(
cbind(
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][1] 
, getME(test.nb.s, "b")[, 1]
  )); abline(a = 0, b = 1)

## Slightly more complicated model to check for slope random effect convergence
} else if (stan.model_which == "Simple2_Poisson") {
  
stan.fit <- stan(
  file    = "stan_models/Simple2_Poisson.stan"
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
  vl_cases ~ year + temp + precip + pop + (1 + year | munip_name)
  , data = lme4.data
  , family = "poisson")

print(
cbind(
  fixef(test.nb.s)
, summary(stan.fit)[[1]][c(1, 4, 2, 3, 5), 1]
  )
)

plot(
cbind(
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] * 
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][1]
, getME(test.nb.s, "b")[seq(1, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)

plot(
cbind(
  summary(stan.fit)[[1]][grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] * 
  summary(stan.fit)[[1]][grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, getME(test.nb.s, "b")[seq(2, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)
  
} else if (stan.model_which == "Simple2_NB") {
  
stan.fit <- stan(
  file    = "stan_models/Simple2_NB.stan"
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
  vl_cases ~ year + temp + precip + pop + (1 + year | munip_name)
  , data = lme4.data
  )

print(
cbind(
  fixef(test.nb.s)
, summary(stan.fit)[[1]][c(1, 4, 2, 3, 5), 1]
))

plot(
cbind(
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][1]
, getME(test.nb.s, "b")[seq(1, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)

plot(
cbind(
  summary(stan.fit)[[1]][grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, getME(test.nb.s, "b")[seq(2, nrow(getME(test.nb.s, "b")), by = 2), 1])
); abline(a = 0, b = 1)
  
} else if (stan.model_which == "Simple2_Poisson.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/Simple2_Poisson.ZI.stan"
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
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 30
)

hist(
  summary(stan.fit)[[1]][grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
  , breaks = 30
)

## Also, explore the prediction intervals from this model against the data.
  
} else if (stan.model_which == "Simple2_NB.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/Simple2_NB.ZI.stan"
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
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 30
)

hist(
  summary(stan.fit)[[1]][grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 30
)

## Also, explore the prediction intervals from this model against the data.
stan.fit.summary <- summary(stan.fit)[[1]]
pred.out <- stan.fit.summary[grep("y_sim", dimnames(stan.fit.summary)[[1]]), ]
pred.out <- pred.out[, c(4, 8)]
pred.out <- data.frame(pred.out)
names(pred.out) <- c("lwr", "upr")
pred.out <- pred.out %>% mutate(entry = seq(1, n()))

test.out <- VL.stan %>% ungroup( ) %>% mutate(entry = seq(1, n())) %>%
  left_join(., pred.out) %>% arrange(desc(vl_cases)) %>%
  mutate(ordered_entry = factor(seq(1, n())))

ggplot(test.out, aes(ordered_entry, vl_cases)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "red", alpha = 0.4) +
  geom_point(aes(ordered_entry, vl_cases))

} else if (stan.model_which == "Simple2_NB.ZI.OoS.S") {

####
## NOTE: This isn't quite correct because the covaraites need to get scaled AFTER the split
## to the fitting set and out-of-sample set. However, for a proof of concept this is fine...
## It is fixed in the subsequent model fits with the greater number of covaraites. Keeping it 
## because it fits in with the script well, but keep in mind it isn't quite right
####
  
    
top_locs.out  <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% slice((deubg.top_samps + 1):(deubg.top_samps + 10)) %>% 
  dplyr::select(munip_name) %>% unname() %>% unlist()

rand_locs.out <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% filter(munip_name %notin% top_locs) %>% 
  filter(munip_name %notin% rand_locs) %>% 
  dplyr::select(munip_name) %>% unname() %>% 
  unlist() %>% sample(10)
 
rand_locs.out <- c(rand_locs.out, top_locs.out)

VL.stan.out <- VL.year %>% filter(munip_name %in% rand_locs.out)

stan.data <- c(
  stan.data
, N_out          = nrow(VL.stan.out)
   ## Center out of sample data relative to the scale of the data used to fit the model
    ## to have the correct values for the fitted coefficients
, temp_out       = list(c(VL.stan.out$mean_temp - mean(VL.stan$mean_temp)))
, precip_out     = list(c(scale(VL.stan.out$mean_precip, scale = F)))
, year_out       = list(c(VL.stan.out$mean_year - mean(VL.stan$mean_year)))
, pop_out        = list(c(VL.stan.out$mean_pop - mean(VL.stan$mean_pop)))
)
  
stan.fit <- stan(
  file    = "stan_models/Simple2_NB.ZI.OoS.S.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, seed    = 1001
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_year_lambda  = 1
  )
)
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  )) 

## Plot the out of sample PI and data points
stan.fit.summary <- summary(stan.fit)[[1]]
pred.out <- stan.fit.summary[grep("y_sim_out", dimnames(stan.fit.summary)[[1]]), ]
pred.out <- pred.out[, c(4, 8)]
pred.out <- data.frame(pred.out)
names(pred.out) <- c("lwr", "upr")
pred.out <- pred.out %>% mutate(entry = seq(1, n()))

test.out <- VL.stan.out %>% ungroup( ) %>% mutate(entry = seq(1, n())) %>%
  left_join(., pred.out) %>% arrange(desc(vl_cases)) %>%
  mutate(ordered_entry = factor(seq(1, n())))

## What this tells us is that the fixed effects currently in th model predict basically none of the variance in the data
 ## and that the variation in locations is all sucked up by the random effect
ggplot(test.out, aes(ordered_entry, vl_cases)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "red", alpha = 0.4) +
  geom_point(aes(ordered_entry, vl_cases))
  
} else if (stan.model_which == "Simple2_NB.ZI.OoS.T") {
  
## Set up the out of sample years for the same locations here
VL.stan.in  <- VL.stan %>% filter(mean_year < 2014)
VL.stan.out <- VL.stan %>% filter(mean_year > 2013)
  
stan.data   <- list(
  N              = nrow(VL.stan.in)
, y              = VL.stan.in$vl_cases
, N_loc          = length(unique(VL.stan.in$munip_name))
, loc_id         = as.numeric(as.factor(VL.stan.in$munip_name))
, temp           = VL.stan.in$temp
, precip         = VL.stan.in$precip
, year           = VL.stan.in$year
, pop            = VL.stan.in$pop
## The out of sample covariates are centered using the center of the covariates used to fit the model 
, N_out          = nrow(VL.stan.out)
, N_loc_out      = length(unique(VL.stan.out$munip_name))
, loc_id_out     = as.numeric(as.factor(VL.stan.out$munip_name))
, temp_out       = c(VL.stan.out$mean_temp - mean(VL.stan.in$mean_temp))
, precip_out     = c(VL.stan.out$mean_precip - mean(VL.stan.in$mean_precip))
, year_out       = c(VL.stan.out$mean_year - mean(VL.stan.in$mean_year))
, pop_out        = c(VL.stan.out$mean_pop - mean(VL.stan.in$mean_pop))
  )

stan.fit <- stan(
  file    = "stan_models/Simple2_NB.ZI.OoS.T.stan"
, data    = stan.data
, chains  = nc
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_year_lambda  = 1
  )
)
, seed    = 1001
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))
  
## Plot the out of sample PI and data points
stan.fit.summary <- summary(stan.fit)[[1]]
pred.out <- stan.fit.summary[grep("y_sim_out", dimnames(stan.fit.summary)[[1]]), ]
pred.out <- pred.out[, c(4, 8)]
pred.out <- data.frame(pred.out)
names(pred.out) <- c("lwr", "upr")
pred.out <- pred.out %>% mutate(entry = seq(1, n()))

test.out <- VL.stan.out %>% ungroup( ) %>% mutate(entry = seq(1, n())) %>%
  left_join(., pred.out) %>% arrange(desc(vl_cases)) %>%
  mutate(ordered_entry = factor(seq(1, n())))

## Check this 4th data point. Not awesome, but as expected much better than the previous model. Also not unexpected 
 ## that this is only marginally ok given how few predictors are in the model. 
VL.stan %>% {
 ggplot(., aes(mean_year, vl_cases)) + geom_point() +
    geom_vline(xintercept = 2013.5, linetype = "dotted") +
    facet_wrap(~munip_name) +
    geom_errorbar(
      data = test.out
    , aes(mean_year, ymin = lwr, ymax = upr)
    , color = "red"
    , lwd = 0.5
    , width = 0.2
    ) +
    scale_y_log10()
}

}
