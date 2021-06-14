####
## Stan models from the second group (model.form == "finalize")
####

if (stan.model_which == "Simple3_NB.ZI") {
  
## Trial with 3 chains as will be used in a final run to check on convergence
stan.fit <- stan(
  file    = "stan_models/Simple3_NB.ZI.stan"
, data    = stan.data
, chains  = 3
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
   ## For safety also starting the squared terms at the center of the prior (zero)
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
,   list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
,   list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
)
, seed    = 1002
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))
 
} else if (stan.model_which == "Simple4_NB.ZI") {
  
stan.fit <- stan(
  file    = "stan_models/Simple4_NB.ZI.stan"
, data    = stan.data
, chains  = 1
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
   ## For safety also starting the squared terms at the center of the prior (zero)
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
,   list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
,   list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )
)
, seed    = 1002
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))  
  
} else if (stan.model_which == "NB.ZI.f") {
 
stan.fit <- stan(
  file    = "stan_models/NB.ZI.f.stan"
, data    = stan.data
, chains  = 1
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
   ## For safety also starting the squared terms at the center of the prior (zero)
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
  , sigma_year_lambda  = 1
  , temp_lambda_bar_sq = 0
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , theta              = 0.5
  , theta_out          = 0.5
  )) 
, seed    = 1003
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  )) 
  
} else if (stan.model_which == "NB.ZI.C.L") {
  
stan.fit <- stan(
  file    = "stan_models/NB.ZI.C.L.stan"
, data    = stan.data
, chains  = 1
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
   ## For safety also starting the squared terms at the center of the prior (zero)
  list(
    reciprocal_phi     = 1
  , phi                = 1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
  , sigma_year_lambda  = 1
  , alpha_lambda_bar   = 1
  , alpha_theta_bar    = 0
  , gdp_theta_bar      = 0
  , temp_lambda_bar    = 0
  , temp_lambda_bar_sq = 0
  , precip_lambda_bar    = 0
  , precip_lambda_bar_sq = 0
  , pop_lambda_bar    = 0
  , pop_lambda_bar_sq = 0
  , area_lambda_bar    = 0
  , area_lambda_bar_sq = 0
  , ag_lambda_bar    = 0
  , ndvi_lambda_bar    = 0
  , ndvi_lambda_bar_sq = 0
  , year_lambda_bar    = 0
  , eps_alpha_theta    = rep(0, 126)
  , eps_alpha_lambda   = rep(0, 126)
  , eps_year_lambda    = rep(0, 126)
  )) 
, seed    = 1003
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))
  
} else if (stan.model_which == "NB.ZI.C.S") {

stan.fit <- stan(
  file    = "stan_models/NB.ZI.C.S.stan"
, data    = stan.data
, chains  = 1
, iter    = ni
, warmup  = nb
, thin    = nt
, init    = list(
   ## With the generated quantities for the out of sample predictions need to 
    ## start the variance terms in a reasonable location
   ## For safety also starting the squared terms at the center of the prior (zero)
  list(
    reciprocal_phi     = .1
  , sigma_alpha_lambda = 1
  , sigma_alpha_theta  = 1
    
  , alpha_lambda_bar   = -1
  , alpha_theta_bar    = 0
    
  , gdp_theta_bar      = 0
    
  , temp_lambda_bar      = 0
  , temp_lambda_bar_sq   = 0
  , precip_lambda_bar    = 0
  , precip_lambda_bar_sq = 0
  , pop_lambda_bar       = 0
  , pop_lambda_bar_sq    = 0
  , area_lambda_bar      = 0
  , area_lambda_bar_sq   = 0
  , ag_lambda_bar        = 0
  , ndvi_lambda_bar      = 0
  , ndvi_lambda_bar_sq   = 0
    
  , eps_alpha_theta    = rep(0, stan.data$N_loc)
  , eps_alpha_lambda   = rep(0, stan.data$N_loc)
    
  , eps_year_deltas    = matrix(data = 0, nrow = stan.data$N_loc, ncol = stan.data$max_year - 1)
  , real_deltas_bar    = rep(0, stan.data$max_year)
  , delta_sigma        = 1
  , sigma_year_lambda  = 1
    
  ))
, seed    = 1008
## If issues arise can just give certain parameters for tracking for ease of use with shinystan
#, pars    = c(
#  "reciprocal_phi"
#, "phi"
#, "lambda_log"
#, "lambda_log_out"
#, "mu_out"
#, "mu"
#, "y_sim"
#, "y_sim_out"
#)
, control = list(
    adapt_delta = 0.95
  , max_treedepth = 12
  ))
  
}

launch_shinystan(stan.fit)

########
### Explore the fit (in a pretty non-dynamic way -- this will need to be updated to reflect
### the model that is actually fit)
########

## Plot the out of sample PI and data points
stan.fit.summary <- summary(stan.fit)[[1]]

hist(
  summary(stan.fit)[[1]][grep("eps_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 50
)

hist(
  summary(stan.fit)[[1]][grep("eps_alpha_theta", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_alpha_theta", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 50
)

hist(
  summary(stan.fit)[[1]][grep("eps_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1] *
  summary(stan.fit)[[1]][grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]), 1]
, breaks = 50
)

hist(
  exp(summary(stan.fit)[[1]][grep("lambda_log_out", dimnames(summary(stan.fit)[[1]])[[1]]), ][, 1])
, breaks = 50
)

### Rather un-dynamic here :(
stan.fit.summary.coef <- as.data.frame(stan.fit.summary[c(1:17
  , grep("sigma_alpha_lambda", dimnames(summary(stan.fit)[[1]])[[1]])
  , grep("sigma_year_lambda", dimnames(summary(stan.fit)[[1]])[[1]]))
  , c(4:8)])
stan.fit.summary.coef <- stan.fit.summary.coef %>% mutate(param = rownames(.))
names(stan.fit.summary.coef) <- c("lwr_w", "lwr_n", "mid", "upr_n", "upr_w", "param")
stan.fit.summary.coef <- stan.fit.summary.coef %>%
  arrange(param) %>% 
  mutate(param = factor(param, levels = rev(param)))
stan.fit.summary.coef <- stan.fit.summary.coef %>% 
  mutate(
    param_type    = c(c("slope", "int", "int"), rep("slope", 9), rep("var", 5), rep("slope", 3))
  )
stan.fit.summary.coef <- stan.fit.summary.coef %>% mutate(model_portion = "")
stan.fit.summary.coef$model_portion[grep("lambda", stan.fit.summary.coef$param)] <- "count"
stan.fit.summary.coef$model_portion[grep("phi", stan.fit.summary.coef$param)]    <- "count"
stan.fit.summary.coef$model_portion[grep("theta", stan.fit.summary.coef$param)]  <- "prob"

stan.fit.summary.coef <- stan.fit.summary.coef %>%
  mutate(param = plyr::mapvalues(param
    , from = param
    , to   = c(
      "Agriculutre (linear)"
    , "Intercept (count)"
    , "Intercept (prob)"
    , "Area (linear)"
    , "Area (sq)"
    , "GDP (linear)"
    , "NDVI (linear)"
    , "NDVI (sq)"
    , "Population (linear)"
    , "Population (sq)"
    , "Precipitation (linear)"
    , "Precipitation (sq)"
    , "Negative Binomial Variance"
    , "Random intercept variance (count)"
    , "Random intercept variance (prob)"
    , "Random year variance (count)"
    , "Random year variance sq (count)"
    , "Temperature (mean) (linear)"
    , "Temperature (mean) (sq)"
    , "Year (linear)"
    )))

stan.fit.summary.coef %>% filter(param_type == "slope") %>% arrange(mid) %>%
  mutate(param = factor(param, levels = param)) %>% {

ggplot(., aes(mid, param)) + 
  geom_point() +
  geom_errorbarh(aes(xmin = lwr_w, xmax = upr_w), height = 0.3) +
  ylab("Parameter") +
  xlab("Estimate") +
  geom_vline(xintercept = 0, linetype = "dotted", lwd = 1)
    
}

pred.out <- stan.fit.summary[grep("y_sim_out", dimnames(stan.fit.summary)[[1]]), ]
pred.out <- pred.out[, c(4, 8)]
pred.out <- data.frame(pred.out)
names(pred.out) <- c("lwr", "upr")
pred.out <- pred.out %>% mutate(entry = seq(1, n()))

test.out <- VL.year.out %>% ungroup( ) %>% mutate(entry = seq(1, n())) %>%
  left_join(., pred.out) %>% arrange(desc(vl_cases)) %>%
  mutate(ordered_entry = factor(seq(1, n())))

## Only plot a few random locations to not overload ggplot
randplotloc <- sample(seq(1, length(unique(VL.year$munip_name))), 30)
randplotloc <- unique(VL.year$munip_name)[randplotloc]

VL.year %>% filter(munip_name %in% randplotloc) %>% 
  droplevels() %>% {
 ggplot(., aes(mean_year, vl_cases)) + geom_point() +
    geom_vline(xintercept = 2013.5, linetype = "dotted") +
    facet_wrap(~munip_name) +
    geom_errorbar(
      data = 
        (test.out %>% filter(munip_name %in% randplotloc))
    , aes(mean_year, ymin = lwr, ymax = upr)
    , color = "red"
    , lwd = 0.5
    , width = 0.2
    ) + scale_y_log10()
  }

## Check a specific location
VL.year %>% filter(munip_name == "Chapadão Do Lageado") %>% 
  droplevels() %>% {
 ggplot(., aes(mean_year, vl_cases)) + geom_point() +
    geom_vline(xintercept = 2013.5, linetype = "dotted") +
    facet_wrap(~munip_name) +
    geom_errorbar(
      data = 
        (test.out %>% filter(munip_name == "Chapadão Do Lageado"))
    , aes(mean_year, ymin = lwr, ymax = upr)
    , color = "red"
    , lwd = 0.5
    , width = 0.2
    ) + scale_y_log10()
}
