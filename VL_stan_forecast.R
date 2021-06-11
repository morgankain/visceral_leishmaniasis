####
## Final Stan models for forecasting into the next two years
####

if (stan.model_which == "NB.ZI.F.L") {

stan.fit <- stan(
  file    = "stan_models/NB.ZI.F.L.stan"
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
  , sigma_alpha_lambda = .3
  , sigma_alpha_theta  = .3
  , sigma_year_lambda  = .3
    
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
  , eps_year_lambda    = rep(0, stan.data$N_loc)
    
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
  
} else if (stan.model_which == "NB.ZI.F.S") {
  
stan.fit <- stan(
  file    = "stan_models/NB.ZI.F.S.stan"
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
  , sigma_alpha_lambda = .3
  , sigma_alpha_theta  = .3
    
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
  , delta_sigma        = .3
  , sigma_year_lambda  = .3
    
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
pred.out <- pred.out %>% mutate(
  year = rep(c(max(VL.year$year) + 1, max(VL.year$year) + 2)
    , length(unique(VL.year$munip_name)))
, munip_name = rep(unique(VL.year$munip_name), each = 2)
)

## Check this 4th data point. Not awesome, but as expected much better than the previous model. Also not unexpected 
 ## that this is only marginally ok given how few predictors are in the model. 
randplotloc <- sample(seq(1, length(unique(VL.year$munip_name))), 50)
randplotloc <- unique(VL.year$munip_name)[randplotloc]

VL.year %>% filter(munip_name %in% randplotloc) %>% 
  droplevels() %>% {
 ggplot(.) + geom_point(aes(year, vl_cases)) +
    facet_wrap(~munip_name) +
    geom_errorbar(
      data = (pred.out %>% filter(munip_name %in% randplotloc))
    , aes(year, ymin = lwr, ymax = upr)
    , color = "red"
    , lwd = 0.5
    , width = 0.2
    ) + scale_y_log10() +
      scale_x_continuous(breaks = c(
        2007, 2009, 2011, 2013, 2015, 2017
      )) +
      theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 10, angle = 310, hjust = 0) 
  , axis.text.y = element_text(size = 12) 
  , axis.title.x = element_text(size = 12) 
  , axis.title.y = element_text(size = 12)
  )
  }
