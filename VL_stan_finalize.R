####
## Stan models from group 4 through 6 (see VL_stan.R)
####

## For the models in group 4 through 6 the data setup 

if (stan.model_which == "Simple3_NB.ZI") {
  
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
  
}

## Plot the out of sample PI and data points
stan.fit.summary <- summary(stan.fit)[[1]]
pred.out <- stan.fit.summary[grep("y_sim_out", dimnames(stan.fit.summary)[[1]]), ]
pred.out <- pred.out[, c(4, 8)]
pred.out <- data.frame(pred.out)
names(pred.out) <- c("lwr", "upr")
pred.out <- pred.out %>% mutate(entry = seq(1, n()))

test.out <- VL.year.out %>% ungroup( ) %>% mutate(entry = seq(1, n())) %>%
  left_join(., pred.out) %>% arrange(desc(vl_cases)) %>%
  mutate(ordered_entry = factor(seq(1, n())))

## Check this 4th data point. Not awesome, but as expected much better than the previous model. Also not unexpected 
 ## that this is only marginally ok given how few predictors are in the model. 
VL.year %>% {
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
