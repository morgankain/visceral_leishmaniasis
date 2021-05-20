
data {

  int<lower=0> N;                    // number of observations
  int<lower=0> y[N];                 // VL case counts

  int<lower=0> N_out;                // number of observations for out of sample predictions


  int<lower=0> N_loc;		     // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	     // ID of each location 
    
  // Covariates
  vector[N] temp;                    // temperature
  vector[N] precip;                  // precipitation
  vector[N] year;                    // year
  vector[N] pop;                     // population

  // out of sample predictions 
  vector[N_out] temp_out;            // temperature
  vector[N_out] precip_out;          // precipitation
  vector[N_out] year_out;            // year
  vector[N_out] pop_out;             // population

}

parameters {

  // Fixed Effects

  // Intercepts 
  real alpha_theta_bar;                 // intercept for probability piece 
  real alpha_lambda_bar;                // intercept for count piece  

  // Slopes
  real temp_lambda_bar;                 // slope coefficients for count piece
  real precip_lambda_bar;       
  real year_lambda_bar;  
  real pop_lambda_bar;

  // Variance/Dispersion
  real<lower=0, upper=100> reciprocal_phi;

  // Random Effects, Non-Centered Parameterization, see Section 21.7 in the Stan Users Guide

  real<lower=0> sigma_alpha_lambda;      // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;        // deviates for each place drawn from sigma_alpha (count)

  real<lower=0> sigma_year_lambda;       // estimated variation among locations in slopes (count)
  vector[N_loc] eps_year_lambda;         // deviates for each place drawn from the sigma_beta values (count)

}

transformed parameters {

  real alpha_lambda[N_loc];             // location-specific intercepts for counts
  real year_lambda[N_loc];              // location-specific slope over year for count

  real<lower=0, upper=1> theta;         // linear predictor for probability piece
  real lambda_log[N];                   // linear predictor for count piece

  real lambda_log_out[N_out];           // linear predictor for count piece for out of sample predictions

  real phi;                             // dispersion

for (i in 1:N_loc) {

  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];
  year_lambda[i] = year_lambda_bar + sigma_year_lambda * eps_year_lambda[i];

}    

for (j in 1:N) {

  lambda_log[j] = alpha_lambda[loc_id[j]] + 
    temp_lambda_bar * temp[j] +
    precip_lambda_bar * precip[j] +
    year_lambda[loc_id[j]] * year[j] +
    pop_lambda_bar * pop[j]; 

}

for (jj in 1:N_out) {

  lambda_log_out[jj] = alpha_lambda_bar + 
    temp_lambda_bar * temp_out[jj] +
    precip_lambda_bar * precip_out[jj] +
    year_lambda_bar * year_out[jj] +
    pop_lambda_bar * pop_out[jj]; 

}

  theta = inv_logit(alpha_theta_bar);
  phi = 1. / reciprocal_phi;

}

model {

// priors. Break up the beta's according to some other independent data sources...

// fixed effects
   alpha_theta_bar ~ normal(0, 3);
   alpha_lambda_bar ~ normal(0, 3);

   temp_lambda_bar ~ normal(0, 3);
   year_lambda_bar ~ normal(0, 3);
   precip_lambda_bar ~ normal(0, 3);
   pop_lambda_bar ~ normal(0, 3);

// random effects
   sigma_alpha_lambda ~ inv_gamma(1, 1);
   sigma_year_lambda ~ inv_gamma(1, 1);

   eps_alpha_lambda ~ normal(0, 1);
   eps_year_lambda ~ normal(0, 1);

   reciprocal_phi ~ inv_gamma(1, 1);

// modify the likelihood
   for(n in 1:N) {
    
if (y[n] == 0) {
     
      target += log_sum_exp(bernoulli_lpmf(1 | theta),                 
                             bernoulli_lpmf(0 | theta)
                           + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi));


    } else {

      target += bernoulli_lpmf(0 | theta)                              
                  + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi);

    }
  }
}

generated quantities {

// for simulating values

 int<lower=0> y_sim[N];
 int zero_pred;
 real mu[N];

 int<lower=0> y_sim_out[N_out];
 int zero_pred_out;
 real mu_out[N_out];

 mu = exp(lambda_log);

 mu_out = exp(lambda_log_out);

 for(n in 1:N) {

  zero_pred    = bernoulli_rng(theta); 
  y_sim[n]     = (1 - zero_pred) * neg_binomial_2_rng(mu[n], phi);

  }

 for(nn in 1:N_out) {

  zero_pred_out = bernoulli_rng(theta); 
  y_sim_out[nn] = (1 - zero_pred_out) * neg_binomial_2_rng(mu_out[nn], phi);

  }


}

