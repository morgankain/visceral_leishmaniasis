
data {

  int<lower=0> N;                    // number of observations
  int<lower=0> y[N];                 // VL case counts

  int<lower=0> N_loc;		     // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	     // ID of each location 
    
  // Covariates
  vector[N] temp;                    // temperature
  vector[N] precip;                  // precipitation
  vector[N] year;                    // year
  vector[N] offset;                  // population size offset

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

  // Random Effects, Non-Centered Parameterization, see Section 21.7 in the Stan Users Guide

  real<lower=0> sigma_alpha_lambda;      // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;        // deviates for each place drawn from sigma_alpha (count)

  real<lower=0> sigma_year_lambda;       // estimated variation among locations in slopes (count)
  vector[N_loc] eps_year_lambda;         // deviates for each place drawn from the sigma_beta values (count)

}

transformed parameters {

  real alpha_lambda[N_loc];             // location-specific intercepts for counts
  real year_lambda[N_loc];              // location-specific slope over year for count

  real<lower=0, upper=1> theta[N];      // linear predictor for probability piece
  real lambda[N];                       // linear predictor for count piece

for (i in 1:N_loc) {

  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];
  year_lambda[i] = year_lambda_bar + sigma_year_lambda * eps_year_lambda[i];

}    

for (j in 1:N) {

  lambda[j] = alpha_lambda[loc_id[j]] + 
    temp_lambda_bar * temp[j] +
    precip_lambda_bar * precip[j] +
    year_lambda[loc_id[j]] * year[j] + 
    offset[j]; 

  theta[j]      = inv_logit(alpha_theta_bar);

}

}

model {

// priors. Break up the beta's according to some other independent data sources...

// fixed effects
   alpha_theta_bar ~ normal(0, 3);

   alpha_lambda_bar ~ normal(0, 3);
   temp_lambda_bar ~ normal(0, 3);
   year_lambda_bar ~ normal(0, 3);

// random effects
   sigma_alpha_lambda ~ cauchy(0, 5);
   sigma_year_lambda ~ cauchy(0, 5);

   eps_alpha_lambda ~ normal(0, 1);
   eps_year_lambda ~ normal(0, 1);

// modify the likelihood
   for(n in 1:N) {
    
if (y[n] == 0) {
     
      target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),                 
                             bernoulli_lpmf(0 | theta[n])
                           + poisson_log_lpmf(y[n] | lambda[n]));


    } else {

      target += bernoulli_lpmf(0 | theta[n]) + 
                 poisson_log_lpmf(y[n] | lambda[n]);

    }
  }
}

generated quantities{

// for simulating values

}

