
data {

  int<lower=0> N;                    // number of observations
  int<lower=0> y[N];                 // VL case counts

  int<lower=0> N_loc;		     // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	     // ID of each location 
    
  vector[N] temp;                    // temperature
  vector[N] precip;                  // precipitation
  vector[N] offset;                  // population size offset

}

parameters {

  real alpha_lambda_bar;                 // intercept for count piece  
  real temp_lambda_bar;                  // slope coefficients for probability piece
  real precip_lambda_bar;             

  real<lower=0> reciprocal_phi;

// random effect structure  
  real<lower=0> sigma_alpha_lambda;      // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;        // deviates for each place drawn from sigma_alpha (count)

  real<lower=0> sigma_temp_lambda;       // estimated variation among locations in slopes (count)
  real<lower=0> sigma_precip_lambda;     

  vector[N_loc] eps_temp_lambda;         // deviates for each place drawn from the sigma_beta values (count)
  vector[N_loc] eps_precip_lambda;

}

transformed parameters {

  vector[N] lambda_log;              // linear predictor for count piece
  real<lower=0> phi;                 // dispersion

  real alpha_lambda[N_loc];            // location-specific intercepts for counts
  real temp_lambda[N_loc];
  real precip_lambda[N_loc];

for (i in 1:N_loc) {

  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];
  temp_lambda[i] = temp_lambda_bar + sigma_temp_lambda * eps_temp_lambda[i];
  precip_lambda[i] = precip_lambda_bar + sigma_precip_lambda * eps_precip_lambda[i];

}    


for (j in 1:N) {

  lambda_log[j] = alpha_lambda[loc_id[j]] + 
    temp_lambda[loc_id[j]] * temp[j] + 
    precip_lambda[loc_id[j]] * precip[j] + 
    offset[j]; 

}

  phi = 1. / reciprocal_phi;

}

model {

// priors. Break up the beta's according to some other independent data sources...

// fixed effects
   alpha_lambda_bar ~ normal(0, 3);
   temp_lambda_bar ~ normal(0, 3);
   precip_lambda_bar ~ normal(0, 3);

// random effects
   sigma_alpha_lambda ~ cauchy(0, 5);
   sigma_temp_lambda ~ cauchy(0, 2);
   sigma_precip_lambda ~ cauchy(0, 2);

   eps_alpha_lambda ~ normal(0, 1);
   eps_temp_lambda ~ normal(0, 1);
   eps_precip_lambda ~ normal(0, 1);

// var
   reciprocal_phi ~ cauchy(0., 5);

// modify the likelihood
   for(n in 1:N) {
         
    target += neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi);

}

}


generated quantities {

// for simulating values

}

