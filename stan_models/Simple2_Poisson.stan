
data {

  int<lower=0> N;                    // number of observations
  int<lower=0> y[N];                 // VL case counts

  int<lower=0> N_loc;		     // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	     // ID of each location 
    
  vector[N] temp;                    // temperature
  vector[N] precip;                  // precipitation
  vector[N] year;                    // year
  vector[N] pop;                     // population size 

 }

parameters {

  real alpha_lambda_bar;                 // intercept for count piece  
  real temp_lambda_bar;                  // slope coefficients for probability piece
  real precip_lambda_bar;       
  real year_lambda_bar;  
  real pop_lambda_bar;     

// random effect structure  

  real<lower=0> sigma_alpha_lambda;      // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;        // deviates for each place drawn from sigma_alpha (count)

  real<lower=0> sigma_year_lambda;       // estimated variation among locations in slopes (count)
  vector[N_loc] eps_year_lambda;         // deviates for each place drawn from the sigma_beta values (count)

}

transformed parameters {

  real lambda_log[N];                   // linear predictor for count piece

  real alpha_lambda[N_loc];             // location-specific intercepts for counts
  real year_lambda[N_loc];

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

}

model {

// priors. Break up the beta's according to some other independent data sources...

// fixed effects
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

// modify the likelihood
   for(n in 1:N) {
         
   target += poisson_log_lpmf(y[n] | lambda_log[n]);

}

}


generated quantities {


}

