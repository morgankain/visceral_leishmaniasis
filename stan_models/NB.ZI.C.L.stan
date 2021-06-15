
data {

  // ** In sample bookkeeping **
  int<lower=0> N;                      // number of observations
  int<lower=0> y[N];                   // VL case counts
  int<lower=0> N_loc;		       // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	       // ID of each location 

  // ** In sample covariates ** 
  vector[N] temp;                      // temperature
  vector[N] precip;                    // precipitation
  vector[N] pop;                       // population
  vector[N] gdp;                       // gdp
  vector[N] area;                      // area
  vector[N] ag;                        // ag
  vector[N] ndvi;                      // ndvi
  vector[N] year;                      // year
  
  // ** Out of Sample bookkeeping
  int<lower=0> N_out;                  // number of observations for out of sample predictions
  int<lower=0> N_loc_out;	       // number of locations where counts are repeated (for the random effect) for out of sample predictions
  int<lower=0> loc_id_out[N_out];      // ID of each location for out of sample predictions
    
  // ** Out of Sample covariates
  vector[N_out] temp_out;              // temperature
  vector[N_out] precip_out;            // precipitation
  vector[N_out] pop_out;               // population
  vector[N_out] gdp_out;               // gdp
  vector[N_out] area_out;              // area
  vector[N_out] ag_out;                // ag
  vector[N_out] ndvi_out;              // ndvi
  vector[N_out] year_out;              // year

}

parameters {

  // ** Fixed Effects **

  // ** Intercepts ** 
  real alpha_theta_bar;                   // "grand" (given the presence of a random effect, using this language) intercept for probability piece 
  real alpha_lambda_bar;                  // "grand" intercept for count piece

  // ** Slopes (linear terms) **

  // ** probability piece **
  real gdp_theta_bar;                     // all "grand" slopes follow: COVARIATE_MODEL PIECE_bar

  // ** count piece **
  real temp_lambda_bar;               
  real precip_lambda_bar;         
  real pop_lambda_bar;
  real area_lambda_bar;
  real ag_lambda_bar;
  real ndvi_lambda_bar;
  real year_lambda_bar;

  // ** Slopes (higher order terms) **

  // ** count piece **
  real temp_lambda_bar_sq;                // all higher-order polynomials have the details as the last piece, i.e. XXXX_sq for a squared term
  real precip_lambda_bar_sq;
  real pop_lambda_bar_sq;
  real area_lambda_bar_sq;
  real ndvi_lambda_bar_sq;

  // ** Variance/Dispersion **
  real<lower=0, upper=20> reciprocal_phi; // putting an upper bound for constraints (20 is _really_ big)


  // ** Random Effects, Non-Centered Parameterization, see Section 21.7 in the Stan Users Guide **

  // ** intercepts **
  real<lower=0> sigma_alpha_theta;         // estimated variation among locations in the intercept (probability)
  vector[N_loc] eps_alpha_theta;           // deviates for each place drawn from sigma_alpha (probability)

  real<lower=0> sigma_alpha_lambda;        // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;          // deviates for each place drawn from sigma_alpha (count)

  // ** slopes **
  real<lower=0> sigma_year_lambda;         // estimated variation among locations in slopes (count)
  vector[N_loc] eps_year_lambda;           // deviates for each place drawn from the sigma_beta values (count)

}

transformed parameters {

  // ** random effect deviates **
  real alpha_theta[N_loc];                  // location-specific intercepts for probability piece
  real alpha_lambda[N_loc];                 // location-specific intercepts for counts
  real year_lambda[N_loc];                  // location-specific slope over year for count

  // ** linear predictors for probability **
  real<lower=0, upper=1> theta[N];          // linear predictor for probability piece
  real<lower=0, upper=1> theta_out[N_out];  // linear predictor for probability piece

  real inv_theta[N];                        // linear predictor for probability piece
  real inv_theta_out[N_out];                // linear predictor for probability piece

  // ** linear predictors for count **
  real lambda_log[N];                       // linear predictor for count piece
  real lambda_log_out[N_out];               // linear predictor for count piece for out of sample predictions

  real phi;                                 // Dispersion of negative binomial


  // ** Build the linear predictors  **

for (i in 1:N_loc) {

  alpha_theta[i] = alpha_theta_bar + sigma_alpha_theta * eps_alpha_theta[i];
  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];
  year_lambda[i] = year_lambda_bar + sigma_year_lambda * eps_year_lambda[i];
 
}    

  // ** in sample (fitting) ** 

for (j in 1:N) {

  lambda_log[j] = alpha_lambda[loc_id[j]] + 
    temp_lambda_bar * temp[j] +
    temp_lambda_bar_sq * square(temp[j]) +
    precip_lambda_bar * precip[j] +
    precip_lambda_bar_sq * square(precip[j]) +
    pop_lambda_bar * pop[j] +
    pop_lambda_bar_sq * square(pop[j]) +
    area_lambda_bar * area[j] +
    area_lambda_bar_sq * square(area[j]) +
    ag_lambda_bar * ag[j] +
    ndvi_lambda_bar * ndvi[j] +
    ndvi_lambda_bar_sq * square(ndvi[j]) +
    year_lambda[loc_id[j]] * year[j];  

  inv_theta[j] = alpha_theta[loc_id[j]] + gdp_theta_bar * gdp[j];

}

  theta = inv_logit(inv_theta);

  // ** out of sample (predicting) ** 

for (jj in 1:N_out) {

  lambda_log_out[jj] = alpha_lambda[loc_id_out[jj]] + 
    temp_lambda_bar * temp_out[jj] +
    temp_lambda_bar_sq * square(temp_out[jj]) +
    precip_lambda_bar * precip_out[jj] +
    precip_lambda_bar_sq * square(precip_out[jj]) +
    pop_lambda_bar * pop_out[jj] +
    pop_lambda_bar_sq * square(pop_out[jj]) +
    area_lambda_bar * area_out[jj] +
    area_lambda_bar_sq * square(area_out[jj]) +
    ag_lambda_bar * ag_out[jj] +
    ndvi_lambda_bar * ndvi_out[jj] +
    ndvi_lambda_bar_sq * square(ndvi_out[jj]) +
    year_lambda[loc_id_out[jj]] * year_out[jj]; 

  inv_theta_out[jj] = alpha_theta[loc_id_out[jj]] + gdp_theta_bar * gdp_out[jj];

}

  theta_out = inv_logit(inv_theta_out);

  phi = 1. / reciprocal_phi;

}

model {

// ** Priors. ** 

// ** Fixed effects **
   alpha_theta_bar ~ normal(0, 3);
   gdp_theta_bar ~ normal(0, 3);

   alpha_lambda_bar ~ normal(0, 3);

   temp_lambda_bar ~ normal(0, 3);
   precip_lambda_bar ~ normal(0, 3);
   pop_lambda_bar ~ normal(0, 3);
   area_lambda_bar ~ normal(0, 3);
   ag_lambda_bar ~ normal(0, 3);
   ndvi_lambda_bar ~ normal(0, 3);
   year_lambda_bar ~ normal(0, 3);

   temp_lambda_bar_sq ~ normal(0, 3);
   precip_lambda_bar_sq ~ normal(0, 3);
   pop_lambda_bar_sq ~ normal(0, 3);
   area_lambda_bar_sq ~ normal(0, 3);
   ndvi_lambda_bar_sq ~ normal(0, 3);

// ** Random effects **

   sigma_alpha_theta ~ inv_gamma(3.5, 1);
   eps_alpha_theta ~ normal(0, 1);

   sigma_alpha_lambda ~ inv_gamma(3.5, 1);
   eps_alpha_lambda ~ normal(0, 1);

   sigma_year_lambda ~ inv_gamma(3.5, 1);
   eps_year_lambda ~ normal(0, 1);

   reciprocal_phi ~ inv_gamma(3.5, 1);


// ** modify the likelihood **
   
for(n in 1:N) {
    
if (y[n] == 0) {
     
      target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),                 
                             bernoulli_lpmf(0 | theta[n])
                           + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi));


    } else {

      target += bernoulli_lpmf(0 | theta[n])                              
                  + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi);

    }
  }
}

generated quantities {

// ** Estimates for both in- and out-of-sample predictions

 int<lower=0> y_sim[N];
 int zero_pred;
 real mu[N];

 int<lower=0> y_sim_out[N_out];
 int zero_pred_out;
 real mu_out[N_out];

 mu = exp(lambda_log);
 mu_out = exp(lambda_log_out);

// ** In sample predictions ** 

 for(n in 1:N) {

  zero_pred    = bernoulli_rng(theta[n]); 
  y_sim[n]     = (1 - zero_pred) * neg_binomial_2_rng(mu[n], phi);

  }

// ** Out of sample predictions ** 

 for(nn in 1:N_out) {

  zero_pred_out = bernoulli_rng(theta_out[nn]); 
  y_sim_out[nn] = (1 - zero_pred_out) * neg_binomial_2_rng(mu_out[nn], phi);

  }

}

