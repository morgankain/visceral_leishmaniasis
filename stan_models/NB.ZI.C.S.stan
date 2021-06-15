
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

  int<lower=0> year[N];                // year
  int<lower=0> max_year;               // the largest year index (for the spline)
  
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

}

parameters {

  // ** Fixed Effects **

  // ** Intercepts ** 
  real alpha_theta_bar;                // "grand" (given the presence of a random effect, using this language) intercept for probability piece 
  real alpha_lambda_bar;               // "grand" intercept for count piece

  // ** Slopes (linear terms) **

  // ** probability piece **
  real gdp_theta_bar;                  // all "grand" slopes follow: COVARIATE_MODEL PIECE_bar

  // ** count piece **
  real temp_lambda_bar;               
  real precip_lambda_bar;         
  real pop_lambda_bar;
  real area_lambda_bar;
  real ag_lambda_bar;
  real ndvi_lambda_bar;

  // ** Slopes (higher order terms) **

  // ** count piece **
  real temp_lambda_bar_sq;                 // all higher-order polynomials have the details as the last piece, i.e. XXXX_sq for a squared term
  real precip_lambda_bar_sq;
  real pop_lambda_bar_sq;
  real area_lambda_bar_sq;
  real ndvi_lambda_bar_sq;

  vector[max_year-1] year_deltas_bar;      // Year_deltas shorter than max_years, because the first delta logically has to be 0

  // ** Variance/Dispersion **
  real<lower=0, upper=20> reciprocal_phi;  // putting an upper bound for constraints (20 is _really_ big)

  real<lower=0, upper=10> delta_sigma;     // Ghitza & Gelman used normal(delta[i-1],1) for sampling deltas, but that can lead to overfitting, so use a hyperprior

  // ** Random Effects, Non-Centered Parameterization, see Section 21.7 in the Stan Users Guide **

  // ** intercepts **
  real<lower=0> sigma_alpha_theta;         // estimated variation among locations in the intercept (probability)
  vector[N_loc] eps_alpha_theta;           // deviates for each place drawn from sigma_alpha (probability)

  real<lower=0> sigma_alpha_lambda;        // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;          // deviates for each place drawn from sigma_alpha (count)

  // ** slopes **
  real<lower=0> sigma_year_lambda;       // estimated variation among locations in the year variation (count)
  matrix[N_loc, max_year-1] eps_year_deltas;

}

transformed parameters {

  // ** random effect deviates **
  real alpha_theta[N_loc];                  // location-specific intercepts for probability piece
  real alpha_lambda[N_loc];                 // location-specific intercepts for counts

  // ** linear predictors for probability **
  real<lower=0, upper=1> theta[N];          // linear predictor for probability piece
  real<lower=0, upper=1> theta_out[N_out];  // linear predictor for probability piece

  real inv_theta[N];                        // linear predictor for probability piece
  real inv_theta_out[N_out];                // linear predictor for probability piece

  // ** linear predictors for count **
  real lambda_log[N];                       // linear predictor for count piece
  real lambda_log_out[N_out];               // linear predictor for count piece for out of sample predictions

  // ** year smoother setup **
  matrix[N_loc, max_year] year_mu;          // year_mu will be the modeled effect of year on VL count at each year
  vector[max_year] real_deltas_bar;         // real_deltas is just year_deltas with 0 concatenated to the front
  matrix[N_loc, max_year] real_deltas_eps;  // random deviates

  // ** dispersion, variance terms **
  real phi;                                 // Dispersion of negative binomial

  real_deltas_bar[1] = 0.0;

  for (i in 1:N_loc) {
    real_deltas_eps[i, 1] = 0.0;
  }

  for(i in 1:(max_year-1)) {
    real_deltas_bar[i+1] = year_deltas_bar[i];
    real_deltas_eps[, i+1] = eps_year_deltas[, i];
   }

  // The trick here is to use cumulative sum of deltas (which are the year to year individual smoothing terms)
  for (i in 1:N_loc) {
    year_mu[i, ] = to_row_vector(cumulative_sum(real_deltas_bar + sigma_year_lambda * to_vector(real_deltas_eps[i, ])));  
  }

  // ** Build the linear predictors  **

for (i in 1:N_loc) {

  alpha_theta[i] = alpha_theta_bar + sigma_alpha_theta * eps_alpha_theta[i];
  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];
 
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
    year_mu[loc_id[j], year[j]];  

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
    year_mu[loc_id_out[jj], max_year];                   // Use the last cumulative year smooth for forward projecting; 

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

   alpha_lambda_bar ~ normal(0, 2);

   temp_lambda_bar ~ normal(0, 3);
   precip_lambda_bar ~ normal(0, 3);
   pop_lambda_bar ~ normal(0, 3);
   area_lambda_bar ~ normal(0, 3);
   ag_lambda_bar ~ normal(0, 3);
   ndvi_lambda_bar ~ normal(0, 3);

   temp_lambda_bar_sq ~ normal(0, 1);
   precip_lambda_bar_sq ~ normal(0, 1);
   pop_lambda_bar_sq ~ normal(0, 1);
   area_lambda_bar_sq ~ normal(0, 1);
   ndvi_lambda_bar_sq ~ normal(0, 1);

   // year smoother: The first time_delta should be less constrained than the rest. delta_sigma could be very small,
    // and if so, the subsequent delta values would be constrained to be too close to 0.
    year_deltas_bar[1] ~ normal(0, 3);

    for(i in 2:(max_year-1)) {
      year_deltas_bar[i] ~ normal(year_deltas_bar[i-1], delta_sigma);
    }

// ** Random effects **

   sigma_alpha_theta ~ inv_gamma(3.5, 1);
   eps_alpha_theta ~ normal(0, 1);

   sigma_alpha_lambda ~ inv_gamma(3.5, 1);
   eps_alpha_lambda ~ normal(0, 1);

   sigma_year_lambda ~ inv_gamma(1, 1);

   for (i in 1:N_loc) {
     eps_year_deltas[i, ] ~ normal(0, 1);
   }

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

