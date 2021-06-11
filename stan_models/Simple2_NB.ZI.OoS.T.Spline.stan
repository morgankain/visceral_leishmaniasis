
data {

  // data for the model
  int<lower=0> N;                    // number of observations
  int<lower=0> y[N];                 // VL case counts

  int<lower=0> N_loc;		     // number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	     // ID of each location 

  // out of sample
  int<lower=0> N_out;                // number of observations for out of sample predictions
  int<lower=0> N_loc_out;	     // number of locations where counts are repeated (for the random effect) for out of sample predictions
  int<lower=0> loc_id_out[N_out];    // ID of each location for out of sample predictions
    
  // Covariates
  vector[N] temp;                    // temperature
  vector[N] precip;                  // precipitation
  vector[N] pop;                     // population

  int<lower=0> year[N];              // year
  int<lower=0> max_year;             // the largest year index (for the spline)

  // out of sample
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

  // Year smoother terms. See https://jofrhwld.github.io/blog/2014/07/29/nonlinear_stan.html, language here taken from that resource.
  real<lower=0, upper=10> delta_sigma;  // Ghitza & Gelman used normal(delta[i-1],1) for sampling deltas, but that can lead to overfitting, so use a hyperprior
  vector[max_year-1] year_deltas;       // Year_deltas shorter than max_years, because the first delta logically has to be 0

  // Variance/Dispersion
  real<lower=0, upper=20> reciprocal_phi;

  // Random Effects, Non-Centered Parameterization, see Section 21.7 in the Stan Users Guide

  real<lower=0> sigma_alpha_lambda;      // estimated variation among locations in the intercept (count)
  vector[N_loc] eps_alpha_lambda;        // deviates for each place drawn from sigma_alpha (count)

}

transformed parameters {

  real alpha_lambda[N_loc];             // location-specific intercepts for counts

  real<lower=0, upper=1> theta;         // linear predictor for probability piece
  real lambda_log[N];                   // linear predictor for count piece

  real phi;                             // Dispersion

  // out of sample
  real lambda_log_out[N_out];           // linear predictor for count piece for out of sample predictions


  // Year smoother

  // year_mu will be the modeled effect of year on VL count at each year
  vector[max_year] year_mu;
    
  // real_deltas is just year_deltas with 0 concatenated to the front
  vector[max_year] real_deltas;
  real_deltas[1] = 0.0;

  for(i in 1:(max_year-1)) {
    real_deltas[i+1] = year_deltas[i];
   }

  // The trick here is to use cumulative sum of deltas (which are the year to year individual smoothing terms)
  year_mu = cumulative_sum(real_deltas);  


for (i in 1:N_loc) {

  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];

}    

for (j in 1:N) {

  lambda_log[j] = alpha_lambda[loc_id[j]] + 
    temp_lambda_bar * temp[j] +
    precip_lambda_bar * precip[j] +
    year_mu[year[j]] +
    pop_lambda_bar * pop[j];  

}

for (jj in 1:N_out) {

  lambda_log_out[jj] = alpha_lambda[loc_id_out[jj]] + 
    temp_lambda_bar * temp_out[jj] +
    precip_lambda_bar * precip_out[jj] +
    year_mu[max_year] +                                       // Use the last cumulative year smooth for forward projecting
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

   eps_alpha_lambda ~ normal(0, 1);

   reciprocal_phi ~ inv_gamma(1, 1);


// year smoother

   // The first time_delta should be less constrained than the rest. delta_sigma could be very small,
    // and if so, the subsequent delta values would be constrained to be too close to 0.
    year_deltas[1] ~ normal(0, 5);

    for(i in 2:(max_year-1)) {
      year_deltas[i] ~ normal(year_deltas[i-1], delta_sigma);
    }


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

