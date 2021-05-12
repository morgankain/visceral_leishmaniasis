####
## Bayesian model fit to VL data with the aim of both parameter estimates and forecasting
####

### Model questions to keep in mind:
 ## 1) Will we assume perfect detection?
 ## 2) Which predictors go into the zero-inflated part and which go into the count part?
 ## 3) How are we going to define the zero-inflation piece. 
# -- For example, either a zero-inflated Poisson or a zero-inflated negative binomial likely could fit, but the 
#    coefficient on the zero inflated piece will differ in magnitude. If we want to try and put a biological
#    description to the zero inflated piece (e.g., suitability) vs count piece what we assume about the count
#    distribution will matter about the inference on the (e.g., suitability) piece. This feels as much of 
#    a philosophical question as a fitting question
# -- If we expect some places to just be poorly set up for VL, can we take care of this with some random
#    effects structure in the probability piece?
 ## 3) How do we deal with month? GAM? some form of sin( ) or cos( )?
 ## 4) What random effects can we conceivably fit and which ones are necessary?
 ## 5) For forecasting how will we define uncertainty in our predictors (e.g., temp, population, or tree cover)?
 ## 6) What is the plan to translate sandfly counts into a predictor for the model? Interpolation?
 ## 7) With so many potential predictors, which ones do we actually want?
# -- What functional forms do we want on the predictors that we decide to include?

### Next steps
 ## 1) Rewrite the zero-inflated model to a vectorized form.
# -- Fit the model both ways and make sure the predictions are identical
 ## 2) Fit both a zero-inflated Poisson and a zero-inflated NB to a slightly larger data set
# -- Produce CI on coefficients and CI on predictions
# -- Create a few prediction and diagnostic plots
 ## 3) Clean up the scripts with printing and help for a new user
 ## 4) Get the zero-inflated model fitting with slopes in the zero-inflated piece
 ## 5) Add landscape predictors to the model 
 ## 6) Use non-linear functional forms for temp and a few other predictors.
# -- Potentially play with GAMs
 ## 7) Set up the predicted quantities block for out-of-sample predictions
 ## 8) Fit a model for a large data set
 
####
## Prep the data and predictors
####
debug.stan.model <- TRUE  ## Run a small sample model for debugging purposes
deubg.top_samps  <- 20    ## Include the top X municipalities by cases 
debug.ran_samps  <- 100   ## Include Y random municipalities
set.seed(10003)

####
## Load packages, functions, and data
####
source("packages_functions.R")

## Note: In theory the modeling process could be automated so that the desired covariates could be designated out front
 ## and the covariate matrix built by extracting these named predictors. 
  ## However: the stan model then needs to be written in a much more opaque way, reducing the ease of interpretation
   ## This will maybe be the strategy long term, but for now keeping a more labor entinsive version of a stan model that
    ## has named coefficients

print.predictors <- TRUE ## If true, produce histograms of the predictors

source("VL_stan_data.R")

## MCMC settings
ni <- 2000
nt <- 1
nb <- 1000
nc <- 1

## Building the model up from from the very basics ## 

# "Simple_Poisson"     -- Random intercept only model
# "Simple2_Poisson"    -- Random slope model (in one covariate)
# "Simple2_NB"         -- Random slope model with Negative Binomial distribution
# "Simple2_Poisson.ZI" -- Random slope model but with a Zero-Inflated Negative Binomial distribution
# "Simple2_NB.ZI"      -- Random slope model but with a Zero-Inflated Negative Binomial distribution
#                         -- No predictors yet in the zero-inflated piece of the model
#                         -- Random slopes only in the count distribution                 
# "Simple3_NB.ZI"      -- Simple2_NB.ZI with slopes in the probability piece
# "Simple4_NB.ZI"      -- Simple3_NB.ZI with random intercepts in the probability piece

stan.model_which <- "Simple2_NB.ZI"

## Run the chosen model
source("VL_stan_run.R")

## Take a look at the model diagnostics with the nifty shinystan
launch_shinystan(stan.fit)
