##############################################################################################################
## Bayesian model fit to VL data with the aim of both parameter estimates and forecasting (forward in time) ##
##############################################################################################################

####
## Model questions to keep in mind:
####
 ## 1) Assume perfect detection?
 ## 2) How should the zero-inflation piece be defined?
# -- For example, either a zero-inflated Poisson or a zero-inflated Negative Binomial likely could fit, but the 
#    coefficient on the zero inflated piece will differ in magnitude depending on the option. 
#    Conceivably some classic measure of statistical fit could be used to compare (wAIC or w/e).
#    But, there may be just as much of an important a priori biological choice for inference about 
#    suitability (zero inflated piece) vs rarity (suitable but zero in the count piece)
 ## 3) Which predictors go into the zero-inflated part and which go into the count part?
# -- If we expect some places to be poorly set up for VL, can we take care of this with some random
#    effects structure in the probability piece?
 ## 4) Focus on patterns over years or over months?
# -- If over months how to deal with the wigglyness over month? GAM? some form of sin( ) or cos( )?
 ## 5) With so many potential predictors, which ones do we actually want?
# -- What functional forms do we want on the predictors that we decide to include?
# -- What random effects can we conceivably fit and which ones are necessary?
# -- What is the plan to translate sandfly counts into a predictor for the model? Interpolation?
 ## 6) For forecasting how will we define uncertainty in our predictors (e.g., temp, population, or tree cover)?

####
## Next steps
####
 ## 1) Get the zero-inflated model fitting with slopes in the zero-inflated piece
 ## 2) Use non-linear functional forms for temp and a few other predictors
# -- Potentially play with GAMs
 ## 3) Expand the set of predictors considered in the model 
 ## 4) Fit a model for a large data set
 
####
## Notes
####
## 1) In theory the modeling process could be automated so that the desired covariates could be 
 ## designated out front and the covariate matrix built by extracting these named predictors
## HOWEVER: the model then needs to be written in a much more opaque way, reducing the ease of interpretation
 ## This may be a more viable strategy long term (as it is a pain in the current form to add predictors)
  ## but for now keeping a more labor intensive version of the model that has named coefficients

####
## Prep the data and predictors
####
## Run a small sample model for debugging purposes? 
debug.stan.model <- TRUE  
## If TRUE:
deubg.top_samps  <- 50    ## Include the top X municipalities by cases 
debug.ran_samps  <- 100   ## Include Y random municipalities
set.seed(10003)           ## Set a seed to pick these random municipalities for reproducibility

####
## Load packages, functions, and data
####
inst_miss_pack <- FALSE         ## Bulk install all needed missing packages (TRUE) or just print those not installed (FALSE)
source("packages_functions.R")  ## Load packages, helper functions, and data

## MCMC settings
ni <- 1500   ## number of iterations per chain
nt <- 1      ## thinning (1 = keep all samples)
nb <- 500    ## burn-in (number of initial samples to throw out)
nc <- 1      ## number of chains (3 or 4 is usually sensible, 1 is ok for quick debugging)

####
## Building the model up from from the very basics
####

model.form <- "forecast" ## setup, fit, or forecast

## Each subsequent grouping incorporates all of the material from the previous steps

if (model.form == "setup") {
  
## This first set of models (non-zero inflated) also fit Frequentist models with canned packages to compare predictions 
# "Simple_Poisson"     -- Random intercept only model
# "Simple2_Poisson"    -- Random slope model (in one covariate)
# "Simple2_NB"         -- Random slope model with Negative Binomial distribution

## The second set fit simple zero-inflated models. 
## These also start incorporating generated quantities to compare predicted values ##
# "Simple2_Poisson.ZI" -- Random slope model but with a Zero-Inflated Negative Binomial distribution
# "Simple2_NB.ZI"      -- Random slope model but with a Zero-Inflated Negative Binomial distribution
#                         -- No predictors yet in the zero-inflated piece of the model
#                         -- Random slopes only in the count distribution 

## Third set includes out-of-sample predictions in two ways
## A location not included in the fitted model (first). Expected to perform only ok because of the random effect of location
## Two years for locations that are included in the model (second). Expected to perform better than ^^ because these locations have
## their conditional mode estimated
# "Simple2_NB.ZI.OoS.S"          -- Out of sample over space
# "Simple2_NB.ZI.OoS.T"          -- Out of sample over time
# "Simple2_NB.ZI.OoS.T.spline"   -- Replace the linear term over year with a smoothing term
# "Simple2_NB.ZI.OoS.T.spline.R" -- Allow the smoothing terms to vary by location

stan.model_which <- "Simple2_NB.ZI.OoS.T"

print.predictors <- FALSE          ## If true, produce histograms of the predictors
## Set up the data into the structure needed for the stan model for these models working on building the structure
source("VL_stan_data_construct.R") 

## Run the chosen model
source("VL_stan_construct.R")

} else if (model.form == "fit") {
  
## Fourth set explores adding predictors to the zero-inflated section of the model
## and starts to explore quadratic terms as well
## These keep the out-of-sample predictions for the last two years from "Simple2_NB.ZI.OoS.T" above
# "Simple3_NB.ZI"      -- Simple2_NB.ZI with a slope in the probability piece and quadratic term to temperature 
# "Simple4_NB.ZI"      -- Simple3_NB.ZI with random intercepts in the probability piece

## Fifth set starts exploring more robust functional forms for the predictors and adding many predictors
# "NB.ZI.f1"            -- Starts adding a series of other predictors

## Sixth set are _hypothetically_ complete models
# "NB.ZI.C1"            -- Lots of predictors, sensible functional forms etc. 
# "NB.ZI.C2"            -- Random spline smoother for year instead of the linear random effect for year

stan.model_which <- "NB.ZI.C2"

## Set up the data into the structure needed for these stan models. The data required will start to vary a lot for these
 ## models so using a new data script
source("VL_stan_data_finalize.R") 

## Run the chosen model
source("VL_stan_finalize.R")

} else if (model.form == "forecast") {

## use the previous models 
source("VL_stan_data_forecast.R")  
source("VL_stan_forecast.R")

} else {
  
print("Choose one of the three correct options")
  
}



