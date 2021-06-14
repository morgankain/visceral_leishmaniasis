#########################################################
## Bayesian model fit to VL data with the aim of both: ##
##  -- Parameter estimation                            ##
##  -- Short-term forecasting (two years out)          ## 
#########################################################

####
## Model questions that remain unresolved:
####
 ## 1) Will we assume perfect detection?
#    -- The model is set up to (sort of) address this as it is a zero-inflated model, however the zero-inflation bit
#       so far is being used as a form of "suitability"
 ## 2) How should the zero-inflation piece be defined?
#    -- For example, either a zero-inflated Poisson or a zero-inflated Negative Binomial likely could fit, but the 
#       coefficient on the zero inflated piece will differ in magnitude depending on the option. 
#       Conceivably some classic measure of statistical fit could be used to compare (wAIC or w/e).
#       But, there may be just as much of a priori biological choice for inference about 
#       suitability (zero inflated piece) vs rarity (suitable but zero in the count piece)
 ## 3) Which predictors should go into the zero-inflated part and which go into the count part?
#    -- Do we consider a priori from a biological rationale the predictors that go into the zero vs non-zero parts?
#    -- Should the same predictors enter both parts of the model?
#    -- If we expect some places to be poorly suited for VL, can we take care of this with some random
#       effects structure in the probability piece?
 ## 4) Focus on patterns over years or over months?
#    -- So far the model takes the sum over months to just predict yearly counts as modeling seasonality is whole new can of worms
#    -- If over months how to deal with the wigglyness over month? GAM? some form of sin( ) or cos( )?
 ## 5) With so many potential predictors, which ones do we actually want?
#    -- What functional forms do we want on the predictors that we decide to include?
#    -- What random effects can we conceivably fit and which ones are necessary?
#    -- What is the plan to translate sandfly counts into a predictor for the model? Interpolation? So far this hasn't been in the model
 ## 6) For forecasting how will we define uncertainty in our predictors (e.g., temp, population, or tree cover)?
#    -- So far I am using a simplistic version of the mean and sd in that predictor in the last couple of years and converting
#       the covariates into "parameters" that have a prior (with the defined mean and sd) but are not estimated 

####
## Notes about these scripts:
####
 ## 1) This modeling exercise is set up in a way like an exposition-light model building tutorial in that 
#      the model is constructed from the ground up with all intermediate models saved.
 ## 2) In theory the modeling process could be automated so that the desired covariates could be 
#      designated out front and the model matrix built by extracting these named predictors
#      HOWEVER: the model then needs to be written in a much more opaque way, reducing the ease of interpretation,
#      which I am putting a particularly large emphasis on in case this project gets handed off to someone else and
#      because of the "tutorial" structure
#    -- A model matrix adaptive approach may be a more viable strategy long term (as it is a pain in the current form to add predictors)
#       but for now keeping a more labor intensive version of the model that has named coefficients

####
## Load packages, functions, and data
####
inst_miss_pack <- FALSE         ## Bulk install all needed missing packages (TRUE) or just print those not installed (FALSE)
source("packages_functions.R")  ## Load packages, helper functions, and data

####
## Prep the data and predictors
####

## Run a small sample model with only a subset of the data for debugging and development purposes? 
debug.stan.model <- TRUE  
## If TRUE:
deubg.top_samps  <- 50    ## Include the top X municipalities by cases 
debug.ran_samps  <- 100   ## Include Y random municipalities
set.seed(10003)           ## Set a seed to pick these random municipalities for reproducibility
## MCMC settings
ni <- 1500                ## number of iterations per chain. 1500 is what I have been using for debug and development
                           ## For the final version this should probably be around 12,000
nt <- 1                   ## thinning (1 = keep all samples). NOTE: there is definitely some auto-correlation in the later models
                           ## For the final version this should probably be at least 5
nb <- 500                 ## burn-in (number of initial samples to throw out)
                           ## For the final verison this should probably be about 2000, with nt = 5 and ni = 12,000
nc <- 1                   ## number of chains (3 or 4 is usually sensible, 1 is ok for quick debugging)

####
## Building the model up from from the very basics
####

## Three stages of model development (all models described in detail below):
 ## 1) "construct"    -- early model iterations with few predictors and simple structure
 ## 2) "finalize"     -- more complicated models designed to fit to N - 2 years and predict the last two years of data
 ## 3) "forecast"     -- the most complicated/extensive of the "fit" models expanded to allow forecasting into the future 
model.form <- "forecast"

## NOTE" Each subsequent model incorporates all (or most) of the model additions from the previous model

## This first set of models (non-zero inflated) that are compared to Frequentist models with canned packages (to compare predictions)
if (model.form == "construct") { 
  
# "Simple_Poisson"     -- Random intercept only model
# "Simple2_Poisson"    -- Random slopes model (one covariate)
# "Simple2_NB"         -- ^^ + Negative Binomial distribution
# "Simple2_Poisson.ZI" -- Random slopes model but with a Zero-Inflated Poisson distribution
# "Simple2_NB.ZI"      -- Random slopes model but with a Zero-Inflated Negative Binomial distribution
# "Simple2_NB.ZI.OoS.S"          -- ^^ + Out of sample over space (a location not included in the fitted model (first))
#                                   Not expected to perform very well because of the large variance in the random effect of location
# "Simple2_NB.ZI.OoS.T"          -- ^^ + Out of sample over time (the next two years for all locations fit)
#                                   Expected to perform better than ^^ because these locations have their conditional mode estimated
# "Simple2_NB.ZI.OoS.T.spline"   -- ^^ + Replaced linear term over year with a spline
# "Simple2_NB.ZI.OoS.T.spline.R" -- ^^ + Allowing the smoothing terms to vary by location in a random-effects form

## Choose the model to run
stan.model_which <- "Simple2_NB.ZI.OoS.T.spline.R"

## If true, produce histograms of the predictors
print.predictors <- FALSE          

## Set up the data into the structure needed for the stan model for any of these models 
source("VL_stan_data_construct.R") 

## Run the chosen model
source("VL_stan_construct.R")

## Continue to expand the above models with more predictors 
} else if (model.form == "finalize") {
  
## NOTE: These keep the out-of-sample predictions for the last two years from "Simple2_NB.ZI.OoS.T" above
#        as well as the linear term over years from that model. The spline is reintroduced later
# "Simple3_NB.ZI" -- Simple2_NB.ZI with a slope in the probability piece and quadratic term for temperature 
# "Simple4_NB.ZI" -- ^^ + Random intercepts in the probability piece
# "NB.ZI.f"       -- ^^ + A series of other predictors
# "NB.ZI.C.L"     -- ^^ + Lots of predictors, sensible functional forms etc. 
# "NB.ZI.C.S"     -- ^^ But with the random splines instead of the linear term for year

stan.model_which <- "NB.ZI.C.S"

## Set up the data into the structure needed for these stan models. 
#  The data setup required for this set of models is quite different than for the first set so using a different script
source("VL_stan_data_finalize.R") 

## Run the chosen model
source("VL_stan_finalize.R")

## Finally, use the most expansive models from the set above (NB.ZI.C.L and NB.ZI.C.S) to forecast 
} else if (model.form == "forecast") {

source("VL_stan_data_forecast.R")  
source("VL_stan_forecast.R")

} else {
  
print("Choose one of the three correct options")
  
}



