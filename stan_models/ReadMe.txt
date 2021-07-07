## List of all Stan models contained within this folder


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

## NOTE: These keep the out-of-sample predictions for the last two years from "Simple2_NB.ZI.OoS.T" above
#        as well as the linear term over years from that model. The spline is reintroduced later
# "Simple3_NB.ZI" -- Simple2_NB.ZI with a slope in the probability piece and quadratic term for temperature 
# "Simple4_NB.ZI" -- ^^ + Random intercepts in the probability piece
# "NB.ZI.f"       -- ^^ + A series of other predictors
# "NB.ZI.C.L"     -- ^^ + Lots of predictors, sensible functional forms etc. 
# "NB.ZI.C.S"     -- ^^ But with the random splines instead of the linear term for year

# "NB.ZI.F.S"        -- Forecast with a spline term for year
# "NB.ZI.F.L"        -- Forecast with a linear term for year
# "NB.ZI.F.L.Panel"  -- Forecast with a linear term for year without the random effect specification that leads to partial pooling (fixed effects estimation method)