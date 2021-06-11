####
## Prep the data for the later more complicated stan models that start adding complexity
####

## If debugging run the model with only a few locations
if (debug.stan.model) {
top_locs  <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% slice(1:deubg.top_samps) %>% dplyr::select(munip_name) %>% unname() %>% unlist()

rand_locs <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% filter(munip_name %notin% top_locs) %>% dplyr::select(munip_name) %>% unname() %>% 
  unlist() %>% sample(debug.ran_samps)
 
rand_locs <- c(rand_locs, top_locs)

VL.temp <- VL %>% filter(munip_name %in% rand_locs)
}

## temp   --- no log, quadratic
## precip --- log10, quadratic
## pop    --- log10, quadratic
## gdp    --- log10, quadratic
## area   --- log10, quadratic
## ag     --- no log, linear
## ndvi   --- no log, quadratic

## Need to decide which of these covariates are relevant to be used in the model:
VL.year <- VL.temp %>% group_by(munip_name, year) %>% 
  summarize(
    vl_cases     = sum(vl_cases)
  , mean_temp    = mean(mean_air_temp)
  , mean_precip  = mean(total_precipitation)
  , mean_pop     = mean(log10(human_pop))
  , mean_GDP     = mean(log10(GDP_1000s_R))
  , mean_area    = mean(log10(AREA_KM2))
  , mean_ag      = mean(Agriculture.2)
  , mean_ndvi    = mean(median_ndvi)
# , mean_ibg     = mean(CD_MUN_ibg)
    ) %>% filter(munip_name != "") %>%
  mutate(mean_year = as.numeric(year)) %>%
  drop_na() %>% ungroup()

## Problem if there is zero precip, so set locations with zero to the min observed 
if (length(VL.year[VL.year$mean_precip == 0, ]$mean_precip) > 0) {
 VL.year[VL.year$mean_precip == 0, ]$mean_precip <- min(VL.year[VL.year$mean_precip > 0, ]$mean_precip)
}

VL.year <- VL.year %>% mutate(mean_precip = log10(mean_precip))

VL.year.in  <- VL.year %>% filter(mean_year < 2014)
VL.year.out <- VL.year %>% filter(mean_year > 2013)

VL.stan.in <- VL.year.in %>% mutate(
    temp   = c(scale(mean_temp  , scale = T))
  , precip = c(scale(mean_precip, scale = T))
  , pop    = c(scale(mean_pop   , scale = T))
  , gdp    = c(scale(mean_GDP   , scale = T))
  , area   = c(scale(mean_area  , scale = T))
  , ag     = c(scale(mean_ag    , scale = T))
  , ndvi   = c(scale(mean_ndvi  , scale = T))
  , year   = c(scale(mean_year  , scale = F))
  )

## Data for fitting

if (stan.model_which != "NB.ZI.C2") {

stan.data   <- with(
    VL.stan.in
  , list(
  N              = nrow(VL.stan.in)
, y              = vl_cases
, N_loc          = length(unique(munip_name))
, loc_id         = as.numeric(as.factor(munip_name))
, temp           = temp
, precip         = precip
, pop            = pop
, gdp            = gdp
, area           = area
, ag             = ag
, ndvi           = ndvi
, year           = year
  ))

## Add data for out-of-sample predictions
stan.data <- c(
  stan.data
, N_out          = nrow(VL.year.out)
, N_loc_out      = length(unique(VL.year.out$munip_name))
, loc_id_out     = list(as.numeric(as.factor(VL.year.out$munip_name)))
, temp_out       = list(c(VL.year.out$mean_temp   - mean(VL.stan.in$mean_temp))   / sd(VL.stan.in$mean_temp))
, precip_out     = list(c(VL.year.out$mean_precip - mean(VL.stan.in$mean_precip)) / sd(VL.stan.in$mean_precip))
, pop_out        = list(c(VL.year.out$mean_pop    - mean(VL.stan.in$mean_pop))    / sd(VL.stan.in$mean_pop))
, gdp_out        = list(c(VL.year.out$mean_GDP    - mean(VL.stan.in$mean_GDP))    / sd(VL.stan.in$mean_GDP))
, area_out       = list(c(VL.year.out$mean_area   - mean(VL.stan.in$mean_area))   / sd(VL.stan.in$mean_area))
, ag_out         = list(c(VL.year.out$mean_ag     - mean(VL.stan.in$mean_ag))     / sd(VL.stan.in$mean_ag))
, ndvi_out       = list(c(VL.year.out$mean_ndvi   - mean(VL.stan.in$mean_ndvi))   / sd(VL.stan.in$mean_ndvi))
, year_out       = list(c(VL.year.out$mean_year   - mean(VL.stan.in$mean_year)))
)

} else {
  
stan.data   <- with(
    VL.stan.in
  , list(
  N              = nrow(VL.stan.in)
, y              = vl_cases
, N_loc          = length(unique(munip_name))
, loc_id         = as.numeric(as.factor(munip_name))
, temp           = temp
, precip         = precip
, pop            = pop
, gdp            = gdp
, area           = area
, ag             = ag
, ndvi           = ndvi
, year           = year + abs(min(year)) + 1
, max_year       = max(year + abs(min(year)) + 1)
  ))

## Add data for out-of-sample predictions
stan.data <- c(
  stan.data
, N_out          = nrow(VL.year.out)
, N_loc_out      = length(unique(VL.year.out$munip_name))
, loc_id_out     = list(as.numeric(as.factor(VL.year.out$munip_name)))
, temp_out       = list(c(VL.year.out$mean_temp   - mean(VL.stan.in$mean_temp))   / sd(VL.stan.in$mean_temp))
, precip_out     = list(c(VL.year.out$mean_precip - mean(VL.stan.in$mean_precip)) / sd(VL.stan.in$mean_precip))
, pop_out        = list(c(VL.year.out$mean_pop    - mean(VL.stan.in$mean_pop))    / sd(VL.stan.in$mean_pop))
, gdp_out        = list(c(VL.year.out$mean_GDP    - mean(VL.stan.in$mean_GDP))    / sd(VL.stan.in$mean_GDP))
, area_out       = list(c(VL.year.out$mean_area   - mean(VL.stan.in$mean_area))   / sd(VL.stan.in$mean_area))
, ag_out         = list(c(VL.year.out$mean_ag     - mean(VL.stan.in$mean_ag))     / sd(VL.stan.in$mean_ag))
, ndvi_out       = list(c(VL.year.out$mean_ndvi   - mean(VL.stan.in$mean_ndvi))   / sd(VL.stan.in$mean_ndvi))
, year_out       = list(c(VL.year.out$mean_year   - mean(VL.stan.in$mean_year))   + max(VL.stan.in$year))
)
  
}
