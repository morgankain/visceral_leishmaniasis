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

VL.stan <- VL.year %>% mutate(
    temp   = c(scale(mean_temp  , scale = T))
  , precip = c(scale(mean_precip, scale = T))
  , pop    = c(scale(mean_pop   , scale = T))
  , gdp    = c(scale(mean_GDP   , scale = T))
  , area   = c(scale(mean_area  , scale = T))
  , ag     = c(scale(mean_ag    , scale = T))
  , ndvi   = c(scale(mean_ndvi  , scale = T))
  , year   = c(scale(mean_year  , scale = F))
  )

## Data for forecasting. This section needs some work to better capture the trends of these covaraiates over time.
 ## This current debugging / prep version simply uses (very crudely) the mean in the last three years and the sd over 
  ## the whole period
VL.stan.F_data <- VL.stan %>% group_by(munip_name) %>%  
  filter(year >= 0) %>% 
  summarize(
  temp_out_mean    = mean(temp)
, precip_out_mean  = mean(precip)
, pop_out_mean     = mean(pop)
, gdp_out_mean     = mean(gdp)
, area_out_mean    = mean(area)
, ag_out_mean      = mean(ag)
, ndvi_out_mean    = mean(ndvi)
)

VL.stan.F_data <- cbind(
  VL.stan.F_data
, (
  VL.stan %>% group_by(munip_name) %>% summarize(
  temp_out_sd      = sd(temp)
, precip_out_sd    = sd(precip)
, pop_out_sd       = sd(pop)
, gdp_out_sd       = sd(gdp)
, area_out_sd      = sd(area)
, ag_out_sd        = sd(ag)
, ndvi_out_sd      = sd(ndvi)
)
)
  )

if (stan.model_which == "NB.ZI.F.L") {

stan.data   <- with(
    VL.stan
  , list(
  N              = nrow(VL.stan)
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

stan.data <- c(stan.data
  , with(VL.stan.F_data
    , list(
  temp_out_mean    = temp_out_mean
, precip_out_mean  = precip_out_mean
, pop_out_mean     = pop_out_mean
, gdp_out_mean     = gdp_out_mean
, area_out_mean    = area_out_mean
, ag_out_mean      = ag_out_mean
, ndvi_out_mean    = ndvi_out_mean
, temp_out_sd      = temp_out_sd
, precip_out_sd    = precip_out_sd
, pop_out_sd       = pop_out_sd  + 0.1    ## Not an optimal decision, just wanting to get this up and running
, gdp_out_sd       = gdp_out_sd 
, area_out_sd      = area_out_sd + 0.1
, ag_out_sd        = ag_out_sd
, ndvi_out_sd      = ndvi_out_sd
    )
  ))

stan.data$ag_out_sd[which(stan.data$ag_out_sd == 0)] <- 0.1

stan.data <- c(stan.data
 , list(
    N_out          = nrow(VL.stan.F_data) * 2
  , N_loc_out      = length(unique(VL.stan.F_data$munip_name))
  , loc_id_out     = rep(as.numeric(as.factor(VL.stan.F_data$munip_name)), each = 2)
  , year_out       = rep(c(max(VL.stan$year) + 1, max(VL.stan$year) + 2), nrow(VL.stan.F_data))
  )
)

} else if (stan.model_which == "NB.ZI.F.S") {
  
stan.data   <- with(
    VL.stan
  , list(
  N              = nrow(VL.stan)
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
  
stan.data <- c(stan.data
  , with(VL.stan.F_data
    , list(
  temp_out_mean    = temp_out_mean
, precip_out_mean  = precip_out_mean
, pop_out_mean     = pop_out_mean
, gdp_out_mean     = gdp_out_mean
, area_out_mean    = area_out_mean
, ag_out_mean      = ag_out_mean
, ndvi_out_mean    = ndvi_out_mean
, temp_out_sd      = temp_out_sd
, precip_out_sd    = precip_out_sd
, pop_out_sd       = pop_out_sd  + 0.1    ## Not an optimal decision, just wanting to get this up and running
, gdp_out_sd       = gdp_out_sd 
, area_out_sd      = area_out_sd + 0.1
, ag_out_sd        = ag_out_sd
, ndvi_out_sd      = ndvi_out_sd
    )
  ))

stan.data$ag_out_sd[which(stan.data$ag_out_sd == 0)] <- 0.1

stan.data <- c(stan.data
 , list(
    N_out          = nrow(VL.stan.F_data) * 2
  , N_loc_out      = length(unique(VL.stan.F_data$munip_name))
  , loc_id_out     = rep(as.numeric(as.factor(VL.stan.F_data$munip_name)), each = 2)
  )
)

}
