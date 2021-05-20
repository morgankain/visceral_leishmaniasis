####
## Prep the data for the first set of Stan models using the same predictors
####

## Lots of covariates. First take a look at them:
if (print.predictors) {
  
all.covar <- names(VL)[c(7:25, 28)][-c(8, 12)]

for (i in seq_along(all.covar)) {
  
temp.VL <- VL %>% group_by(munip_name) %>% summarize(mean_val = mean(get(all.covar[i]), na.rm = T)) 
temp.gg <- ggplot(temp.VL, aes(x = mean_val)) + 
  geom_histogram(bins = 100) +
  scale_x_log10() +
  xlab(all.covar[i])
  
temp_name <- paste("temp.gg", i, sep = "_")
assign(temp_name, temp.gg)

}

gridExtra::grid.arrange(
  temp.gg_1, temp.gg_2, temp.gg_3, temp.gg_4, temp.gg_5, temp.gg_6
, temp.gg_7, temp.gg_8, temp.gg_9, temp.gg_10, temp.gg_11, temp.gg_12
, temp.gg_13, temp.gg_14, temp.gg_15, temp.gg_16, temp.gg_17, temp.gg_18, ncol = 5)

}

## Need to decide which of these covariates are relevant to be used in the model:
VL.year <- VL %>% group_by(munip_name, year) %>% 
  summarize(
    vl_cases    = sum(vl_cases)
  , mean_temp   = mean(mean_air_temp)
  , mean_precip = mean(total_precipitation)
  , mean_pop    = mean(log10(human_pop))
  , mean_GDP    = mean(log10(GDP_1000s_R))
    ) %>% filter(munip_name != "") %>%
  mutate(mean_year = as.numeric(year))

## Cant have NA's so drop all of those
VL.year <- VL.year %>% drop_na() %>% ungroup()

## If debugging run the model with only a few locations
if (debug.stan.model) {
top_locs  <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% slice(1:deubg.top_samps) %>% dplyr::select(munip_name) %>% unname() %>% unlist()

rand_locs <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% filter(munip_name %notin% top_locs) %>% dplyr::select(munip_name) %>% unname() %>% 
  unlist() %>% sample(debug.ran_samps)
 
rand_locs <- c(rand_locs, top_locs)

VL.year <- VL.year %>% filter(munip_name %in% rand_locs)

}

VL.stan <- VL.year %>% mutate(
    year   = c(scale(mean_year, scale = F))
  , precip = c(scale(mean_precip, scale = F))
  , temp   = c(scale(mean_temp, scale = F))
  , pop    = c(scale(mean_pop, scale = F))
  , gdp    = c(scale(mean_GDP, scale = F)))

## Stan data requires a list
stan.data   <- with(
    VL.stan
  , list(
  N              = nrow(VL.stan)
, y              = vl_cases
, N_loc          = length(unique(munip_name))
, loc_id         = as.numeric(as.factor(munip_name))
, temp           = temp
, precip         = precip
, year           = year
, pop            = pop
  ))

## Also set up a data frame for the Frequentist comparisons for the simple models
lme4.data <- VL.year %>% 
  filter(munip_name %in% rand_locs) %>% 
  ungroup() %>% 
  mutate(
    year   = c(scale(mean_year, scale = F))
  , precip = c(scale(mean_precip, scale = F))
  , temp   = c(scale(mean_temp, scale = F))
  , pop    = c(scale(mean_pop, scale = F))
  , gdp    = c(scale(mean_GDP, scale = F)))
