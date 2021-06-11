####################################################################################
## First try and fit some mixed models to the VL data as a first pass exploration ##
####################################################################################

## First try just over years, ignoring the monthly seasonality
names(VL)

VL.year <- VL %>% group_by(munip_name, year) %>% 
  summarize(
    vl_cases    = sum(vl_cases)
  , mean_temp   = mean(mean_air_temp)
  , mean_precip = mean(total_precipitation)
  , mean_pop    = mean(log10(human_pop))
  , mean_GDP    = mean(log10(GDP_1000s_R))
    ) %>% filter(munip_name != "") %>%
  mutate(year = as.numeric(year))

VL.year <- transform(
  VL.year
  , temp   = scale(mean_temp)
  , precip = scale(mean_precip)
  , pop    = scale(mean_pop)
  , gdp    = scale(mean_GDP)
  )

rand_locs <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% head(20) %>% dplyr::select(munip_name) %>% unname() %>% unlist()

test.nb <- glmer.nb(
  vl_cases ~ year + poly(temp, 2) + precip + pop + gdp + (1 | munip_name)
  , data = (VL.year %>% filter(munip_name %in% rand_locs))
)

test.nb.s <- glmer(
  vl_cases ~ year + poly(temp, 2) + precip + pop + gdp + (1 | munip_name)
  , data = (VL.year %>% filter(munip_name %in% rand_locs))
  , family = "poisson"
)

test.nb.u <- glmer(
  vl_cases ~ year + poly(mean_temp, 2) + mean_precip + mean_pop + mean_GDP + (1 | munip_name)
  , data = (VL.year %>% filter(munip_name %in% rand_locs))
  , family = "poisson"
)

####
## A loose caricature of a mixed model ignoring monthly trends would look like:
####

test.pois.s <- glmer(
  vl_cases ~ year + poly(temp, 2) + precip + pop + poly(gdp, 2) + (1 + year | munip_name)
  , data = (VL.year %>% filter(munip_name %in% rand_locs))
  , family = "poisson"
)

VL.all <-  VL %>% filter(munip_name != "") %>% mutate(year = as.numeric(year), month = as.numeric(month))

VL.all <- transform(
  VL.all
  , temp   = scale(mean_air_temp)
  , precip = scale(total_precipitation)
  , pop    = scale(log10(human_pop))
  , gdp    = scale(log10(GDP_1000s_R))
  )

### A loose caricature of a model considering monthly trends would look like:
test.pois.s <- glmer(
  vl_cases ~ year + sin(2*pi*month/12) + cos(2*pi*month/12) + poly(temp, 2) + precip + pop + (1 | munip_name)
  , data = VL.all %>% filter(munip_name %in% rand_locs)
  , family = "poisson"
)

test_vals <- predict(
    object   = test.pois.s
  , newdata  = data.frame(year = 2017, month = seq(1, 12, by = 1), temp = 0, precip = 0, pop = 0)
  , type     = "response"
  , re.form  = NA
    )
