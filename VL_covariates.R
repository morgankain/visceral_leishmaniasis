##################################################################################################
## Scratch script to really start exploring the functional forms to use in the model as well as ##
## covariates that seem to bifurcate 0s from non-zeros                                          ##
##################################################################################################

####
## First part explores the vast number of covariates generally
####

top_locs  <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% slice(1:100) %>% dplyr::select(munip_name) %>% unname() %>% unlist()

rand_locs <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% filter(munip_name %notin% top_locs) %>% dplyr::select(munip_name) %>% unname() %>% 
  unlist() %>% sample(300)
 
rand_locs <- c(rand_locs, top_locs)

VL.temp <- VL %>% filter(munip_name %in% rand_locs)

all.covar <- names(VL)[c(6:25, 28)][-c(2, 9, 13, 19)]

all.covar

covar_plot <- 17

## temp   -- 4
## precip -- 6
## pop    -- 17
## gdp    -- 10
## area   -- 1
## ag     -- 11
## ndvi   -- 2

VL.temp %>% {
  ggplot(., aes(
    get(all.covar[covar_plot])
    , vl_cases)) + 
    geom_point(lwd = 2, alpha = 0.5) +
    xlab(all.covar[covar_plot]) + 
    ylab("VL Cases (Month)")
}

func_form <- c(
  "quad log 10", "quad", "quad", "quad", "linear", "linear", "linear"
, "unclear", "unclear", "quad log 10", "linear"
, "unclear", "unclear", "third order", "unclear"
, "third order", "quadratic log 10"
 )

vis_strength <- c(
  "strong", "weak", "strong", "strong", "moderate", "strong", "weak"
, "weak", "weak", "moderate but missing data", "strong but missing data"
, "weak and missing data", "weak and missing data", "unclear", "moderate and missing data"
, "moderate and missing data", "moderate"
)

incl_pred <- c(
  "yes", "maybe", "maybe", "yes", "no", "yes", "no"
, "no", "no", "yes if possible", "maybe"
, "no", "no", "maybe"
, "maybe", "maybe", "yes"
 )

data.frame(
   covariate = all.covar
 , func_form
 , vis_strength
 , incl_pred
  )

VL.test <- VL.temp %>%
  dplyr::select(mean_air_temp, total_precipitation, human_pop, GDP_1000s_R
   , AREA_KM2, Agriculture.2, median_ndvi, vl_cases) %>%
  mutate(
  temp   = mean_air_temp
, precip = total_precipitation
, pop    = human_pop
, gdp    = GDP_1000s_R
, area   = AREA_KM2
, ag     = Agriculture.2
, ndvi   = median_ndvi
  )

VL.test[VL.test$precip == 0, ]$precip <- min(VL.test[VL.test$precip > 0, ]$precip)

VL.test <- VL.test %>% mutate(
  temp   = c(scale(temp, scale = T))
, precip = c(scale(log10(precip), scale = T))   ## log
, pop    = c(scale(log10(pop), scale = T))      ## log
, gdp    = c(scale(log10(gdp), scale = T))           ## log 
, area   = c(scale(log10(area), scale = T))       ## log
, ag     = c(scale(ag, scale = T))     
, ndvi   = c(scale(ndvi, scale = T))
  )

VL.test %>% {
  ggplot(., aes(
      ndvi
    , vl_cases)) + 
    geom_point(lwd = 2, alpha = 0.5) +
    ylab("VL Cases (Month)")
}

## temp   --- no log, quadratic
## precip --- log10, quadratic
## pop    --- log10, quadratic
## gdp    --- log10, quadratic
## area   --- log10, quadratic
## ag     --- no log, linear
## ndvi   --- no log, quadratic
## year   --- no log, cubic?

### Test of scaling the out of sample values vs the in sample values
cov.in  <- c(2, 3, 4)
cov.out <- c(3, 4, 5)

(cov.out - mean(cov.in)) / sd(cov.in)

out.in  <- c(VL.year.out$mean_area - mean(VL.year.in$mean_area)) / sd(VL.year.in$mean_area)
out.out <- c(scale(VL.year.out$mean_area))

out.in  <- c(VL.year.out$mean_ag - mean(VL.year.in$mean_ag)) / sd(VL.year.in$mean_ag)
out.out <- c(scale(VL.year.out$mean_ag))

check_setup.in  <- data.frame(ag = VL.stan.in$ag)
check_setup.out <- data.frame(ag = c(VL.year.out$mean_ag - mean(VL.year.in$mean_ag)) / sd(VL.year.in$mean_ag))

ggplot(check_setup.in, aes(x = ag)) + 
  geom_histogram(bins = 100, alpha = 0.2, colour = "red") +
  geom_histogram(data = check_setup.out, bins = 100, alpha = 0.2, colour = "blue")

check_setup.in  <- data.frame(ag = scale(VL.stan.in$mean_ag, scale = T))
check_setup.out <- data.frame(ag = scale(VL.year.out$mean_ag, scale = T))

ggplot(check_setup.in, aes(x = ag)) + 
  geom_histogram(bins = 100, alpha = 0.2, colour = "red") +
  geom_histogram(data = check_setup.out, bins = 100, alpha = 0.2, colour = "blue")

####
## Second part explores bifurcations in zeros and no zeros
####

top_locs  <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% slice(1:300) %>% dplyr::select(munip_name) %>% unname() %>% unlist()

rand_locs <- VL %>% group_by(munip_name) %>% summarize(tot_cases = sum(vl_cases)) %>% 
  arrange(desc(tot_cases)) %>% filter(munip_name %notin% top_locs) %>% dplyr::select(munip_name) %>% unname() %>% 
  unlist() %>% sample(1000)
 
rand_locs <- c(rand_locs, top_locs)

VL.temp <- VL %>% filter(munip_name %in% rand_locs)

all.covar <- names(VL)[c(6:25, 28)][-c(2, 9, 13, 19)]

all.covar

VL.temp.z  <- VL.temp %>% filter(vl_cases == 0) 
VL.temp.nz <- VL.temp %>% filter(vl_cases > 0) 
  
for (i in seq_along(all.covar)) {
  
VL.temp.z.t <- VL.temp.z %>% dplyr::select(all.covar[i])
names(VL.temp.z.t) <- "x_val"

VL.temp.nz.t <- VL.temp.nz %>% dplyr::select(all.covar[i])
names(VL.temp.nz.t) <- "x_val"
  
temp.gg <- ggplot(VL.temp.z.t, aes(x = x_val)) + 
    geom_histogram(alpha = 0.2, colour = "blue", lwd = 0.1, bins = 200) +
    geom_histogram(data = VL.temp.nz.t
      , aes(x = x_val)
      , alpha = 0.2, colour = "red", bins = 200, lwd = 0.1) +
    scale_x_log10() +
    xlab(all.covar[i]) + 
   # ylab("VL Cases (Month)") +
    ylab("Count") +
    geom_vline(aes(xintercept = median(x_val, na.rm = T)), colour = "blue", lwd = 1) +
    geom_vline(data = VL.temp.nz.t
      , aes(xintercept = median(x_val, na.rm = T)), colour = "red", lwd = 1) 
  
temp_name <- paste("temp.gg", i, sep = "_")
assign(temp_name, temp.gg)

}

gridExtra::grid.arrange(
  temp.gg_1, temp.gg_2, temp.gg_3, temp.gg_4, temp.gg_5, temp.gg_6
, temp.gg_7, temp.gg_8, temp.gg_9, temp.gg_10, temp.gg_11, temp.gg_12
, temp.gg_13, temp.gg_14, temp.gg_15, temp.gg_16, temp.gg_17, ncol = 5)

## Possibly:
 ## Area 
 ## CD_MUN_ibg
 ## GDP
 ## pop
 ## Savannah


