####
## Needed packages
####

needed_packages <- c(
  "tidyr"
, "dplyr"
, "reshape2"
, "ggplot2"
, "lme4"
, "gridExtra"
, "betareg"
, "MASS"
, "rgdal"
, "gdalUtils"
, "rsq"
, "ciTools"
, "rstan"
, "shinystan"
, "glmmTMB"
)

## If inst_miss_pack == TRUE, install all missing packages, otherwise print the packages that are missing
if (inst_miss_pack) {

if (length(setdiff(needed_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(needed_packages, rownames(installed.packages())))  
}

lapply(needed_packages, require, character.only = TRUE)

} else {

print(
data.frame(needed_packages, loaded = unlist(lapply(needed_packages, require, character.only = TRUE)))
)
  
}

####
## Functions
####
`%notin%` <- Negate(`%in%`)
source("ggplot_theme.R")

####
## Data
####

## A previously cleaned data file has been saved. But if for some reason it isn't present, it can be recreated from the raw data
if (!file.exists("VL_cleaned.Rds")) {
## VL data, District mapping data, and a matching of the district names
mun        <- readRDS("Brazil_map.rds")
VL         <- readRDS("prelim_vl_data.rds")
name_match <- read.csv("match_names.csv")

map_names <- strsplit(as.character(unique(mun$name_muni)), " ") %>% 
  sapply(., FUN = function(x) x) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

VL_names <- strsplit(as.character(VL$NM_MUN_MoH), " ") %>% 
  sapply(., FUN = function(x) x) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

VL         <- VL %>% mutate(VL_name = VL_names)

name_match <- name_match %>% dplyr::select(Data_Name, Name_To) %>% rename(munip_name = Name_To, VL_name = Data_Name)

VL         <- VL %>% left_join(., name_match)
mun        <- mun %>% rename(munip_name = name_muni)

mun.all   <- mun %>% left_join(., (VL %>% group_by(munip_name) %>% summarize(total_cases = sum(vl_cases))))
mun.month <- mun %>% left_join(., (VL %>% group_by(munip_name, month) %>% summarize(total_cases = sum(vl_cases))))
mun.year  <- mun %>% left_join(., (VL %>% group_by(munip_name, year) %>% summarize(total_cases = sum(vl_cases))))

## Some missing population sizes
pop_size <- VL %>% filter(!is.na(total_population)) %>% group_by(munip_name) %>% 
  slice(1) %>% dplyr::select(munip_name, total_population) %>%
  rename(human_pop = total_population)
  
VL <- VL %>% left_join(., pop_size)

saveRDS(
    list(VL = VL, mun.all = mun.all, mun.month = mun.month, mun.year = mun.year)
  , "VL_cleaned.Rds") 

} else {
all.data  <- readRDS("VL_cleaned.Rds")
VL        <- all.data[[1]]
mun.all   <- all.data[[2]]
mun.month <- all.data[[3]]
mun.year  <- all.data[[4]]
rm(all.data)
}
