########################################################
## Explore VL data sent by Caroline on April 13, 2021 ##
########################################################

library(ggplot2)
library(dplyr)
library(geobr)
library(sf)
source("ggplot_theme.R")

mun        <- readRDS("Brazil_map.rds")
VL         <- readRDS("prelim_vl_data.rds")
name_match <- read.csv("VL_municipality_match.csv")

map_names <- strsplit(as.character(unique(mun$name_muni)), " ") %>% 
  sapply(., FUN = function(x) x) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

VL_names <- strsplit(as.character(VL$NM_MUN), " ") %>% 
  sapply(., FUN = function(x) x) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

VL         <- VL %>% mutate(VL_name = VL_names)
name_match <- name_match %>% dplyr::select(VL_name, Convert_name) %>% rename(munip_name = Convert_name)

VL         <- VL %>% left_join(., name_match)

mun <- mun %>% rename(munip_name = name_muni)

mun <- mun %>% left_join(., 
  (VL %>% group_by(munip_name) %>% summarize(total_cases = sum(vl_cases)))
  )

ggplot(data = mun) + geom_sf(aes(fill = total_cases), color = "#FEBF57", size = 0)
 
## Cases by year by municipality
VL %>% 
  group_by(NM_MUN, year) %>%
  summarize(num_cases = sum(vl_cases)) %>% 
  mutate(year = as.numeric(year)) %>% {
    ggplot(., aes(year, num_cases)) +
      geom_line(aes(group = NM_MUN), alpha = 0.2) +
      scale_y_continuous(trans = "pseudo_log", breaks = c(0, 1, 5, 10, 100, 200, 300, 3000)) +
      geom_line(
        data = (VL %>% group_by(year) %>% summarize(num_cases = sum(vl_cases)) %>% mutate(year = as.numeric(year)))
      , aes(year, num_cases)
      , lwd = 1, colour = "red3"
      ) +
      geom_line(
        data = (VL %>% group_by(year) %>% summarize(num_cases = mean(vl_cases)) %>% mutate(year = as.numeric(year)))
      , aes(year, num_cases)
      , lwd = 1, colour = "blue3"
      ) +
      xlab("Year") + ylab("VL Cases")
  }

## Cases by month by municipality
VL %>% 
  group_by(NM_MUN, month) %>%
  summarize(num_cases = mean(vl_cases)) %>% 
  mutate(month = as.numeric(month)) %>% {
    ggplot(., aes(month, num_cases)) +
      geom_line(aes(colour = NM_MUN)) +
      guides(colour = FALSE) +
      xlab("Month") + ylab("VL Cases")
  }
