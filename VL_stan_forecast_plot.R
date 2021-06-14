randplotloc <- sample(seq(1, length(unique(VL.year$munip_name))), 30)
randplotloc <- unique(VL.year$munip_name)[randplotloc]

VL.year %>% 
  filter(munip_name %in% randplotloc) %>% 
#   filter(munip_name == "Bauru") %>% 
  droplevels() %>% {
  ggplot(.) + 
    geom_point(aes(year, vl_cases)) +
    geom_errorbar(
      data = (pred.out.L %>% 
          filter(munip_name %in% randplotloc)
    #      filter(munip_name == "Bauru")
        )
    , aes(year, ymin = lwr, ymax = upr)
    , color = "firebrick3"
    , lwd = 0.5
    , width = 0.2
      , position = position_nudge(x = -0.2)
    ) + 
    geom_errorbar(
      data = (pred.out.S %>% 
         filter(munip_name %in% randplotloc)
     #     filter(munip_name == "Bauru")
        )
    , aes(year, ymin = lwr, ymax = upr)
    , color = "dodgerblue3"
    , lwd = 0.5
    , width = 0.2
    , position = position_nudge(x = 0.2)
    ) + 
     scale_y_sqrt(breaks = c(0, 1, 2, 16, 32, 64, 128, 256)) +
      scale_x_continuous(breaks = c(
        2007, 2009, 2011, 2013, 2015, 2017
      )) +
     facet_wrap(~munip_name, scales = "free") +
      theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 10, angle = 310, hjust = 0) 
  , axis.text.y = element_text(size = 12) 
  , axis.title.x = element_text(size = 12) 
  , axis.title.y = element_text(size = 12)
  )
  }
