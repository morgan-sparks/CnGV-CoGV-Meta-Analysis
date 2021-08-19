setwd("~/Dropbox/PhD Work/Critical Review")
library(ggplot2)
library(ggmap) 
library(maps)
 

usa <- map_data("usa")
states <- map_data("state")

east_sea= subset(states, region %in% c( "connecticut", "delaware", "florida", "georgia",
                                        "maine", "maryland", 
                                        "massachusetts", "new hampshire", "new jersey", 
                                        "new york", "north carolina", "pennsylvania", "rhode island", 
                                        "south carolina", "vermont", 
                                        "virginia", "west virginia"))
east_time= subset(states, region %in% c( "connecticut", "delaware", "florida", "georgia", 
                                        "indiana", "kentucky", "maine", "maryland", 
                                        "massachusetts", "michigan", "new hampshire", "new jersey", 
                                        "new york", "north carolina", "ohio", "pennsylvania", "rhode island", 
                                        "south carolina", "tennessee", "vermont", 
                                        "virginia", "west virginia"))
east_sea_map <- ggplot(data=east_sea) +
  geom_polygon(aes(x=long, y = lat, group=group), fill = "gray75", color= "black", line = 6) +
  coord_fixed(1.3) +
  guides(fill=F) +
  theme_classic(base_size = 30) +
  ylab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) 

ggsave("~/Downloads/east_sea_map.pdf", east_sea_map, bg = "transparent")



