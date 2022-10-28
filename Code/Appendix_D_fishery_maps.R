library(tidyverse)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rgdal)

#PREPARE data----------------------------------------------------------------------------------------------------------
#downloaded 10/14/22 from website 
#https://www.fisheries.noaa.gov/resource/map/spatial-data-collected-groundfish-observers-alaska
obs <- read.csv("../Data/MOST_Fish.csv")

#rename columns
names(obs)[names(obs) == "YEAR"] <- "Year"
names(obs)[names(obs) == "LAT400SQKM"] <- "Lat"
names(obs)[names(obs) == "LON400SQKM"] <- "Long"

unique(obs$GEAR)
obs$GEAR <- factor(obs$GEAR)
levels(obs$GEAR) <- list(Longline="LGL", "Non-pelagic trawl"="NPT", Pot="POT", "Pelagic trawl" = "PTR")

#adjust data points for 180 line
obs$Long = ifelse(obs$Long > 0, obs$Long - 360, obs$Long)

unique(obs$SPECIES)
#reduce to just shark species
selected <- c("Pacific Sleeper Shark", 
              "Spiny Dogfish Shark",
              "Salmon Shark",
              "Shark Unidentified",
              "Basking Shark", 
              "Blue Shark", 
              "Brown Cat Shark", 
              "Pacific Sharpnose Shark", 
              "Sixgill Shark",
              "Other Shark Species")
obs <- obs[obs$SPECIES %in% selected,]

#Set categories for species
obs <- obs %>%
  mutate(Common_Name =
           ifelse(SPECIES %in% c("Pacific Sleeper Shark"), "Sleeper Shark",
            ifelse(SPECIES %in% c("Spiny Dogfish Shark"), "Spiny Dogfish",
              ifelse(SPECIES %in% c("Salmon Shark"), "Salmon Shark", 
                ifelse(SPECIES %in% c("Shark Unidentified"), "Unidentified Shark Species", 
                  ifelse(SPECIES %in% c("Basking Shark", "Blue Shark", "Brown Cat Shark", "Pacific Sharpnose Shark", "Sixgill Shark"), 
                    "Other Shark Species", "other"))))))

obs$Common_Name<-factor(obs$Common_Name)

#Only show catch since 2003
obs <- obs %>% 
  filter(Year >= 2003)

#Cut into 5-year blocks
#struggled with how to avoid NAs for max bin (max(obs$Year) didn't work), will have to adjust in future
obs$year_blocks <- cut(obs$Year, seq(min(obs$Year), 2025,5), 
                       include.lowest = TRUE, right=FALSE, ordered_result = TRUE,
                       labels = c("2003-2007",
                                  "2008-2012",
                                  "2013-2017",
                                  "2018-2021"))

#Add column to divide pre and post-observer program restructuring (2013)
obs$restructure <- ifelse(obs$Year < 2013, "pre", "post")
obs$restructure <- factor(obs$restructure,
                                          levels = c("pre", "post"),
                                          labels = c("pre-2013", "post-2013"))

###set up map---------------------------------------------------------------------------------------------------------
#get land
world <- ne_countries(scale = "medium", returnclass = "sp")

#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)
#russia
russia <- subset(world, admin == "Russia")
russia <- fortify(russia)
russia$long = ifelse(russia$long > 0, russia$long - 360, russia$long)
#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy = readOGR("../Data/race_bathy_to_200_NAD1983_HARN")
race_bathy_df = fortify(race_bathy)
race_bathy_df$long = ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)



#Catch----------------------------------------------------------------------------------------------
#..Sleeper sharks --------------

#This aggregates by 5-year time block and calculates the sum or the average catch weight for each grid cell
PSS <- obs %>% 
  filter(Common_Name == "Sleeper Shark") %>% 
  group_by(year_blocks, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

PSS.fishery.map.timeblocks <- 
  ggplot() +
  facet_wrap(~year_blocks) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = PSS, 
             aes(x = Long, y = Lat, color=avg),
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/PSS_catch_map_timeblocks.png", plot=PSS.fishery.map.timeblocks, width = 11, height = 7, dpi=600)


#This aggregates by gear and pre/post restructure and calculates sum or average catch weight for each grid cell
PSS.gear <- obs %>% 
  filter(Common_Name == "Sleeper Shark" & GEAR != "Pot") %>% 
  group_by(restructure, GEAR, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

PSS.catch.map.gear <- 
  ggplot() +
  facet_grid(GEAR~restructure) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = PSS.gear,
             aes(x = Long, y = Lat, color=avg), 
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/PSS_catch_gear.png", plot=PSS.catch.map.gear, dpi=600)



#..Spiny dogfish --------------
#This aggregates by 5-year time block and calculates the sum or the average catch weight for each grid cell
SD <- obs %>% 
  filter(Common_Name == "Spiny Dogfish") %>% 
  group_by(year_blocks, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

SD.fishery.map.timeblocks <- 
  ggplot() +
  facet_wrap(~year_blocks) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = SD, 
             aes(x = Long, y = Lat, color=avg),
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))


ggsave("../Figures/SD_catch_map_timeblocks.png", plot=SD.fishery.map.timeblocks, width = 11, height = 7, dpi=600)

#This aggregates by gear and pre/post restructure and calculates sum or average catch weight for each grid cell
SD.gear <- obs %>% 
  filter(Common_Name == "Spiny Dogfish" & GEAR != "Pot") %>%  
  group_by(restructure, GEAR, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

SD.catch.map.gear <- 
  ggplot() +
  facet_grid(GEAR~restructure) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = SD.gear,
             aes(x = Long, y = Lat, color=avg), 
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/SD_catch_gear.png", plot=SD.catch.map.gear, dpi=600)


#..Salmon shark----------------------------
#This aggregates by 5-year time block and calculates the sum or the average catch weight for each grid cell
SS <- obs %>% 
  filter(Common_Name == "Salmon Shark") %>% 
  group_by(year_blocks, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

SS.fishery.map.timeblocks <- 
  ggplot() +
  facet_wrap(~year_blocks) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = SS, 
             aes(x = Long, y = Lat, color=avg),
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))


ggsave("../Figures/SS_catch_map_timeblocks.png", plot=SS.fishery.map.timeblocks, width = 11, height = 7, dpi=600)

#This aggregates by gear and pre/post restructure and calculates sum or average catch weight for each grid cell
SS.gear <- obs %>% 
  filter(Common_Name == "Salmon Shark" & GEAR != "Pot") %>%  
  group_by(restructure, GEAR, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

SS.catch.map.gear <- 
  ggplot() +
  facet_grid(GEAR~restructure) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = SS.gear,
             aes(x = Long, y = Lat, color=avg), 
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

ggsave("../Figures/SS_catch_gear.png", plot=SS.catch.map.gear, dpi=600)




# ..Other sharks ----------------------------------------------------------

other <- obs %>% 
  filter(Common_Name == "Other Shark Species") %>% 
  group_by(SPECIES, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

other.tot <- 
ggplot() +
  facet_wrap(~SPECIES, ncol=1) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = other, 
             aes(x = Long, y = Lat, color=tot),
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,0,-0.5, "pt"))

ggsave("../Figures/other_catch_map_totals.png", plot=other.tot, dpi=600)


# Unidentified ------------------------------------------------------------

#This aggregates by 5-year time block and calculates the sum or the average catch weight for each grid cell
unID <- obs %>% 
  filter(Common_Name == "Unidentified Shark Species") %>% 
  group_by(year_blocks, Lat, Long) %>% 
  summarize(avg = mean(KG),
            tot = sum(KG))

unID.fishery.map.timeblocks <- 
  ggplot() +
  facet_wrap(~year_blocks) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_point(data = unID, 
             aes(x = Long, y = Lat, color=avg),
             shape = 15, size=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, 
                        guide="colourbar", label = scales::comma) +
  labs(color = "Catch weight (kg)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(2, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,0,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))


ggsave("../Figures/unID_catch_map_timeblocks.png", plot=unID.fishery.map.timeblocks, width = 11, height = 7, dpi=600)
