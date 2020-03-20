#### Mapping Occurrences of Longleaf and Loblolly Pine ####
#### Meg Goeke (goekeme2@vcu.edu)
#### 03/16/2020

# Packages and Libraries ----
library(rgbif)
library(dplyr)
library(ggplot2)
library(maps)
library(GISTools)
library(ggsn)

# Working directory ----
setwd("C:/Users/mgoek/Documents/R/Spatial_analysis_mapping")

# Loading longleaf pine data from the Global Biodiversity Index----
longleaf_pine <- occ_search(scientificName = "Pinus palustris Mill.",
                            country = "US", hasCoordinate = TRUE, limit = 3000,
                            year = '2000,2020', return = "data")

write.csv(longleaf_pine, file = "longleaf_pine.csv", row.names = F) # save locally

# Clean data ----
vars <- c("gbifID", "scientificName", "decimalLongitude", "decimalLatitude", 
          "coordinateUncertaintyInMeters", "stateProvince")

pine_trim <- longleaf_pine %>%  # keeping just the columns I need
  dplyr::select(one_of(vars))

# make preliminary plot to check data
(prelim_plot <- ggplot(pine_trim, aes(x = decimalLongitude, y = decimalLatitude)) +
    geom_point())

# assign US map data to object
states_map <- map_data("state")

# check map
(prelim_states <- ggplot() +
    geom_polygon(data = states_map, aes(x = long, y = lat, group = group), 
                 fill = "lightyellow2", color = "gray21") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()))

# now with the occurrence data
(prelim_pine <- ggplot(pine_trim, aes(x = decimalLongitude, 
                                      y = decimalLatitude)) +
    geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                 fill = "antiquewhite", color = "gray45", size = 0.7) +
    geom_point(alpha = 0.6, fill = "forestgreen", color = "darkgreen") +
    scalebar(data = states_map, dist = 250, dist_unit = "km", transform = TRUE, 
             model = "WGS84", height = 0.005, location = "bottomright",
             anchor = c(x = -73.5, y = 26), st.color = "gray21",
             box.color = "gray21", box.fill = c("gray45", "white"), 
             st.size = 3, border.size = 0.5) +
    coord_cartesian(xlim = c(-96,-73), ylim = c(25.5,41)) +
    labs(title = "Longleaf pine distribution in the US",
         caption = "Source: Global Biodiversity Index") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          title = element_text(color = "gray21"),
          panel.background = element_rect(fill = "azure3"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")))

# Make the finished plot to include a density overlay ----
(longleaf_density <- ggplot(pine_trim, aes(x = decimalLongitude, 
                                           y = decimalLatitude)) +
   geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                fill = "antiquewhite", color = "gray45", size = 0.7) +
   geom_point(size = 1.5, alpha = 0.5, fill = "#008B8B", color = "#2F4F4F") +
   scalebar(data = states_map, dist = 250, dist_unit = "km", transform = TRUE, 
            model = "WGS84", height = 0.005, location = "bottomright",
            anchor = c(x = -73.5, y = 26), st.color = "gray21",
            box.color = "gray21", box.fill = c("gray45", "white"), 
            st.size = 3, border.size = 0.5) +
   stat_density2d(aes(x = decimalLongitude, y = decimalLatitude, 
                      fill = ..level.., alpha = ..level..),
                  geom = "polygon", color = NA, size = 0.3) +
   scale_fill_gradient(low = "darkolivegreen1", high = "darkgreen", name = "Density") +
   scale_alpha(range = c(.25, .5), guide = FALSE) +
   coord_cartesian(xlim = c(-96,-73), ylim = c(25.5,41)) +
   labs(title = "Longleaf pine distribution in the US",
        caption = "Source: Global Biodiversity Index") +
   theme_classic() +
   theme(axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         legend.text = element_text(size = 8, color = "gray21"),
         title = element_text(color = "gray21"),
         panel.background = element_rect(fill = "azure3"),
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")))

# Map loblolly occurrence data to compare with longleaf pine ----
# Get GBIF data
loblolly_pine <- occ_search(scientificName = "Pinus taeda L.",
                            country = "US", hasCoordinate = TRUE, limit = 4000,
                            year = '2000,2020', return = "data")


write.csv(loblolly_pine, file = "loblolly_pine.csv", row.names = F) # saving

# Clean data
vars2 <- c("gbifID", "scientificName", "decimalLongitude", "decimalLatitude", 
           "coordinateUncertaintyInMeters", "stateProvince")
loblolly_trim <- loblolly_pine %>% 
  dplyr::select(one_of(vars2)) %>% 
  filter(!decimalLongitude < -100 & !decimalLongitude > -72)

# check data with preliminary plot
(prelim_plot2 <- ggplot(loblolly_trim, 
                        aes(x = decimalLongitude, y = decimalLatitude)) +
    geom_point())

# plot distribution and density
(loblolly_density <- ggplot(loblolly_trim, aes(x = decimalLongitude, 
                                           y = decimalLatitude)) +
    geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                 fill = "antiquewhite", color = "gray45", size = 0.7) +
    geom_point(size = 1.5, alpha = 0.5, fill = "#EE3B3B", color = "#8B2323") +
    scalebar(data = states_map, dist = 250, dist_unit = "km", transform = TRUE, 
             model = "WGS84", height = 0.005, location = "bottomright",
             anchor = c(x = -73.5, y = 26), st.color = "gray21",
             box.color = "gray21", box.fill = c("gray45", "white"), 
             st.size = 3, border.size = 0.5) +
    stat_density2d(aes(x = decimalLongitude, y = decimalLatitude, 
                       fill = ..level.., alpha = ..level..),
                   geom = "polygon", color = NA, size = 0.3) +
    scale_fill_gradient(low = "darkolivegreen1", high = "darkgreen", 
                        name = "Density") +
    scale_alpha(range = c(.25, .5), guide = FALSE) +
    coord_cartesian(xlim = c(-99.5,-73), ylim = c(25.5,41)) +
    labs(title = "Loblolly pine distribution in the US",
         caption = "Source: Global Biodiversity Index") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.text = element_text(size = 8, color = "gray21"),
          title = element_text(color = "gray21"),
          panel.background = element_rect(fill = "azure3"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")))

pine_merged <- full_join(loblolly_trim, pine_trim)  # merge occurrence datasets

# Plot merged dataset to compare species distribution
(loblolly_longleaf <- ggplot(pine_merged, 
                             aes(x = decimalLongitude, y = decimalLatitude, 
                                 color = scientificName)) +
    geom_polygon(data = states_map, aes(x = long, y = lat, group = group),
                 fill = "antiquewhite", color = "gray45", size = 0.05) +
    geom_point(alpha = 0.6, size = 1) +
    scale_fill_manual(values = c("#008B8B", "#EE3B3B")) +
    scale_color_manual(values = c("#008B8B", "#EE3B3B"), name = "Species") +
    scalebar(data = states_map, dist = 250, dist_unit = "km", transform = TRUE, 
             model = "WGS84", height = 0.005, location = "bottomright",
             anchor = c(x = -73.5, y = 26), st.color = "gray21",
             box.color = "gray21", box.fill = c("gray45", "white"), 
             st.size = 3, border.size = 0.5) +
    labs(title = "Distribution of longleaf and loblolly pine in the US",
         caption = "Source: Global Biodiversity Index") +
    coord_cartesian(xlim = c(-99.5,-73), ylim = c(25.5,41)) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          title = element_text(color = "gray21"),
          panel.background = element_rect(fill = "azure3"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = "cm")))
