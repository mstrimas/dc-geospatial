library(sf)
library(raster)
library(ggplot2)
library(dplyr)

# 6. open and plot shapefiles ----

# are of interest
aoi_boundary_harv <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# spatial metadata
st_geometry_type(aoi_boundary_harv)
st_crs(aoi_boundary_harv)
st_bbox(aoi_boundary_harv)
aoi_boundary_harv

# these are data frames
class(aoi_boundary_harv)

# plot boundary
ggplot() +
  geom_sf(data = aoi_boundary_harv,
          size = 3, color = "black", fill = "cyan1") +
  ggtitle("Area of Interst")

#> challenge 1
lines_harv <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
points_harv <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")
# explore:
lines_harv
points_harv

# 7. explore and plot by attribute ----

# explore spatial metadata and attributes
lines_harv

# access a specific attribute
lines_harv$TYPE
unique(lines_harv$TYPE)

# only keep footpaths
footpath_harv <- lines_harv %>% 
  filter(TYPE == "footpath")
nrow(footpath_harv)
ggplot() +
  geom_sf(data = footpath_harv) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Footpaths")
# color by attribute
ggplot() +
  geom_sf(data = footpath_harv,
          # need a factor because OBJECTID is a number
          aes(color = factor(OBJECTID))) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Footpaths",
       color = "Footpath ID")

#> challenge 3
stonewall_harv <- lines_harv %>% 
  filter(TYPE == "stone wall")
ggplot() +
  geom_sf(data = stonewall_harv,
          aes(color = factor(OBJECTID)),
          size = 1.5) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Stonewalls",
       color = "Wall ID")

# all roads by type, user defined columns
unique(lines_harv$TYPE)
ggplot() +
  geom_sf(data = lines_harv,
          aes(color = TYPE, size = TYPE)) +
  scale_color_manual(values = c("blue", "green", "navy", "purple")) +
  scale_size_manual(values = c(1, 2, 3, 4)) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Roads & Trails",
       color = "Road Type")

#> challenge 4
ggplot() +
  geom_sf(data = lines_harv,
          aes(color = TYPE, size = TYPE)) +
  scale_color_manual(values = c("blue", "green", "navy", "purple")) +
  scale_size_manual(values = c(1, 3, 2, 6)) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Roads & Trails",
       color = "Road Type")

# make a line legend
ggplot() +
  geom_sf(data = lines_harv,
          aes(color = TYPE, size = TYPE),
          show.legend = "line") +
  scale_color_manual(values = c("blue", "green", "navy", "purple")) +
  scale_size_manual(values = c(0.5, 1.5, 1, 3)) +
  labs(title = "NEON Harvard Forest Site",
       subtitle = "Roads & Trails",
       color = "Road Type")

#> challenge 6

state_boundary <- read_sf("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp")
ggplot() +
  geom_sf(data = state_boundary, 
          aes(color = region)) +
  scale_color_manual(values = c("orange", "forestgreen", "black", "skyblue", "black")) +
  ggtitle("Contiguous U.S. State Boundaries")


# 8. plot multiple shapefiles ----

# sampling plots
plot_locations <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/PlotLocations_HARV.shp")

# lines and points
ggplot() + 
  geom_sf(data = aoi_boundary_harv, fill = "grey", color = "grey") +
  geom_sf(data = lines_harv, 
          aes(color = TYPE), 
          show.legend = "line") +
  geom_sf(data = plot_locations,
          aes(fill = soilTypeOr),
          shape = 16,
          show.legend = "point") +
  labs(title = "NEON Harvard Forest Field Site",
       color = "Line Type")

# 10 convert from .csv to a shapefile ----

# csv of study plot locations
plot_locations_harv <- read.csv("data/NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv",
                                stringsAsFactors = FALSE)
glimpse(plot_locations_harv)
st_crs(points_harv)
utm18n <- st_crs(points_harv) 

# convert to sf
plot_locations_harv_sf <- st_as_sf(plot_locations_harv, 
                                   coords = c("easting", "northing"),
                                   crs = utm18n)
plot_locations_harv_sf

# map
ggplot() +
  geom_sf(data = plot_locations_harv_sf) +
  ggtitle("Harvard Forest Plot Locations")

#> challenge 1
phen_plots <- read.csv("data/NEON-DS-Site-Layout-Files/HARV/HARV_2NewPhenPlots.csv",
                       stringsAsFactors = FALSE)
glimpse(phen_plots)
phen_plots_sf <- st_as_sf(phen_plots, 
                          coords = c("decimalLon", "decimalLat"),
                          crs = 4326)
phen_plots_sf
