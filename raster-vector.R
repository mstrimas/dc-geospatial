library(raster)
library(sf)

# import data
# vector
point <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")
plot_locations <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/PlotLocations_HARV.shp")
aoi_boundary <- read_sf("data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# raster
chm <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/CHM/HARV_chmCrop.tif")
chm_df <- as.data.frame(chm, xy = TRUE)

# map 
plot(chm)
plot(aoi_boundary, col = NA, add = TRUE)

# crop
chm_cropped <- crop(chm, aoi_boundary)
plot(chm_cropped)
plot(aoi_boundary, add = TRUE)
ncell(chm)
ncell(chm_cropped)

#> challenge 1
chm_plot_crop <- crop(chm, plot_locations)
plot(chm_plot_crop)
plot(plot_locations, add = TRUE)

# extract polygon
tree_height <- extract(chm, aoi_boundary, df = TRUE)
str(tree_height)
ggplot() +
  geom_histogram(data = tree_height, aes(HARV_chmCrop))
summary(tree_height$HARV_chmCrop)

# summarize extracted values
tree_height_mean <- extract(chm, aoi_boundary, fun = mean)
tree_height_mean

# extract from point buffer
tree_height_tower <- extract(chm, point, fun = mean, buffer = 20)
tree_height_tower

#> challenge 2
tree_height_plots <- extract(chm,
                             plot_locations, 
                             fun = mean, 
                             buffer = 20, 
                             df = TRUE)
tree_height_plots
