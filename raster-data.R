library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

# 1. intro to raster data ----

# geotiff metadata
GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

# load harvard forest digital surface model
dsm_harv <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")
dsm_harv
summary(dsm_harv, maxsamp = ncell(dsm_harv))

# convert to data frame for plotting
dsm_harv_df <- as.data.frame(dsm_harv, xy = TRUE)
glimpse(dsm_harv_df)

# plot
ggplot() +
  geom_raster(data = dsm_harv_df,
              aes(x, y, fill = HARV_dsmCrop)) +
  scale_fill_viridis_c() +
  coord_quickmap()

# can also quickly plot with plot from raster package
plot(dsm_harv)

#>  challenge 1
crs(dsm_harv)

# min-max value
minValue(dsm_harv)
maxValue(dsm_harv)
# set min-max
dsm_harv <- setMinMax(dsm_harv)

# use a histogram to highlight bad data
ggplot() +
  geom_histogram(data = dsm_harv_df,
                 aes(HARV_dsmCrop),
                 bins = 40)
  

# 2. plotting raster data ----


# classify elevation and plot
dsm_harv_df <- dsm_harv_df %>% 
  mutate(fct_elevation = cut(HARV_dsmCrop, breaks = 3))
ggplot() +
  geom_bar(data = dsm_harv_df,
           aes(fct_elevation))

# cutoff values for the groups
unique(dsm_harv_df$fct_elevation)
count(dsm_harv_df, fct_elevation)

# try custom breaks
custom_bins <- c(300, 350, 400, 450)
dsm_harv_df <- dsm_harv_df %>% 
  mutate(fct_elevation_2 = cut(HARV_dsmCrop, breaks = custom_bins))
unique(dsm_harv_df$fct_elevation_2)
count(dsm_harv_df, fct_elevation_2)
ggplot() +
  geom_bar(data = dsm_harv_df,
           aes(x = fct_elevation_2))

# plot raster using these groups
ggplot() +
  geom_raster(data = dsm_harv_df,
              aes(x, y, fill = fct_elevation_2)) +
  coord_quickmap()

# use terrain colors
terrain.colors(3)
ggplot() +
  geom_raster(data = dsm_harv_df,
              aes(x, y, fill = fct_elevation_2)) +
  scale_fill_manual(values = terrain.colors(3)) +
  coord_quickmap()

# further refinement
my_col <- terrain.colors(3)
ggplot() +
  geom_raster(data = dsm_harv_df,
              aes(x, y, fill = fct_elevation_2)) +
  scale_fill_manual(values = my_col, name = "Elevation") +
  coord_quickmap() +
  theme(axis.title = element_blank())
  
#> challenge 2
dsm_harv_df <- dsm_harv_df %>% 
  mutate(fct_elevation_6 = cut(HARV_dsmCrop, breaks = 6))
ggplot() +
  geom_raster(data = dsm_harv_df,
              aes(x, y, fill = fct_elevation_6)) +
  scale_fill_manual(values = terrain.colors(6), name = "Elevation") +
  ggtitle("Classified Elevation Map - NEON Harvard Forest Field Site") +
  xlab("UTM Westing Coordinate (m)") +
  ylab("UTM Northing Coordinate (m)") +
  coord_quickmap()
  theme(axis.title = element_blank())
  
# hillshade
dsm_hill_harv <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif")
dsm_hill_harv_df <- as.data.frame(dsm_hill_harv, xy = TRUE)
glimpse(dsm_hill_harv_df)
ggplot() +
  geom_raster(data = dsm_harv_df , 
              aes(x, y, fill = HARV_dsmCrop)) + 
  geom_raster(data = dsm_hill_harv_df, 
              aes(x, y, alpha = HARV_DSMhill)) +  
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

#> challenge 2

# dsm
dsm_jster <- raster("data/NEON-DS-Airborne-Remote-Sensing/SJER/DSM/SJER_dsmCrop.tif")
dsm_jster_df <- as.data.frame(dsm_jster, xy = TRUE)
# hillshade
dsm_hill_jster <- raster("data/NEON-DS-Airborne-Remote-Sensing/SJER/DSM/SJER_dsmHill.tif")
dsm_hill_jster_df <- as.data.frame(dsm_hill_jster, xy = TRUE)

# plot
ggplot() +
  geom_raster(data = dsm_jster_df , 
              aes(x = x, y = y, 
                  fill = SJER_dsmCrop,
                  alpha = 0.8)) + 
  geom_raster(data = dsm_hill_jster_df, 
              aes(x = x, y = y, 
                  alpha = SJER_dsmHill)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar()) +
  scale_alpha(range = c(0.4, 0.7), guide = "none") +
  # remove grey background and grid lines
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab("UTM Westing Coordinate (m)") +
  ylab("UTM Northing Coordinate (m)") +
  ggtitle("DSM with Hillshade") +
  coord_quickmap()

# 3. reprojecting raster data ----

# import data, convert to df for plotting
dtm_harv <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")
dtm_hill_harv <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_DTMhill_WGS84.tif")
dtm_harv_df <- as.data.frame(dtm_harv, xy = TRUE) %>% 
  rename(elevation = HARV_dtmCrop)
dtm_hill_harv_df <- as.data.frame(dtm_hill_harv, xy = TRUE) %>% 
  rename(hillshade = HARV_DTMhill_WGS84)

# naive map
ggplot() +
  geom_raster(data = dtm_harv_df,
              aes(x, y, fill = elevation)) +
  geom_raster(data = dtm_hill_harv_df,
              aes(x, y, alpha = hillshade)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

# do they plot fine on their own
ggplot() +
  geom_raster(data = dtm_harv_df,
              aes(x, y, fill = elevation)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()
ggplot() +
  geom_raster(data = dtm_hill_harv_df,
              aes(x, y, alpha = hillshade)) +
  coord_quickmap()

#> exercise 1
crs(dtm_harv)
crs(dtm_hill_harv)

# reproject
dtm_hill_harv_utm <- projectRaster(dtm_hill_harv, crs = crs(dtm_harv))
crs(dtm_hill_harv_utm)
crs(dtm_hill_harv)
extent(dtm_hill_harv_utm)
extent(dtm_hill_harv)

# do the resolutions match
res(dtm_harv)
res(dtm_hill_harv_utm)
dtm_hill_harv_utm <- projectRaster(dtm_hill_harv, 
                                   crs = crs(dtm_harv),
                                   res = 1)

# plot
dtm_hill_harv_utm_df <- as.data.frame(dtm_hill_harv_utm, xy = TRUE) %>% 
  rename(hillshade = HARV_DTMhill_WGS84)
ggplot() +
  geom_raster(data = dtm_harv_df,
              aes(x, y, fill = elevation)) +
  geom_raster(data = dtm_hill_harv_utm_df,
              aes(x, y, alpha = hillshade)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

# 4. raster calculations ----

# look at the dsm and dtm
plot(dtm_harv)
plot(dsm_harv)
chm_harv <- dsm_harv - dtm_harv
plot(chm_harv)

# distribution
chm_harv_df <- as.data.frame(chm_harv, xy = TRUE) %>% 
  rename(canopy_height = layer)
ggplot(chm_harv_df) +
  geom_histogram(aes(canopy_height))

# overlay
chm_harv_ov <- overlay(dsm_harv, dtm_harv,
                       fun = function(r1, r2) {r1 - r2})
plot(chm_harv_ov)

# export
writeRaster(chm_harv_ov, "data/chm_harv.tif")


# 5. multi band rasters ----

# raster() loads only one band
rgb_harv <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")
rgb_harv
plot(rgb_harv, col = grey.colors(25))

# import a specific band
rgb_harv_b2 <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif",
                      band = 2)
plot(rgb_harv_b2, col = grey.colors(25))

# load the stack
rgb_harv_stack <- stack("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")
rgb_harv_stack
# single band
rgb_harv_stack[[1]]

# plot rgb image
plotRGB(rgb_harv_stack, r = 1, g = 2, b = 3)
