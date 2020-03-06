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