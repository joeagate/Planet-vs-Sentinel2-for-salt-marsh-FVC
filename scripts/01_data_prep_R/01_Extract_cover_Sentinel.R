# Set up ------------------------------------------------------------------

# Load packages
library(terra)
library(tidyverse)
library(here)
library(tidyterra)

# Reset working directory
setwd(here())

# Load the extraction script
source('./src/extract_cover_per pixel.R')

# TODO Change the writing of files to match the new file structure


# Wrangling ---------------------------------------------------------------

# Import the Sentinel images
setwd('data/external/Sentinel_images_super_resolved/')
files <- list.files(pattern = '.tif')
s2_images <- lapply(files, rast)
setwd(here())
names(s2_images) <- gsub('.tif', '', files)
rm(files)

# Paths to UAV maps established
UAV_est <- c("AE_2021-07-17" = 'data/raw/uav_maps/majority_filter/AE_est21_predicted_NVC.tif',
             "AE_2022-07-10" = 'data/raw/uav_maps/majority_filter/AE_est22_predicted_NVC.tif',
             "AE_2023-07-07" = 'data/raw/uav_maps/majority_filter/AE_est23_predicted_NVC.tif',
             "AH_2023-07-07" = 'data/raw/uav_maps/majority_filter/AH_est23_predicted_NVC.tif',
             "CP_2023-06-25" = 'data/raw/uav_maps/majority_filter/CP_est23_predicted_NVC.tif')

# Paths to UAV maps new
UAV_new <- c("AE_2021-07-17" = 'data/raw/uav_maps/majority_filter/AE_new21_predicted_NVC.tif',
             "AE_2022-07-10" = 'data/raw/uav_maps/majority_filter/AE_new22_predicted_NVC.tif',
             "AE_2023-07-07" = 'data/raw/uav_maps/majority_filter/AE_new23_predicted_NVC.tif',
             "AH_2023-07-07" = 'data/raw/uav_maps/majority_filter/AH_new23_predicted_NVC.tif',
             "CP_2023-06-25" = 'data/raw/uav_maps/majority_filter/CP_new23_predicted_NVC.tif')

# DTMs
dtm_paths <- c("AE_2021-07-17" = 'data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif',
               "AE_2022-07-10" = 'data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif',
               "AE_2023-07-07" = 'data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20240212.tif',
               "AH_2023-07-07" = 'data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20230422.tif',
               "CP_2023-06-25" = 'data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif')

# AOIs established
aoi_est_paths <- c("AE_2021-07-17" = 'data/raw/aois/Adur_Estuary/W7_established.shp',
                   "AE_2022-07-10" = 'data/raw/aois/Adur_Estuary/W7_established.shp',
                   "AE_2023-07-07" = 'data/raw/aois/Adur_Estuary/W7_established.shp',
                   "AH_2023-07-07" = 'data/raw/aois/Abbotts_Hall/AHS_AOI.shp',
                   "CP_2023-06-25" = 'data/raw/aois/Cobnor_Point/AOI_CobEst.shp')

# AOIs established
aoi_new_paths <- c("AE_2021-07-17" = 'data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp',
                   "AE_2022-07-10" = 'data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp',
                   "AE_2023-07-07" = 'data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp',
                   "AH_2023-07-07" = 'data/raw/aois/Abbotts_Hall/AHF_AOI.shp',
                   "CP_2023-06-25" = 'data/raw/aois/Cobnor_Point/Cobnor_new_AOI.shp')

# UAV survey dates est
uav_dates_est <- c("AE_2021-07-17" = '2021-07-16',
                   "AE_2022-07-10" = '2022-06-23',
                   "AE_2023-07-07" = '2023-07-18',
                   "AH_2023-07-07" = '2023-08-28',
                   "CP_2023-06-25" = '2023-07-18')

# UAV survey dates new
uav_dates_new <- c("AE_2021-07-17" = '2021-07-16',
                   "AE_2022-07-10" = '2022-06-23',
                   "AE_2023-07-07" = '2023-07-18',
                   "AH_2023-07-07" = '2023-08-10',
                   "CP_2023-06-25" = '2023-07-18')


# Established loop --------------------------------------------------------

cat('\n===========\nESTABLISHED\n===========\n')

# Initialise a tibble to store all the results
est_full <- tibble()

for (i in names(s2_images)){
  cat('\n', i, '\n-------------\n', sep = '')
  
  # Extract the metadata
  sat_date <- str_sub(i, start = 4)
  site <- substr(i, start = 1, stop = 2)
  uav_date = uav_dates_est[i]
  
  # Import the AOI
  aoi <- vect(aoi_est_paths[i])
  
  # Extract the s2 image
  s2 <- s2_images[[i]]
  
  # Rename the s2 bands
  if(names(s2) %>% length() == 11){
    s2 <- subset(s2, -1)
  }
  names(s2) <- c('B2', 'B3', 'B4', 'SRB5', 'SRB6', 'SRB7', 'B8', 'B8A', 'SRB11', 'SRB12')
  
  # Set OSGB36 CRS
  OSGB36 <- "+init=EPSG:27700"
  
  # Convert Sentinel CRS to OSGB36
  s2 <- terra::project(s2, OSGB36)
  
  # Clip s2 to the aoi
  s2_clip <- mask(crop(s2, aoi), aoi, touches = F)
  

## Vegetation indices ----
  
  # Extract the bands
  B <- s2_clip[["B2"]]
  G <- s2_clip[["B3"]]
  R <- s2_clip[["B4"]]
  Redg <- s2_clip[["SRB5"]]
  NIR <- s2_clip[["B8A"]]
  
  # Normalized Difference Vegetation Index
  NDVI <- (NIR - R) / (NIR + R)
  # Green Difference Vegetation Index
  GDVI <- NIR - G
  # Green Normalized Vegetation Index
  GNDVI <- (NIR - G) / (NIR + G)
  # Green Ratio Vegetation Index
  GRVI <- NIR / G
  # Green Infrared Percentage Vegetation Index
  GIPVI <- NIR / (NIR + G)
  # Simple Ratio
  SR <- NIR / R
  # Green Difference Index
  GDI <- NIR - R + G
  # Green Red Difference Index
  GRDI <- (G - R) / (G + R)
  # Red edge normalized difference vegetation index
  NDVIre <- (NIR - Redg) / (NIR + Redg)
  # Red edge simple ratio
  SRre <- NIR / Redg
  # Red edge triangular vegetation index (core only)
  RTVIcore <- 100 * (NIR - Redg) - 10 * (NIR - G)
  # MSRred edge
  MSRedg <- (NIR / Redg) - 1 / sqrt(NIR / Redg) + 1
  # Datt4
  Datt4 <- R / G * Redg
  # MSAVI2
  MSAVI2 <- (2 * NIR + 1 - sqrt ((2 * NIR + 1)^2 - 8 * (NIR - R))) / 2
  # EVI
  EVI <-  2.5*((NIR-R)/(NIR+6*R-7.5*B+1))
  
  # Append to Sentinel data
  s2_clip <- c(s2_clip, NDVI, GDVI, GNDVI,  
               # GIPVI, MSRedg, SR, MSAVI2, GRVI,
               GDI, GRDI, 
               NDVIre, SRre, RTVIcore,  Datt4, EVI)
  
  # Remove VIs from scope
  rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
     NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  
  # Add the VI names
  names(s2_clip) <- c(names(s2), 'NDVI', 'GDVI', 'GNDVI', 
                      # 'GIPVI', 'SR', 'MSRedg', 'MSAVI2', 'GRVI', 
                      'GDI', 'GRDI', 'NDVIre', 'SRre', 'RTVIcore', 'Datt4', 'EVI')
  
  # Import the DTM
  dtm <- rast(dtm_paths[i])
  crs(dtm) <- crs(aoi)
  
  # Resample the dtm to s2
  dtm_ag <- aggregate(dtm, res(s2)[1]/res(dtm)[1])
  dtm_s2 <- resample(dtm_ag, s2)
  
  # Clip the dtm to the aoi
  dtm_s2_clip = mask(crop(dtm_s2, aoi), aoi, touches = F)
  
  # Rename the DTM layer
  names(dtm_s2_clip) <- 'DTM'
  
  # Combine the s2 and dtm
  s2_dtm_combined <- c(s2_clip, dtm_s2_clip)
  
  
  # Write the satellite image to drive
  filenameAllBands <- paste0(site, '_est_AllBands_B8A-NIR_', sat_date, '.tiff')
  # writeRaster(s2_dtm_combined, paste0('Upscaling/Inputs/Sentinel_maps_v2/', filenameAllBands),
  #             overwrite = T)
  
  # Filter away the non common bands and write to drive
  s2_dtm_combined_common <- s2_dtm_combined %>%
    select(-c(SRB6, SRB7, B8, SRB11, SRB12))
  filenameCommonBands <- paste0(site, '_est_CommonBands_B8A-NIR_', sat_date, '.tiff')
  # writeRaster(s2_dtm_combined_common, paste0('Upscaling/Inputs/Sentinel_maps_v2/', filenameCommonBands),
  #             overwrite = T)
  
## One-hot ----
  
  # Add the site one-hot
  if (site == 'AE'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'AH'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'CP'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
  }
  
  # Add the established one-hot
  s2_dtm_combined_1hot <- s2_dtm_combined_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 1.0),
           Est_New = ifelse(is.na(DTM), NA, 0.0))
  s2_dtm_combined_common_1hot <- s2_dtm_combined_common_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 1.0),
           Est_New = ifelse(is.na(DTM), NA, 0.0))
  
  # Write to drive
  # writeRaster(s2_dtm_combined_1hot, paste0('Upscaling/Inputs/Sentinel_maps_v2/One-hot/', filenameAllBands),
  #             overwrite = T)
  # writeRaster(s2_dtm_combined_common_1hot, paste0('Upscaling/Inputs/Sentinel_maps_v2/One-hot/', filenameCommonBands),
  #             overwrite = T)
  
  
  ## Extract cover ----  
  
  # Import the UAV image
  uav <- rast(UAV_est[i])
  
  # Extract the values per pixel
  cat('\nextracting values...\n')
  Values <- extractCoverPerPixel(classified_image = uav,
                                 satellite_image = s2_dtm_combined,
                                 AOI = aoi)
  
  # Add extra data
  Values <- Values %>%
    mutate(Site = site,
           Est = 'Est',
           Sat_date = sat_date,
           UAV_date = uav_date)
  
  # Append to the larger data frame
  est_full <- bind_rows(est_full, Values)
  
  # beep(10)
  
}

# beep(1)

# New loop ----------------------------------------------------------------

cat('\n===========\nNEW\n===========\n')

# Initialise a tibble to store all the results
new_full <- tibble()

for (i in names(s2_images)){
  cat('\n', i, '\n-------------\n', sep = '')
  
  # Extract the metadata
  sat_date <- str_sub(i, start = 4)
  site <- substr(i, start = 1, stop = 2)
  uav_date = uav_dates_new[i]
  
  # Import the AOI
  aoi <- vect(aoi_new_paths[i])
  
  # Extract the s2 image
  s2 <- s2_images[[i]]
  
  # Rename the s2 bands
  if(names(s2) %>% length() == 11){
    s2 <- subset(s2, -1)
  }
  names(s2) <- c('B2', 'B3', 'B4', 'SRB5', 'SRB6', 'SRB7', 'B8', 'B8A', 'SRB11', 'SRB12')
  
  # Set OSGB36 CRS
  OSGB36 <- "+init=EPSG:27700"
  
  # Convert Sentinel CRS to OSGB36
  s2 <- terra::project(s2, OSGB36)
  
  # Clip s2 to the aoi
  s2_clip <- mask(crop(s2, aoi), aoi, touches = F)
  
## Vegetation indices ----
  
  # Extract the bands
  B <- s2_clip[["B2"]]
  G <- s2_clip[["B3"]]
  R <- s2_clip[["B4"]]
  Redg <- s2_clip[["SRB5"]]
  NIR <- s2_clip[["B8"]]
  
  # Normalized Difference Vegetation Index
  NDVI <- (NIR - R) / (NIR + R)
  # Green Difference Vegetation Index
  GDVI <- NIR - G
  # Green Normalized Vegetation Index
  GNDVI <- (NIR - G) / (NIR + G)
  # Green Ratio Vegetation Index
  GRVI <- NIR / G
  # Green Infrared Percentage Vegetation Index
  GIPVI <- NIR / (NIR + G)
  # Simple Ratio
  SR <- NIR / R
  # Green Difference Index
  GDI <- NIR - R + G
  # Green Red Difference Index
  GRDI <- (G - R) / (G + R)
  # Red edge normalized difference vegetation index
  NDVIre <- (NIR - Redg) / (NIR + Redg)
  # Red edge simple ratio
  SRre <- NIR / Redg
  # Red edge triangular vegetation index (core only)
  RTVIcore <- 100 * (NIR - Redg) - 10 * (NIR - G)
  # MSRred edge
  MSRedg <- (NIR / Redg) - 1 / sqrt(NIR / Redg) + 1
  # Datt4
  Datt4 <- R / G * Redg
  # MSAVI2
  MSAVI2 <- (2 * NIR + 1 - sqrt ((2 * NIR + 1)^2 - 8 * (NIR - R))) / 2
  # EVI
  EVI <-  2.5*((NIR-R)/(NIR+6*R-7.5*B+1))
  
  # Append to Sentinel data
  s2_clip <- c(s2_clip, NDVI, GDVI, GNDVI,  
               # GIPVI, MSRedg, SR, MSAVI2, GRVI,
               GDI, GRDI, 
               NDVIre, SRre, RTVIcore,  Datt4, EVI)
  
  # Remove VIs from scope
  rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
     NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  
  # Add the VI names
  names(s2_clip) <- c(names(s2), 'NDVI', 'GDVI', 'GNDVI', 
                      # 'GIPVI', 'SR', 'MSRedg', 'MSAVI2', 'GRVI', 
                      'GDI', 'GRDI', 'NDVIre', 'SRre', 'RTVIcore', 'Datt4', 'EVI')
  
  # Import the DTM
  # stop('DTM or DTM?')
  dtm <- rast(dtm_paths[i])
  crs(dtm) <- crs(aoi)
  
  # Resample the dtm to s2
  dtm_ag <- aggregate(dtm, res(s2)[1]/res(dtm)[1])
  dtm_s2 <- resample(dtm_ag, s2)
  
  # Clip the dtm to the aoi
  dtm_s2_clip = mask(crop(dtm_s2, aoi), aoi, touches = F)
  
  # Rename the DTM layer
  names(dtm_s2_clip) <- 'DTM'
  
  # Combine the s2 and dtm
  s2_dtm_combined <- c(s2_clip, dtm_s2_clip)
  
  # Write the satellite image to drive
  filenameAllBands <- paste0(site, '_new_AllBands_B8A-NIR_', sat_date, '.tiff')
  # writeRaster(s2_dtm_combined, paste0('Upscaling/Inputs/Sentinel_maps_v2/', filenameAllBands),
  #             overwrite = T)
  
  # Filter away the non common bands and write to drive
  s2_dtm_combined_common <- s2_dtm_combined %>%
    select(-c(SRB6, SRB7, B8, SRB11, SRB12))
  filenameCommonBands <- paste0(site, '_new_CommonBands_B8A-NIR_', sat_date, '.tiff')
  # writeRaster(s2_dtm_combined_common, paste0('Upscaling/Inputs/Sentinel_maps_v2/', filenameCommonBands),
  #             overwrite = T)
  
## One-hot ----
  
  # Add the site one-hot
  if (site == 'AE'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'AH'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'CP'){
    s2_dtm_combined_1hot <- s2_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
    s2_dtm_combined_common_1hot <- s2_dtm_combined_common %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
  }
  
  # Add the established one-hot
  s2_dtm_combined_1hot <- s2_dtm_combined_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 0.0),
           Est_New = ifelse(is.na(DTM), NA, 1.0))
  s2_dtm_combined_common_1hot <- s2_dtm_combined_common_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 0.0),
           Est_New = ifelse(is.na(DTM), NA, 1.0))
  
  # Write to drive
  # writeRaster(s2_dtm_combined_1hot, paste0('Upscaling/Inputs/Sentinel_maps_v2/One-hot/', filenameAllBands),
  #             overwrite = T)
  # writeRaster(s2_dtm_combined_common_1hot, paste0('Upscaling/Inputs/Sentinel_maps_v2/One-hot/', filenameCommonBands),
  #             overwrite = T)
  

## Extract cover ----  
  
  # Import the UAV image
  uav <- rast(UAV_new[i])
  
  # Extract the values per pixel
  cat('\nextracting values...\n')
  Values <- extractCoverPerPixel(classified_image = uav,
                                 satellite_image = s2_dtm_combined,
                                 AOI = aoi)
  
  # Add extra data
  Values <- Values %>%
    mutate(Site = site,
           Est = 'New',
           Sat_date = sat_date,
           UAV_date = uav_date)
  
  # Append to the larger data frame
  new_full <- bind_rows(new_full, Values)
  
  # beep(10)
  
}

# beep(1)


# Wrangling ---------------------------------------------------------------

# Combine the new and est layers
combined <- bind_rows(est_full, new_full)

# Work out the ULMH cover
combined <- combined %>%
  mutate(Unvegetated = rowSums(select(., Water, Bare_ground, Algae, Fucoids, Shadow), na.rm = T),
         Low = rowSums(select(., SM6, SM8, `SM8/9/11`, SM10), na.rm = T),
         Mid = rowSums(select(., SM11, SM13c, SM14a, SM14c), na.rm = T),
         High = rowSums(select(., SM24), na.rm = T)) %>%
  mutate(across(Unvegetated:High, function(x) round(x, 3)))

# Filter to only pixels with 100% cover of the UAV map
combined <- combined %>%
  mutate(Total = rowSums(select(., Unvegetated:High))) %>%
  filter(Total == 1)

# Create a function to classify the cover type
cover_class <- function(x) {
  case_when(
    x == 0 ~ 'Absent',
    x == 1 ~ 'Dominant',
    x > 0 & x < 1 ~ 'In-between',
    .default = NA
  )
}
# Create the cover classes
combined <- combined %>%
  mutate(across(Unvegetated:High, cover_class, .names = '{.col}_class'))

# Remove ID
combined <- combined %>%
  select(-ID)

# Write csv to drive
write_csv(combined, 'data/interim/satellite_values/Combined_Sentinel_values_AllBands_B8A-NIR.csv')

# Produce a version with only the common bands and write to drive
combinedCommonBands <- combined %>%
  select(-c(SRB6, SRB7, B8, SRB11, SRB12))
write_csv(combinedCommonBands, 'data/interim/satellite_values/Combined_Sentinel_values_CommonBands_B8A-NIR.csv')
