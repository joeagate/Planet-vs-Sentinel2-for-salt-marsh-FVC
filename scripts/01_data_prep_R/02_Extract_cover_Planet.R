# Set up ------------------------------------------------------------------

# Load packages
library(readxl)
library(tidyverse)
library(terra)
library(here)
library(beepr)
library(tidyterra)

# Reset wd
setwd(here())

# Load the extraction script
source("src/extract_cover_per pixel.R")


# Wrangling ---------------------------------------------------------------

# Import the images
files <- list.files(
  pattern = ".tif$",
  path = "data/external/Planet_images_harmonised/",
  full.names = TRUE
)
planet_images <- lapply(files, rast)

# Extract the image ids
image_ids <- str_remove_all(files, "_3B_AnalyticMS_SR_8b_harmonized_clip.tif")
image_ids <- str_remove_all(image_ids, "data/external/Planet_images_harmonised//")
names(planet_images) <- image_ids # Set names for Planet images as IDs

# Import the Planet order details as metadata
metadata <- read_xlsx("data/external/Planet_images_harmonised/Planet order.xlsx")

# Wrangle the metadata into a better format
metadata <- metadata %>%
  drop_na(Time) %>% # Remove the rows with NA in time as they do not contain data
  filter(Site != "AHF") %>% # Remove the double-counting of the AH sites
  mutate(Date = as.Date(Date)) %>% # Change Data from datetime to date format
  mutate(Site = case_match(Site, "AHS" ~ "AH", "Adur" ~ "AE", "Cobnor" ~ "CP",
                           .default = Site)) %>% # Change the site name format
  filter(ID %in% image_ids) %>% # Filter to the dates which are wanted
  mutate(Year = year(Date), Month = month(Date))


## Named vectors ----

# AOI est
aoi_est_paths <- c("20210718_101432_37_2465" = "data/raw/aois/Adur_Estuary/W7_established.shp",
                   "20220620_100628_00_2428" = "data/raw/aois/Adur_Estuary/W7_established.shp",
                   "20230729_101035_28_24b0" = "data/raw/aois/Adur_Estuary/W7_established.shp",
                   "20230707_100822_17_24cf" = "data/raw/aois/Abbotts_Hall/AHS_AOI.shp",
                   "20230712_101447_62_24af" = "data/raw/aois/Cobnor_Point/AOI_CobEst.shp")

# AOI new
aoi_new_paths <- c("20210718_101432_37_2465" = "data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp",
                   "20220620_100628_00_2428" = "data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp",
                   "20230729_101035_28_24b0" = "data/raw/aois/Adur_Estuary/W7_restoration_after_NMF_works.shp",
                   "20230707_100822_17_24cf" = "data/raw/aois/Abbotts_Hall/AHF_AOI.shp",
                   "20230712_101447_62_24af" = "data/raw/aois/Cobnor_Point/Cobnor_new_AOI.shp")

# UAV NVC maps est
uav_est_paths <- c("20210718_101432_37_2465" = "data/raw/uav_maps/majority_filter/AE_est21_predicted_NVC.tif",
                   "20220620_100628_00_2428" = "data/raw/uav_maps/majority_filter/AE_est22_predicted_NVC.tif",
                   "20230729_101035_28_24b0" = "data/raw/uav_maps/majority_filter/AE_est23_predicted_NVC.tif",
                   "20230707_100822_17_24cf" = "data/raw/uav_maps/majority_filter/AH_est23_predicted_NVC.tif",
                   "20230712_101447_62_24af" = "data/raw/uav_maps/majority_filter/CP_est23_predicted_NVC.tif")

# UAV NVC maps new
uav_new_paths <- c("20210718_101432_37_2465" = "data/raw/uav_maps/majority_filter/AE_new21_predicted_NVC.tif",
                   "20220620_100628_00_2428" = "data/raw/uav_maps/majority_filter/AE_new22_predicted_NVC.tif",
                   "20230729_101035_28_24b0" = "data/raw/uav_maps/majority_filter/AE_new23_predicted_NVC.tif",
                   "20230707_100822_17_24cf" = "data/raw/uav_maps/majority_filter/AH_new23_predicted_NVC.tif",
                   "20230712_101447_62_24af" = "data/raw/uav_maps/majority_filter/CP_new23_predicted_NVC.tif")

# Lidar paths
dtm_paths <- c("20210718_101432_37_2465" = "Lidar/Adur/DTM_Adur_lidar_tiles_F0222857_2021-11-07.tif",
               "20220620_100628_00_2428" = "Lidar/Adur/DTM_Adur_lidar_tiles_F0222857_2021-11-07.tif",
               "20230729_101035_28_24b0" = "Lidar/Adur/DTM_Adur_lidar_tiles_20240212.tif",
               "20230707_100822_17_24cf" = "Lidar/Abbotts Hall/DTM_AH_lidar_tiles_20230422.tif",
               "20230712_101447_62_24af" = "Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif")

dtm_paths <- c("20210718_101432_37_2465" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif",
               "20220620_100628_00_2428" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif",
               "20230729_101035_28_24b0" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20240212.tif",
               "20230707_100822_17_24cf" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20230422.tif",
               "20230712_101447_62_24af" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif")



# Established loop --------------------------------------------------------

cat("\n===========\nESTABLISHED\n===========\n")

# Initialise a tibble to store the results
est_results <- tibble()

for (i in image_ids) {
  cat("\n", i, "\n-----------------------\n", sep = "")
  
  # Filter to the metadata for i
  meta_i <- metadata %>%
    filter(ID == i)
  
  # Import the AOI
  aoi <- vect(aoi_est_paths[i])
  
  # Extract the Planet image
  p <- planet_images[[i]]
  
  # Filter and arrange the bands in the Planet image
  p <- p %>%
    select(blue, green_i, green, yellow, red, rededge, nir)
  
  # Set OSGB36 CRS
  OSGB36 <- "+init=EPSG:27700"
  
  # Convert the Planet image to OSGB36
  p <- terra::project(p, OSGB36)
  
  # Clip the Planet image to the AOI
  p_clip <- mask(crop(p, aoi), aoi, touches = F)
  

## Vegetation indices ----
  
  # Extract the bands
  B <- p_clip[["blue"]]
  G <- p_clip[["green"]]
  R <- p_clip[["red"]]
  Redg <- p_clip[["rededge"]]
  NIR <- p_clip[["nir"]]
  
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
  
  # Append to Planet data
  p_clip <- c(p_clip, NDVI, GDVI, GNDVI,  
               # GIPVI, MSRedg, SR, MSAVI2, GRVI,
               GDI, GRDI, 
               NDVIre, SRre, RTVIcore,  Datt4, EVI)
  
  # Remove VIs from scope
  rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
     NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  
  # Add the VI names
  names(p_clip) <- c(names(p), "NDVI", "GDVI", "GNDVI", 
                      # "GIPVI", "SR", "MSRedg", "MSAVI2", "GRVI", 
                      "GDI", "GRDI", "NDVIre", "SRre", "RTVIcore", "Datt4", "EVI")
  
  # Import the dtm
  dtm <- rast(dtm_paths[i])
  crs(dtm) <- crs(aoi)
  
  # Resample the dtm to Planet res
  dtm_p <- resample(dtm, p_clip)
  
  # Clip the dtm to the aoi
  dtm_p_clip <- mask(crop(dtm_p, aoi), aoi, touches = F)
  
  # Add band name to the dtm
  names(dtm_p_clip) <- "DTM"
  
  # Combine P with the DTM
  p_dtm_combined <- c(p_clip, dtm_p_clip)
  
  # Write the file to drive
  # filenameAllBands <- paste0(meta_i$Site, "_est_AllBands_", i, ".tiff")
  # writeRaster(p_dtm_combined, paste0("Upscaling/Inputs/Planet_maps/", filenameAllBands),
  #             overwrite = T)
  
  # Create a version with only consistent bands and write to drive
  p_dtm_combined_CommonBands <- p_dtm_combined %>%
    select(-c(green_i, yellow))
  # filenameCommonBands <- paste0(meta_i$Site, "_est_CommonBands_", i, ".tiff")
  # writeRaster(p_dtm_combined_CommonBands, paste0("Upscaling/Inputs/Planet_maps/", filenameCommonBands),
  #             overwrite = T)
  
  ## One-hot ----
  
  # Extract the site
  site <- meta_i$Site
  
  # Add the site one-hot
  if (site == "AE"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == "AH"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == "CP"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
  }
  
  # Add the established one-hot
  p_dtm_combined_1hot <- p_dtm_combined_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 1.0),
           Est_New = ifelse(is.na(DTM), NA, 0.0))
  p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 1.0),
           Est_New = ifelse(is.na(DTM), NA, 0.0))
  
  # Write to drive
  # writeRaster(p_dtm_combined_1hot, paste0("Upscaling/Inputs/Planet_maps/One-hot/", filenameAllBands),
  #             overwrite = T)
  # writeRaster(p_dtm_combined_CommonBands_1hot, paste0("Upscaling/Inputs/Planet_maps/One-hot/", filenameCommonBands),
  #             overwrite = T)
  
  
  ## Extract cover ----  
  
  # Import the UAV NVC map
  uav <- rast(uav_est_paths[i])
  
  # Extract the values per pixel
  cat("\nextracting values...\n")
  Values <- extractCoverPerPixel(classified_image = uav,
                                 satellite_image = p_dtm_combined,
                                 AOI = aoi)
  
  # Add extra data
  Values <- Values %>%
    mutate(Site = meta_i$Site,
           Est = "Est",
           Sat_date = substr(i, start = 1, stop = 8) %>% ymd(),
           UAV_date = meta_i$`Drone survey`)
  
  # Append to the full dataframe
  est_results <- bind_rows(est_results, Values)
  
  beep(10)
}

beep(1)



# New loop --------------------------------------------------------

cat("\n===========\nNEW\n===========\n")

# Initialise a tibble to store the results
new_results <- tibble()

for (i in image_ids) {
  cat("\n", i, "\n-----------------------\n", sep = "")
  
  # Filter to the metadata for i
  meta_i <- metadata %>%
    filter(ID == i)
  
  # Import the AOI
  aoi <- vect(aoi_new_paths[i])
  
  # Extract the Planet image
  p <- planet_images[[i]]
  
  # Filter and arrange the bands in the Planet image
  p <- p %>%
    select(blue, green_i, green, yellow, red, rededge, nir)
  
  # Set OSGB36 CRS
  OSGB36 <- "+init=EPSG:27700"
  
  # Convert the Planet image to OSGB36
  p <- terra::project(p, OSGB36)
  
  # Clip the Planet image to the AOI
  p_clip <- mask(crop(p, aoi), aoi, touches = F)
  
  
  ## Vegetation indices ----
  
  # Extract the bands
  B <- p_clip[["blue"]]
  G <- p_clip[["green"]]
  R <- p_clip[["red"]]
  Redg <- p_clip[["rededge"]]
  NIR <- p_clip[["nir"]]
  
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
  
  # Append to Planet data
  p_clip <- c(p_clip, NDVI, GDVI, GNDVI,  
              # GIPVI, MSRedg, SR, MSAVI2, GRVI,
              GDI, GRDI, 
              NDVIre, SRre, RTVIcore,  Datt4, EVI)
  
  # Remove VIs from scope
  rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
     NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  
  # Add the VI names
  names(p_clip) <- c(names(p), "NDVI", "GDVI", "GNDVI", 
                     # "GIPVI", "SR", "MSRedg", "MSAVI2", "GRVI", 
                     "GDI", "GRDI", "NDVIre", "SRre", "RTVIcore", "Datt4", "EVI")
  
  # # Append to Sentinel data
  # p_clip <- c(p_clip, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
  #             NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  # 
  # # Remove VIs from scope
  # rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
  #    NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  # 
  # # Add the VI names
  # names(p_clip) <- c(names(p), "NDVI", "GDVI", "GNDVI", "GRVI", "GIPVI", "SR", 
  #                    "GDI", "GRDI", "NDVIre", "SRre", "RTVIcore", "MSRedg", "Datt4", "MSAVI2", "EVI")
  
  # Import the dtms
  dtm <- rast(dtm_paths[i])
  crs(dtm) <- crs(aoi)
  
  # Resample the dtm to Planet res
  dtm_p <- resample(dtm, p_clip)
  
  # Clip the dtm to the aoi
  dtm_p_clip <- mask(crop(dtm_p, aoi), aoi, touches = F)
  
  # Add band name to the dtm
  names(dtm_p_clip) <- "DTM"
  
  # Combine P with the DTM
  p_dtm_combined <- c(p_clip, dtm_p_clip)
  
  # Write the file to drive
  # filenameAllBands <- paste0(meta_i$Site, "_new_AllBands_", i, ".tiff")
  # writeRaster(p_dtm_combined, paste0("Upscaling/Inputs/Planet_maps/", filenameAllBands),
  #             overwrite = T)
  
  # Create a version with only common bands (with S2) and write to drive
  p_dtm_combined_CommonBands <- p_dtm_combined %>%
    select(-c(green_i, yellow))
  # filenameCommonBands <- paste0(meta_i$Site, "_new_CommonBands_", i, ".tiff")
  # writeRaster(p_dtm_combined_CommonBands, paste0("Upscaling/Inputs/Planet_maps/", filenameCommonBands),
  #             overwrite = T)
  # 
## One-hot ----
  
  # Extract the site
  site <- meta_i$Site
  
  # Add the site one-hot
  if (site == "AE"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == "AH"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == "CP"){
    p_dtm_combined_1hot <- p_dtm_combined %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
    p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
  }
  
  # Add the established one-hot
  p_dtm_combined_1hot <- p_dtm_combined_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 0.0),
           Est_New = ifelse(is.na(DTM), NA, 1.0))
  p_dtm_combined_CommonBands_1hot <- p_dtm_combined_CommonBands_1hot %>%
    mutate(Est_Est = ifelse(is.na(DTM), NA, 0.0),
           Est_New = ifelse(is.na(DTM), NA, 1.0))
  
  # Write to drive
  # writeRaster(p_dtm_combined_1hot, paste0("Upscaling/Inputs/Planet_maps/One-hot/", filenameAllBands),
  #             overwrite = T)
  # writeRaster(p_dtm_combined_CommonBands_1hot, paste0("Upscaling/Inputs/Planet_maps/One-hot/", filenameCommonBands),
  #             overwrite = T)
  
  
## Extract cover ----  
  
  # Import the UAV NVC map
  uav <- rast(uav_new_paths[i])
  
  # Extract the values per pixel
  cat("\nextracting values...\n")
  Values <- extractCoverPerPixel(classified_image = uav,
                                 satellite_image = p_dtm_combined,
                                 AOI = aoi)
  
  # Add extra data
  Values <- Values %>%
    mutate(Site = meta_i$Site,
           Est = "New",
           Sat_date = substr(i, start = 1, stop = 8) %>% ymd(),
           UAV_date = ifelse(i == "20230707_100822_17_24cf", "2023-08-10", as.character(meta_i$`Drone survey`)))
  
  # Append to the full dataframe
  new_results <- bind_rows(new_results, Values)
  
  beep(10)
}

beep(1)



# Wrangling ---------------------------------------------------------------

# Combine the new and est layers
combined <- bind_rows(est_results %>% mutate(UAV_date = as.character(UAV_date)), new_results)

# Work out the ULMH cover
combined <- combined %>%
  mutate(Unvegetated = rowSums(select(., Water, Bare_ground, Algae, Fucoids, Shadow), na.rm = T),
         Low = rowSums(select(., SM6, SM8, `SM8/9/11`, SM10), na.rm = T),
         Mid = rowSums(select(., SM11, SM13c, SM14a, SM14c), na.rm = T),
         High = rowSums(select(., SM24), na.rm = T)) %>%
  mutate(across(Unvegetated:High, function(x) round(x, 3))) %>%
  select(-c(Bare_ground, SM11), -c(`SM8/9/11`:SM8))

# Filter to only pixels with 100% cover of the UAV map
combined <- combined %>%
  mutate(Total = rowSums(select(., Unvegetated:High))) %>%
  filter(Total == 1)

# Create a function to classify the cover type
cover_class <- function(x) {
  case_when(
    x == 0 ~ "Absent",
    x == 1 ~ "Dominant",
    x > 0 & x < 1 ~ "In-between",
    .default = NA
  )
}
# Create the cover classes
combined <- combined %>%
  mutate(across(Unvegetated:High, cover_class, .names = "{.col}_class"))

# Remove ID
combined <- combined %>%
  select(-ID)

# Write the values to drive as a csv
write_csv(
  combined,
  "data/interim/satellite_values/Combined_Planet_values_AllBands.csv"
)

# Produce a version with only the common bands and write to drive
combined_common_bands <- combined %>%
  select(-c(green_i, yellow))
write_csv(
  combined_common_bands, 
  "data/interim/satellite_values/Combined_Planet_values_CommonBands.csv"
)
