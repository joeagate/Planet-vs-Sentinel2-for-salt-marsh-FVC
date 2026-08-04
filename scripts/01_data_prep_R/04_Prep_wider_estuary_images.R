# Set up ------------------------------------------------------------------

# Import packages
library(tidyverse)
library(terra)
library(tidyterra)
library(here)
library(glue)

setwd(here())


# Set data inputs ---------------------------------------------------------

# Find the satellite images
files <- list.files('data/external/Sentinel_images_super_resolved/Extra_Pub',
                    pattern = '.tif$', full.names = T)

# Create a nambed vector for the dtm paths
dtm_paths <- c(
  "AE_20170601.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20180202.tif",
  "AE_20180628.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20180202.tif",
  "AE_20190723.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20200313.tif",
  "AE_20200625.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20200313.tif",
  "AE_20210717.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif",
  "AE_20220710.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20211107.tif",
  "AE_20230707.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20240212.tif",
  "AE_20240729.tif" = "data/external/Lidar/Adur/DTM_Adur_lidar_tiles_20240212.tif",
  "AH_20180703.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20181108.tif",
  "AH_20190723.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20200214.tif",
  "AH_20200625.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20200214.tif",
  "AH_20210717.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20211117.tif",
  "AH_20220622.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20211117.tif",
  "AH_20230707.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20230422.tif",
  "AH_20240729.tif" = "data/external/Lidar/Abbotts_Hall/DTM_AH_lidar_tiles_20230422.tif",
  "CP_20170601.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20180319.tif",
  "CP_20180621.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20180319.tif",
  "CP_20190606.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20180319.tif",
  "CP_20200625.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20200919.tif",
  "CP_20210720.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif",
  "CP_20220809.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif",
  "CP_20230625.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif",
  "CP_20240729.tif" = "data/external/Lidar/Cobnor/DTM_Cobnor_lidar_tiles_20211107.tif"
)

# Set the paths for the aoi
aoi_paths <- c(
  "AE" = "data/raw/aois/Adur_Estuary/Adur_est_3km_buffer_clipped.shp",
  "AH" = "data/raw/aois/Abbotts_Hall/AH_est_area_3km_buffer.shp",
  "CP" = "data/raw/aois/Cobnor_Point/Cobnor_est_area_3km_buffer.shp"
)

# Set the paths for the restored areas
rest_paths <- c(
  "AE" = "data/raw/aois/Adur_Estuary/W7_restoration_unaffected_by_NMF_works.shp",
  "AH" = "data/raw/aois/Abbotts_Hall/Salcott_restored.shp",
  "CP" = "data/raw/aois/Cobnor_Point/Cobnor_new_AOI.shp"
)


# Loop through the files --------------------------------------------------

# Remove the 2018 CP image due to clouds
files <- files[grepl("CP_2018", files) == F]

for (file in files) {
  print(file)
  
  # Extract metadata
  img_name <- gsub(".*/", "", file)
  site <- substr(img_name, start = 1, stop = 2)
  date <- parse_number(img_name)
  
  # Import the data
  sat <- rast(file)
  dtm <- rast(dtm_paths[img_name])
  aoi <- vect(aoi_paths[site])
  rest <- vect(rest_paths[site])
  
  # Reproject the satellite
  sat <- terra::project(sat, "+init=EPSG:27700")
  
  # Reshape bands
  if(grepl('Sentinel', file) == TRUE){
    
    # Set the sat type
    sat_type <- 'Sentinel'
    
    # Rename the s2 bands
    if(names(sat) %>% length() == 11){sat <- subset(sat, -1)}
    names(sat) <- c('B2', 'B3', 'B4', 'SRB5', 'SRB6', 'SRB7', 'B8', 'SRB8A', 'SRB11', 'SRB12')
    
    # Clip the satellite to the aoi
    sat_crop <- mask(crop(sat, aoi), aoi, touches = F)
    
    # Extract the bands
    B <- sat_crop[["B2"]]
    G <- sat_crop[["B3"]]
    R <- sat_crop[["B4"]]
    Redg <- sat_crop[["SRB5"]]
    NIR <- sat_crop[["SRB8A"]]
    
  } else stop('Unrecognised satellite type')
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Calculate VIs
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
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
  # NDWI
  NDWI <- (G - NIR)/(G + NIR)
  
  # Append to Sentinel data
  sat_crop <- c(sat_crop, NDVI, GDVI, GNDVI,  
                # GIPVI, MSRedg, SR, MSAVI2, GRVI,
                GDI, GRDI, 
                NDVIre, SRre, RTVIcore,  Datt4, EVI)
  
  # Remove VIs from scope
  rm(B, G, NIR, R, Redg, NDVI, GDVI, GNDVI, GRVI, GIPVI, SR, GDI, GRDI, 
     NDVIre, SRre, RTVIcore, MSRedg, Datt4, MSAVI2, EVI)
  
  # Add the VI names
  names(sat_crop) <- c(
    names(sat),
    'NDVI',
    'GDVI',
    'GNDVI',
    # 'GIPVI',
    # 'MSRedg',
    # 'SR',
    # 'MSAVI2',
    # 'GRVI',
    'GDI',
    'GRDI',
    'NDVIre',
    'SRre',
    'RTVIcore',
    'Datt4',
    'EVI'
  )
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # DTM
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Convert DTM crs to OSGB
  crs(dtm) <- crs(aoi)
  
  # Resample the DTM to the resolution of the satellite
  dtm <- resample(dtm, sat_crop)
  
  # Fill any gaps in the raster
  dtm_focal <- focal(dtm, w=15, 'modal', na.policy = 'only')
  
  # Crop the DTM to the aoi
  dtm_crop <- mask(crop(dtm_focal, aoi), aoi, touches = F)
  
  # Rename the DTM layer
  names(dtm_crop) <- 'DTM' 
  
  # Join to the satellite image
  sat_dtm <- c(sat_crop, dtm_crop)
  names(sat_dtm)
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # On-hot encoding
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Site
  # %%%%
  if (site == 'AE'){
    sat_dtm_1hot <- sat_dtm %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 1.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'AH'){
    sat_dtm_1hot <- sat_dtm %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 1.0),
             Site_CP = ifelse(is.na(DTM), NA, 0.0))
  } else if (site == 'CP'){
    sat_dtm_1hot <- sat_dtm %>%
      mutate(Site_AE = ifelse(is.na(DTM), NA, 0.0),
             Site_AH = ifelse(is.na(DTM), NA, 0.0),
             Site_CP = ifelse(is.na(DTM), NA, 1.0))
  }
  
  # Established
  # %%%%%%%%%%%

  # Create binary layers
  est_binary <- rasterize(rest, sat_crop, field = 0, background = 1) %>%
    mask(aoi, touches = F) # Mask to aoi
  rest_binary <- rasterize(rest, sat_crop, field = 1, background = 0) %>%
    mask(aoi, touches = F) # Mask to aoi
  
  # Rename the rest binary
  names(est_binary) <- 'Est_Est'
  names(rest_binary) <- 'Est_New'
  
  # Add to the sat image
  sat_dtm_1hot <- c(sat_dtm_1hot, est_binary, rest_binary)
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Water masking
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Make a water polygon
  water <- as.polygons(NDWI>0)
  water <- (water[water$B3 == 1])
  
  # Mask to non-water areas
  sat_dtm <- mask(sat_dtm, water, inverse = T, touches = F)
  sat_dtm_1hot <- mask(sat_dtm_1hot, water, inverse = T, touches = F)
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Save to drive
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Create a filename for saving
  aoi_name <-  sub(".*/", "", aoi_paths[site]) %>%
    str_remove_all('.shp')
  filename <- glue('{site}_{date}_{sat_type}_CommonBands_{aoi_name}.tiff')
  
  # Common bands
  sat_dtm_common <- sat_dtm %>%
    select(-c(SRB6, SRB7, B8, SRB11, SRB12))
  sat_dtm_1hot_common <- sat_dtm_1hot %>%
    select(-c(SRB6, SRB7, B8, SRB11, SRB12))
  
  # Write to drive
  writeRaster(sat_dtm_common, paste0(
    'data/interim/wider_estuary/notonehot/', filename
  ), overwrite = TRUE)
  # print(paste('Saved', filename, 'to drive'))
  writeRaster(sat_dtm_1hot_common, paste0(
    'data/interim/wider_estuary/onehot/', filename
  ), overwrite = TRUE)
  print(paste('Saved', filename, 'to drive'))
}


