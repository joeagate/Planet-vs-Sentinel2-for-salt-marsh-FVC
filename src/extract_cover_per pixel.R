library(terra)
library(dplyr)

extractCoverPerPixel <- function(classified_image, satellite_image, AOI){
    
  # Force the name of the classified image to class
  names(classified_image) <- 'class'
  
  # Crop the satellite image to the AOI
    sat_crop <- mask(crop(satellite_image, AOI), AOI, touches = F)
    
    # Convert the satellite pixels into vectors
    sat_poly <- as.polygons(sat_crop, dissolve = F)
    
    # Extract the values from the classified raster
    Values_class <- terra::extract(classified_image, sat_poly, xy = TRUE)
    
    # Find the proportion of each class per polygon
    Values_class_prop <- Values_class %>%
      tibble() %>%
      group_by(ID, class) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      group_by(ID) %>%
      mutate(prop = n / sum(n)) %>%
      select(-n) %>%
      pivot_wider(names_from = class, values_from = prop) %>%
      select(-`NA`)
    
    # Convert NAs to 0s
    Values_class_prop[is.na(Values_class_prop)] <- 0
    
    # Find the centroids of the Values_class
    Values_class_centroids <- Values_class %>%
      group_by(ID) %>%
      summarise(across(x:y, mean))
    
    # Get the centroids of each of the satellite pixels
    Centroids <- centroids(sat_poly)
    
    # Extract the satellite pixel values
    Values_sat <- terra::extract(satellite_image, Centroids, xy = TRUE, touches = TRUE)

    # Combine the two data frames
    ValueTable <- Values_sat %>%
      full_join(Values_class_prop, by = "ID") %>%
      tibble()
    
    return(ValueTable)

}
