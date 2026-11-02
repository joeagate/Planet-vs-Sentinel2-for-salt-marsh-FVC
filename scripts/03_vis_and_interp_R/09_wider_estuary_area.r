# Code to save the figures has been commented out. Remove these comments if you
# wish to save the figures.

# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(terra)
library(tidyterra)
library(cowplot)
library(glue)
library(here)
library(beepr)
library(ggnewscale)

home <- here()
setwd(home)

# Set plotting theme
theme_set(theme_cowplot(7))
theme_update(strip.background = element_blank())



# Import data -------------------------------------------------------------

# Classified maps
files <- list.files(pattern = '.tiff$',
                    path = 'data/processed/wider_estuary_cover_maps/onehot/',
                    full.names = T)

# Paths to land area maps
land_paths <- c(
  'AE' = 'data/external/OS_high_water/OS_hw_AE.shp',
  'AH' = 'data/external/OS_high_water/OS_hw_AH.shp',
  'CP' = 'data/external/OS_high_water/OS_hw_CP.shp'
)


# For loop ----------------------------------------------------------------

# Init a new tibble
full_cover <- tibble()

# Init a list to store the line plots
line_plts_list <- list()

# Create named vector for the site long names
site_long_names <- c('AE' = 'AE', 'CP' = 'CH', 'AH' = 'BE')

# Init a to 0
a <- 0

# Extract the dates to keep axes the same for the lines plt
all_dates <- str_extract(files, "\\d{8}")
all_dates <- as.Date(all_dates, format = '%Y%m%d')

for (site in c(
  'AE',
  'CP',
  'AH'
  )) {
  cat(site, '\n')
  # if(site == 'AH') {stop('Steady on now')}
  a <- a+1
  
  # Subset the files in the directory to those from the site
  files_site <- str_subset(files, site)
  
  # Import the files as rasters
  rasters <- lapply(files_site, rast)
  
  # Extract the dates from the files
  dates <- str_extract(files_site, "\\d{8}")
  dates <- as.Date(dates, format = '%Y%m%d')
  
  # Import the rest_area
  if (site == 'AE') {
    rest_area <- vect('./data/raw/aois/Adur_Estuary/W7_restoration_unaffected_by_NMF_works.shp')
  } else if(site == 'CP') {
    rest_area <- vect('./data/raw/aois/Cobnor_Point/Cobnor_new_AOI.shp')
  } else if(site == 'AH') {
    rest_area <- vect('./data/raw/aois/Abbotts_Hall/Salcott_restored.shp')
  }
  
  # Init a site tbl
  site_full_cover <- tibble()
  
  # Init lists to store the habitat classification plots
  hab_class_full <- list()
  hab_class_new <- list()
  
  # Import the land shapefile
  land <- vect(land_paths[site])
  land_new <- crop(land, ext(rest_area))
  
  
  # Create a mask for the non-water areas
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Create a new mask
  mask <- app(!is.na(rasters[[1]]), all)
  
  # Update the mask to match each raster
  for (i in 2:length(rasters)) {
    rasters[[i]] <- resample(rasters[[i]], rasters[[1]])
    mask <- mask & app(!is.na(rasters[[i]]), all)
  }
  
  # Apply the mask to the rasters
  mask <- as.polygons(mask)
  mask <- mask[mask$all == T]
  rasters <- lapply(rasters, mask, mask)
  
  # Loop through the plots
  for (i in 1:length(dates)) {
    
    # Extract the date
    date <- dates[i]
    cat(date |> as.character(), '\n')
    
    # Subset the raster to the specific date
    raster_date <- rasters[[i]]
    
    # Subset the raster into new marsh and established
    new <- mask(crop(raster_date, rest_area), rest_area)
    est <- mask(raster_date, rest_area, inverse = T)
    
    # Add back the names
    names(est) <- names(raster_date)
    
    # Extract the values
    new_vals <- extract(new, ext(new), xy = T) %>% as.data.frame() %>% tibble() %>% mutate(Area = 'new')
    est_vals <- extract(est, ext(est), xy = T) %>% as.data.frame() %>% tibble() %>% mutate(Area = 'est')
    
    # Bind the values
    vals <- rbind(new_vals, est_vals) %>%
      drop_na() %>%
      mutate(Date = date)
    
    # Bind to the overall tibble
    site_full_cover <- rbind(site_full_cover, vals)
    
    
    # Plot the image
    plt <- ggplot() +
      geom_spatvector(data = land, aes(fill = 'Land', col = 'Land')) +
      scale_fill_manual(values = c('Land' = 'grey92'), name = NULL,
                        guide = guide_legend(order = 2)) +
      scale_color_manual(values = c('Land' = 'grey40'), name = NULL,
                         guide = guide_legend(order = 2)) +
      new_scale_fill() +
      # geom_spatvector(data = hw_est, col = 'grey90', linewidth = .2) +
      geom_spatraster(data = rasters[[i]]) +
      facet_wrap(~lyr) +
      scale_y_continuous(expand = expansion(0)) +
      scale_x_continuous(expand = expansion(0)) +
      scale_fill_viridis_c(na.value = 'transparent',
                           # na.value = 'grey95',
                           guide = guide_colorbar(order = 1),
                           name = 'Cover (%)',
                           label = function(x) x*100,
                           limits = c(0,1)) +
      ggtitle(date) +
      theme_map(7) +
      panel_border('black', size = .5)
    
    
    # Add scale bars for CP
    if(site == 'CP') {
      
      # Extract the plot extents
      ext <- ext(est) %>%
        as.vector()
      
      # Add the height and width
      ext['height'] <- ext['ymax'] - ext['ymin']
      ext['width'] <- ext['xmax'] - ext['xmin']
      
      # Create the scale bar
      scale_bar <- tibble(
        xmax = ext['xmin'] + (ext['width'] * .95),
        xmin = xmax - 2000,
        ymin = ext['ymin'] + (ext['height'] *.05),
        ymax = ymin + (ext['height'] * .01)
      )
      
      # Add the scale bar to the plot
      plt <- plt +
        geom_rect(data = scale_bar,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = F, fill = 'black') +
        annotate(geom = 'text', label = '2 km', hjust = 1, size = 2,
                 x = scale_bar$xmax,
                 y = scale_bar$ymax + (ext['height'] * .05))
    }
    
    # Write the plot to drive
    filename <- glue('{site} {date}.png')
    # print(filename)
    path <- glue('{home}/outputs/plots/wider_estuary/per_site/')
    # print(path)
    # ggsave(plt, filename = filename, path = path, dpi = 300,
    #        width = 17, height = 8, units = 'cm')

    # Add to the list
    hab_class_full[[i]] <- plt
    
    # New plot
    plt_new <- ggplot() +
      geom_spatvector(data = land_new, aes(fill = 'Land', col = 'Land')) +
      scale_fill_manual(values = c('Land' = 'grey92'), name = NULL,
                        guide = guide_legend(order = 2)) +
      scale_color_manual(values = c('Land' = 'grey40'), name = NULL,
                         guide = guide_legend(order = 2)) +
      new_scale_fill() +
      # geom_spatvector(data = hw_new, col = 'grey90', linewidth = .2) +
      geom_spatraster(data = new) +
      facet_wrap(~lyr) +
      scale_y_continuous(expand = expansion(0)) +
      scale_x_continuous(expand = expansion(0)) +
      scale_fill_viridis_c(na.value = 'transparent',
                           # na.value = 'grey95',
                           guide = guide_colorbar(order = 1),
                           name = 'Cover (%)',
                           label = function(x) x*100,
                           limits = c(0,1)) +
      ggtitle(date) +
      theme_map(7) +
      panel_border('black', size = .5)
    
    
    # Add scale bars for CP
    if(site == 'CP') {
      
      # Extract the plot extents
      ext <- ext(new) %>%
        as.vector()
      
      # Add the height and width
      ext['height'] <- ext['ymax'] - ext['ymin']
      ext['width'] <- ext['xmax'] - ext['xmin']
      
      # Create the scale bar
      scale_bar <- tibble(
        xmax = ext['xmin'] + (ext['width'] * .95),
        xmin = xmax - 50,
        ymin = ext['ymin'] + (ext['height'] *.05),
        ymax = ymin + (ext['height'] * .01)
      )
      
      # Add the scale bar to the plot
      plt_new <- plt_new +
        geom_rect(data = scale_bar,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = F, fill = 'black') +
        annotate(geom = 'text', label = '50 m', hjust = 1, size = 2,
                 x = scale_bar$xmax,
                 y = scale_bar$ymax + (ext['height'] * .05))
    }
    
    
    # Write the plot to drive
    filename <- glue('{site} new {date}.png')
    # ggsave(plt_new, filename = filename, path = path, dpi = 300,
    #        width = 17, height = 8, units = 'cm')
    
    # Add to the list
    hab_class_new[[i]] <- plt_new
      
  }
  
  # Remove new before site creation in Adur
  if (site == 'AE') {
    site_full_cover <- site_full_cover %>%
      mutate(remove = ifelse(Area == 'new' & Date < as.Date('2019-01-01'), 1, 0)) %>%
      filter(remove == 0)
  }
  
  # Make a line plot of the changes at the site
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  line_plt <- site_full_cover %>%
    pivot_longer(Unvegetated:Mid) %>%
    group_by(Date, Area, name) %>%
    summarise(mean = mean(value)) %>%
    mutate(name = factor(name, levels = c('Unvegetated', 'Low', 'Mid'))) %>%
    ggplot(aes(Date, mean*100, linetype = Area)) +
    geom_point(size = .7) +
    geom_line(linewidth = .4) +
    facet_wrap(~ name) +
    panel_border() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = 'Mean cover (%)', col = 'Cover type', linetype = NULL, title = site_long_names[site]) +
    scale_linetype_manual(labels = c('Established', 'Restored'), values = c(1,2)) +
    scale_x_date(limits = range(all_dates))
  
  # Save the line plot to the plot list 
  line_plts_list[[a]] <- line_plt
  
  # Append the site full_cover to the final full_cover
  full_cover <- bind_rows(full_cover, site_full_cover %>%
                            mutate(site = site))
  
  
  
  # Create a plot of the first and last dates
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Extract the heights of the new/est to be able to make the plot consistent
  # aspect_ratio_est <- (ext(est)[4] - ext(est)[3])/(ext(est)[2] - ext(est)[1])
  # aspect_ratio_new <- (ext(new)[4] - ext(new)[3])/(ext(new)[2] - ext(new)[1])
  
  # Set manual relative heights
  aspect_ratio_est <- 0.55
  aspect_ratio_new <- 0.45
  
  map_grid <- plot_grid(
    hab_class_full[[1]],
    hab_class_full[[i]],
    hab_class_new[[1]],
    hab_class_new[[i]],
    ncol = 1, 
    align = 'v', axis = 'lr',
    # labels = 'AUTO', label_size = 7,
    rel_heights = c(aspect_ratio_est, 
                    aspect_ratio_est,
                    aspect_ratio_new,
                    aspect_ratio_new)
    )

  # ggsave2(map_grid, filename = glue('{site} map comparisons.pdf'),
  #         path = './outputs/plots/wider_estuary/',
  #         width = 19, height = 20, units = 'cm')
  beep(10)
}
beep(1)


# Plot --------------------------------------------------------------------

line_grid <- plot_grid(plotlist = line_plts_list, ncol = 1, labels = 'auto',
                  label_size = 7)

# ggsave(line_grid, filename = 'wider_estuary_extents_lines_1hot.png', 
#        path = './outputs/plots/',
#        dpi = 1000, width = 15, height = 17, units = 'cm')
# 
ggsave(line_grid, filename = 'wider_estuary_extents_lines_1hot.pdf',
       path = './outputs/plots/',
       width = 15, height = 17, units = 'cm')

beep(5)



# Summary stats -----------------------------------------------------------

full_cover %>%
  pivot_longer(Unvegetated:Mid, names_to = 'cover_type',
               values_to = 'cover') %>%
  mutate(cover = cover*100) %>%
  group_by(site, Area, cover_type, Date) %>%
  summarise(mean_cover = round(mean(cover), 1),
            sd = round(sd(cover), 1),
            n = n()) %>%
  ungroup() %>%
  mutate(id = paste(site, Area, cover_type), .before = Date) %>%
  select(-c(site, Area, cover_type)) %>%
  split(f = .$id)


p <- full_cover %>%
  group_by(site, Area, Date) %>%
  summarise(across(Unvegetated:Mid, mean)) %>%
  ungroup() %>%
  group_by(site, Area) %>%
  mutate(across(Unvegetated:Mid, function(x) x = x - lag(x),
                .names = '{col}_change')) %>%
  ggplot(aes(Unvegetated_change, Low_change)) +
  geom_point()

source('src/lm_summary.R')

annotate_lm(p, size = 2)


per_pixel_change <- full_cover %>%
  arrange(Date) %>%
  group_by(site, Area, x, y) %>%
  mutate(across(Unvegetated:Mid, function(x) x = x-lag(x),
                .names = '{col}_change'))

sites <- per_pixel_change %>%
  ungroup() %>%
  distinct(site, Area) %>%
  arrange(site, Area)
 

changes_lm_resuts <- tibble()

for (i in 1:length(sites$site)) {
  site_i <- sites[['site']][i]
  area_i <- sites[['Area']][i]
  
  dat <- per_pixel_change %>%
    filter(site == site_i, Area == area_i)
  
  low_unveg <- lm_to_df(x = dat$Unvegetated_change, y = dat$Low_change) %>%
    mutate(site = site_i, area = area_i, formula = 'low~unveg')
  # lm_to_df(x = dat$L)
  changes_lm_resuts <- rbind(changes_lm_resuts, low_unveg)
}

changes_lm_resuts %>%
  mutate(across(slope:p, function(x) round(x,3)))


per_pixel_change %>%
  filter(site == 'AH', Area == 'new') %>%
  ggplot(aes(Unvegetated_change, Low_change)) +
  geom_bin_2d(bins = 75)



# Correlation per pixel ---------------------------------------------------
# Per-pixel correlation for each of the different cover types

# Create a df of the different cover combinations
cover_combinations <- combn(c('Unvegetated_change', 'Low_change', 'Mid_change'), 2) %>%
  t() %>%
  as.data.frame() %>%
  rename(v1 = 1, v2 = 2)

# Loop through the combinations
for (v1 in cover_combinations$v1){
  for (v2 in cover_combinations$v2){
    cat(glue('\n{v1}-{v2}'))
    cat('\n')
    cor <- cor.test(per_pixel_change[[v1]], per_pixel_change[[v2]])
    r <- cor$estimate
    p <- cor$p.value
    cat(glue('\nr = {round(r,3)}, p = {scales::scientific(p)}'))
    cat('\n')
  }
}


# Correlation rest to est -------------------------------------------------
# Correlation in mean cover between the restored sites and their 

# Calculate mean changes per site
mean_changes <- full_cover %>%
  group_by(site, Area, Date) %>%
  summarise(across(Unvegetated:Mid, mean)) %>%
  ungroup() %>%
  group_by(site, Area) %>%
  mutate(across(Unvegetated:Mid, function(x) x = x - lag(x),
                .names = '{col}_change'))

# Loop through

for (cover in c('Unvegetated', 'Low', 'Mid')) {
  change <- glue('{cover}_change')
  print(cover)
  for (site_i in c('AE', 'CP', 'AH')) {
    dat <- mean_changes %>%
      filter(site == site_i) %>%
      select(site:Date, all_of(change)) %>%
      pivot_wider(names_from = Area, values_from = change)
    cor <- cor.test(dat$est, dat$new, use = 'complete.obs')
    r <- cor$estimate
    p <- cor$p.value
    print(glue('{site_i} r = {round(r,3)}, p = {round(p,4)}'))
  }
  
}
