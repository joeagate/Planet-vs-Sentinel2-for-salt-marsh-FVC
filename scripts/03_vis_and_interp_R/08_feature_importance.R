# Set up ------------------------------------------------------------------

# Import packages
library(tidyverse)
library(cowplot)
library(here)
library(RColorBrewer)
library(glue)

# Reset wd
setwd(here())

# Set plotting theme
theme_set(theme_cowplot(7))

# Source the importing from directories function
source('src/import_from_dir.R')


# Import data -------------------------------------------------------------

# Import the data from drive
featImp <- import_from_dir(
  directory = 'outputs/feature_importance/',
  pattern = '*.csv$',
  import_function = function(x) read_csv(x, show_col_types = F),
  recursive = T,
  also_filenames = T
)

# Add the filename to the data and bind into one large df
for (i in 1:length(featImp$filenames)) {
  featImp$files[[i]]$file <- featImp$filenames[i]
}
featImp <- do.call(rbind, featImp$files)


# Wrangling ---------------------------------------------------------------

# Refactor the feature names
featImp <- featImp %>%
  filter(bands == 'CommonBands') %>%
  mutate(feature = case_match(feature,
    'blue' ~ 'Blue',
    'green' ~ 'Green',
    'red' ~ 'Red',
    'rededge' ~ 'Red edge',
    'nir' ~ 'NIR',
    'B2' ~ 'Blue',
    'B3' ~ 'Green',
    'B4' ~ 'Red',
    'SRB5' ~ 'Red edge',
    'B8A' ~ 'NIR',
  .default = feature),
  onehot = case_when(
    grepl('notonehot', file) ~ 'notonehot',
    .default = 'onehot'
  ))

# Add an identifier for each of the models
featImp <- featImp %>%
  mutate(model_id = paste(satellite, onehot, sep = '_')) 

# Add a feature grouping
featImp <- featImp %>%
  mutate(feature_group = case_when(
    feature == 'DTM' ~ 'DTM',
    feature %in% c('Blue', 'Green', 'Red', 'Red edge', 'NIR') ~ 'Bands',
    grepl('Site', feature) ~ 'Site',
    grepl('Est', feature) ~ 'Site',
    .default = 'VIs'
  ))

# Change '_' to ': '
featImp <- featImp %>%
  mutate(feature = gsub('_', ': ', feature))

# Extract the unique model ids
model_ids <- unique(featImp$model_id)


# Plot the models --------------------------------------------------------

# Loop through the model ids to create the plots
for (i in model_ids){
  cat('\n', i, '\n', sep = '')
  
  # Filter to the model ID
  featImp_mod <- featImp %>%
    filter(model_id == i)
  
  # Initialise before plotting
  plot_list <- list()
  total_list <- list()
  a <- 0
  
  # Loop through cover types
  for (j in c('unveg', 'low', 'mid', 'high')){
    
    # Filter to the cover type
    featImp_mod_cover <- featImp_mod %>%
      filter(cover == j) %>%
      filter(dataset == 'test')
    
    # If there are observations, then plot
    if (nrow(featImp_mod_cover) > 0){
      a <- a+1
      plot <- featImp_mod_cover %>%
        ggplot(aes(reorder(feature, mean_perm_importance), mean_perm_importance,
                   fill = feature_group)) +
        geom_col() +
        geom_errorbar(aes(ymin = mean_perm_importance - std_dev_perm_importance,
                          ymax = mean_perm_importance + std_dev_perm_importance),
                      col = 'grey20', width = .3) +
        coord_flip() +
        labs(x = 'Feature', y = 'Permutation importance') +
        ggtitle(j) +
        theme(legend.position = 'inside',
              legend.position.inside = c(.95,0.05),
              legend.justification.inside = c(1,0)) +
        scale_fill_brewer(palette = 'Set2', name = 'Feature\ngroup')
      plot
      
## Coloured axis text ----
      
      f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
      
      brew_cols <- f('Dark2')
      
      colour_key <- featImp_mod_cover %>%
        distinct(feature_group) %>%
        arrange(feature_group) %>%
        mutate(colour = brew_cols[1:length(.$feature_group)])
      
      # colour_key <- tibble(feature_group = c('Bands', 'DTM', 'VIs'),
      #                      Colour = brew_cols[1:3])
      
      axis_cols <- featImp_mod_cover %>%
        left_join(colour_key, by = 'feature_group') %>%
        # filter(Dataset == 'Test') %>%
        arrange(mean_perm_importance) %>%
        ungroup() %>%
        pull(colour)
      
      plot <- plot +
        theme(axis.text.y = element_text(colour = axis_cols))
      
      
      plot_list[[a]] <- plot
      
## Total ----
      
      # Create a total plot for each feature group
      total <- featImp_mod_cover %>%
        group_by(feature_group) %>%
        summarise(total_importance = sum(mean_perm_importance)) %>%
        ggplot(aes(reorder(feature_group, total_importance), 
                   total_importance, fill = feature_group)) +
        geom_col() +
        coord_flip() +
        labs(x = 'Feature group', y = 'Total permutation importance') +
        scale_fill_brewer(palette = 'Set2', name = NULL) +
        theme(legend.position = 'none') +
        ggtitle(j)
      
      total_list[[a]] <- total
    }
  }
  
  # Combine the plots into a grid
  grid <- plot_grid(plotlist = plot_list, labels = 'AUTO', label_size = 8,
                    ncol = 2, nrow = 2)
  
  # Save the grid to drive
  ggsave2(grid, filename = paste0(i, '.png'), path = 'outputs/plots/feature_importance/',
          dpi = 500, width = 15, height = 17, units = 'cm')
  
  total_grid <- plot_grid(plotlist = total_list, labels = 'AUTO', label_size = 8,
                    ncol = 2, nrow = 2)
  
  # Save the grid to drive
  ggsave2(total_grid, filename = paste0(i, '_total.png'), path = 'outputs/plots/feature_importance/total',
          dpi = 500, width = 15, height = 12, units = 'cm')
}

# Train-test --------------------------------------------------------------

for (sat in c('Planet', 'Sentinel')) {
  print(sat)
  
  # Filter to the satellite
  featImp_sat <- featImp %>%
    filter(Satellite == sat)
  
  # Init a list to store the plots
  plt_list <- list()
  a <- 0
  
  # Loop through the cover types
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  for (cover in c('Unvegetated', 'Low', 'Mid', 'High')) {
    print(cover)
    
    # Filter to the model
    featImp_sat_cover <- featImp_sat %>%
      filter(Cover_type == cover)
    
    # Check first there is data for the model
    if (length(featImp_sat_cover$feature)>0){
      a <- a+1
      
      
      
      # Plot the perm importance with train and test
      plt <- featImp_sat_cover %>%
        ggplot(aes(reorder(feature, mean_perm_importance), 
                   mean_perm_importance, fill = Dataset)) +
        geom_col(position = position_dodge(width = .7), width = .7) +
        geom_errorbar(aes(ymin = mean_perm_importance-std_dev_perm_importance,
                          ymax = mean_perm_importance+std_dev_perm_importance),
                      position = position_dodge(width = .7), width = .4) +
        coord_flip() +
        labs(y = 'Permutation importance', x = 'Features') +
        scale_y_continuous(expand = expansion(c(.01, .05))) +
        scale_fill_brewer(palette = 'Set2', breaks = c('Train', 'Test')) +
        theme(legend.position = 'inside',
              legend.position.inside = c(.95,.05),
              legend.justification.inside = c(1,0)) +
        ggtitle(cover)
      
      # Save the plot to the list
      plt_list[[a]] <- plt
      
    } else {warning(glue('Cover type {cover} for {sat} has length 0, so not plotted'))}
    
  }
  
  # Plot the grid
  grd <- plot_grid(plotlist = plt_list, labels = 'AUTO', label_size = 8)
  
  # Save the grid to drive
  ggsave2(grd, filename = glue('{sat} train test featImp grid.png'),
          path = 'Upscaling/Plots/Feature importance/Publication/Train-Test/',
          dpi = 1000, width = 15, height = 17, units = 'cm')
}


# Ranking -----------------------------------------------------------------

## Mean importance ----

featImp %>%
  filter(feature != 'DTM') %>%
  # filter(mean_perm_importance < 0.004) %>%
  ggplot(aes(reorder(feature, mean_perm_importance), mean_perm_importance, col = feature_group)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL) 

## Ranked ----

ranked <- featImp %>%
  filter(Dataset == 'Test') %>%
  group_by(Satellite, Sample, Cover_type, Bands) %>%
  arrange(desc(mean_perm_importance), .by_group = T) %>%
  mutate(Rank = 1:n()) %>%
  ungroup()

featImp %>%
  filter(Satellite == 'Sentinel', Dataset == 'Test', Cover_type == 'Mid', NIR == 'B8A') %>%
  print(n = Inf)

ranked %>%
  group_by(feature, Rank) %>%
  count() %>%
  ungroup() %>%
  filter(Rank < 5) %>%
  arrange(Rank, desc(n)) %>%
  split(f = .$Rank)

ranked %>%
  mutate(Rank_bin = cut_interval(Rank, length = 5)) %>%
  group_by(feature, Rank_bin) %>%
  count() %>%
  arrange(Rank_bin, desc(n)) %>%
  split(f = .$Rank_bin)

ranked %>%
  filter(feature_group == 'VIs') %>%
  ggplot(aes(reorder(feature, desc(Rank)), Rank, col = feature_group)) +
  # geom_boxplot() +
  geom_point() +
  coord_flip() +
  scale_color_brewer(palette = 'Dark2')


## Grouping ----

featImp %>%
  filter(feature != 'DTM') %>%
  ggplot(aes(reorder(feature_group, mean_perm_importance), mean_perm_importance)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL)


featImp %>%
  filter(feature != 'DTM') %>%
  # filter(feature_group == 'Site') %>%
  mutate(Cover_type = factor(Cover_type, levels = c('Unvegetated', 'Low', 'Mid', 'High'))) %>%
  # filter(mean_perm_importance < 0.004) %>%
  ggplot(aes(reorder(feature, mean_perm_importance), mean_perm_importance)) +
  geom_hline(yintercept = 0, col = 'grey', linetype = 2) +
  geom_boxplot() +
  geom_point(aes(col = paste(Satellite, Sample))) +
  coord_flip() +
  labs(x = NULL, col = NULL) +
  facet_grid(rows = vars(feature_group), cols = vars(Cover_type),
             scales = 'free', space = 'free') +
  panel_border() +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

ranked %>%
  filter(feature != 'DTM') %>%
  ggplot(aes(reorder(feature, desc(Rank)), Rank)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL) +
  facet_grid(rows = vars(feature_group), cols = vars(Cover_type),
             scales = 'free', space = 'free') +
  panel_border()


featImp %>% 
  group_by(feature) %>%
  summarise(mean_perm_importance = mean(mean_perm_importance)) %>%
  arrange(mean_perm_importance)




featImp %>%
  filter(Cover_type == 'Low') %>%
  ggplot(aes(reorder(feature, mean_perm_importance), 
             mean_perm_importance, fill = Dataset)) +
  geom_col(position = position_dodge(width = .7), width = .7) +
  geom_errorbar(aes(ymin = mean_perm_importance-std_dev_perm_importance,
                    ymax = mean_perm_importance+std_dev_perm_importance),
                position = position_dodge(width = .7), width = .4) +
  coord_flip() +
  labs(y = 'Permutation importance', x = 'Features') +
  scale_y_continuous(expand = expansion(c(.01, .05))) +
  scale_fill_brewer(palette = 'Set2') +
  theme(legend.position = 'inside',
        legend.position.inside = c(.95,.05),
        legend.justification.inside = c(1,0))
