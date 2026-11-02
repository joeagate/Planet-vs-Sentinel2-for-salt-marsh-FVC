# Set up ------------------------------------------------------------------

# Import packages
library(tidyverse)
library(cowplot)
library(here)
library(RColorBrewer)
library(glue)
library(tidytext)
library(colorspace)

# Reset wd
setwd(here())

# Set plotting theme
theme_set(theme_cowplot(7))
theme_update(strip.background = element_blank())

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

# Create a named vector of the long names for the cover types
long_names <- c('unveg' = 'Unvegetated',
                'low' = 'Low',
                'mid' = 'Mid',
                'high' = 'High')


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
        ggtitle(long_names[j]) +
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
  grid <- plot_grid(plotlist = plot_list, labels = 'AUTO', label_size = 7,
                    ncol = 2, nrow = 2)
  
  # Save the grid to drive
  ggsave2(grid, filename = paste0(i, '.pdf'), path = 'outputs/plots/feature_importance/',
          dpi = 500, width = 15, height = 17, units = 'cm')
  
  total_grid <- plot_grid(plotlist = total_list, labels = 'AUTO', label_size = 7,
                    ncol = 2, nrow = 2)
  
  # Save the grid to drive
  ggsave2(total_grid, filename = paste0(i, '_total.pdf'), path = 'outputs/plots/feature_importance/total',
          dpi = 500, width = 15, height = 12, units = 'cm')
}


# Ranking -----------------------------------------------------------------

## Mean importance ----

# Mean importance for feature groups
featImp %>%
  filter(dataset == 'test') %>%
  filter(onehot == 'onehot') %>%
  group_by(feature_group) %>%
  summarise(mean_perm = mean(mean_perm_importance),
            sd = sd(mean_perm_importance),
            n = n()) %>%
  arrange(desc(mean_perm))

# Mean importance for feature groups per satellite
featImp %>%
  filter(dataset == 'test') %>%
  filter(onehot == 'onehot') %>%
  group_by(feature_group, satellite) %>%
  summarise(mean_perm = mean(mean_perm_importance),
            sd = sd(mean_perm_importance),
            n = n()) %>%
  arrange(desc(mean_perm)) %>%
  split(f = .$satellite)

# Mean importance for feature groups per cover per satellite
featImp %>%
  filter(dataset == 'test') %>%
  filter(onehot == 'onehot') %>%
  group_by(feature_group, cover, satellite) %>%
  summarise(mean_perm = mean(mean_perm_importance),
            sd = sd(mean_perm_importance),
            n = n()) %>%
  arrange(desc(mean_perm)) %>%
  mutate(sat_cover = interaction(satellite, cover)) %>%
  select(-satellite, cover) %>%
  split(f = .$sat_cover)

# Mean importance for feature groups per cover
featImp %>%
  filter(dataset == 'test') %>%
  filter(onehot == 'onehot') %>%
  group_by(feature_group, cover) %>%
  summarise(mean_perm = mean(mean_perm_importance),
            sd = sd(mean_perm_importance),
            n = n()) %>%
  arrange(desc(mean_perm)) %>%
  split(f = .$cover)

# Plot the relative importance of the features
featImp %>%
  filter(dataset == 'test') %>%
  filter(onehot == 'onehot') %>%
  group_by(feature_group, cover, satellite) %>%
  summarise(mean_perm = mean(mean_perm_importance),
            sd = sd(mean_perm_importance),
            n = n()) %>%
  mutate(mod = paste(cover, satellite)) %>%
  ggplot(aes(reorder_within(feature_group, mean_perm, mod), mean_perm)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_perm-sd, ymax = mean_perm+sd),
                width = .2) +
  coord_flip() +
  facet_wrap(~mod, scales = 'free_y') +
  panel_border() +
  scale_x_reordered() +
  theme(strip.background = element_blank())

featImp %>%
  filter(dataset == 'test') %>%
  group_by(model_id, feature_group) %>%
  summarise(sum_perm = sum(mean_perm_importance)) %>%
  group_by(feature_group) %>%
  summarise(mean_sum_perm = mean(sum_perm),
            sd = sd(sum_perm),
            n = n())


featImp %>%
  filter(feature != 'DTM') %>%
  # filter(mean_perm_importance < 0.004) %>%
  ggplot(aes(reorder(feature, mean_perm_importance), mean_perm_importance, col = feature_group)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL) 



# Train-test --------------------------------------------------------------

featImp %>%
  filter(onehot == 'onehot') %>%
  select(feature, mean_perm_importance, satellite, cover, dataset, onehot) %>%
  pivot_wider(names_from = dataset,
              values_from = mean_perm_importance) %>%
  mutate(test_train_diff = test - train,
         pcnt_test_train_diff = test_train_diff / train) %>%
  mutate(mod = paste(satellite, cover)) %>%
  ggplot(aes(reorder_within(feature, test_train_diff, mod), test_train_diff)) +
  geom_col(aes(fill = test_train_diff), show.legend = F) +
  coord_flip() +
  facet_wrap(~ satellite + cover, scales = 'free') +
  panel_border() +
  scale_x_reordered() +
  scale_fill_continuous_divergingx(mid = 0, palette = 'Geyser',
                                   rev = T)


featImp %>%
  filter(dataset == 'train') %>%
  group_by(satellite, feature_group) %>%
  summarise(mean_perm = mean(mean_perm_importance))
