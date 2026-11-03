# Set up ------------------------------------------------------------------

# Import packages
library(tidyverse)
library(cowplot)
library(here)
library(Metrics)

# Reset wd
setwd(here())

# Set plotting theme
theme_set(theme_cowplot(7))

# Source the importing from directories function
source('src/import_from_dir.R')

# Source the lm summary function
source('src/lm_summary.R')

# Create a function to return the R2 for two datasets
extract_R2 <- function(y, x){
  mod <- lm(y ~ x)
  sum <- summary(mod)
  R2 <- sum$r.squared
  return(R2)
}



# Import the data ---------------------------------------------------------

obspreds <- import_from_dir(
  directory = 'outputs/observed-predicted/',
  pattern = '*.csv$',
  import_function = function(x) read_csv(x, show_col_types = F),
  bind_rows = T,
  full_names = T,
  recursive = T
)

# Add a 'cover' column to match the plotting style
obspreds <- obspreds %>%
  mutate(Cover = recode_values(cover,
                               'high' ~ 'High',
                               'low' ~ 'Low',
                               'mid' ~ 'Mid',
                               'unveg' ~ 'Unvegetated'))

# Add an identifier for each of the models
obspreds <- obspreds %>%
  mutate(ModelID = paste(satellite, bands, sample, onehot, sep = '_')) 


# Summary stats -----------------------------------------------------------

obspreds %>%
  group_by(satellite, cover, onehot) %>%
  summarise(min_pred = min(predicted),
            max_pred = max(predicted))

obspreds %>%
  filter(satellite == 'Planet', cover == 'low') %>%
  ggplot(aes(observed, predicted)) +
  geom_point()


# Plot the data -----------------------------------------------------------


# Extract the unique model ids
ModelIDs <- unique(obspreds$ModelID)

# Init tibble to store lm results
lm_results <- tibble()

# Loop through the model ids to create the plots
for (i in ModelIDs){
  cat('\n', i, '\n', sep = '')
  
  # Filter to the model type
  obspreds_mod <- obspreds %>%
    filter(ModelID == i)
  
  # Initialise before plotting
  plot_list <- list()
  resids_pred_plots <- list()
  resids_obs_plots <- list()
  a <- 0
  
  # Loop through cover types
  for (j in c('Unvegetated', 'Low', 'Mid', 'High')){
    
    # Filter to the cover type
    obspreds_mod_cover <- obspreds_mod %>%
      filter(Cover == j)
    
    # If there are observations, then plot
    if (nrow(obspreds_mod_cover) > 0){
      a <- a+1
      
      # Create the plot
      plot <- obspreds_mod_cover %>%
        mutate(observed = observed*100, predicted=predicted*100) %>%
        ggplot(aes(x = observed, y = predicted)) +
        geom_abline(slope = 1, col = 'grey', linetype = 2) +
        geom_point(alpha = .4, size = .4) +
        geom_smooth(method = lm, se = F, col = 'red') +
        xlim(0,100) + ylim(0,100) +
        coord_equal() +
        ggtitle(j) +
        # panel_border() +
        labs(x = 'Observed cover (%)', y = 'Predicted cover (%)')
      
      # Annotate the plot
      plot <- annotate_lm(plot, slope = T, intercept = F,
                          R2 = T, adjR2 = F, size = 2, pv = F, bias = T,
                          xpos = 0, ypos = 100)
      
      plot_list[[a]] <- plot
      
      # Create a plot of the residuals
      resids_pred <- obspreds_mod_cover %>%
        mutate(residual = observed-predicted) %>%
        ggplot(aes(predicted*100, residual*100)) +
        geom_hline(yintercept = 0, linetype = 2, col = 'grey') +
        geom_point(alpha = .5, size = .6) +
        ggtitle(j) +
        labs(x = 'Predicted cover (%)', y = 'Residual (%)')
      resids_pred_plots[[a]] <- resids_pred
      
      # Create a plot of the residuals
      resids_obs <- obspreds_mod_cover %>%
        mutate(residual = observed-predicted) %>%
        ggplot(aes(observed*100, residual*100)) +
        geom_hline(yintercept = 0, linetype = 2, col = 'grey') +
        geom_point(alpha = .5, size = .6) +
        ggtitle(j) +
        labs(x = 'Observed cover (%)', y = 'Residual (%)')
      resids_obs_plots[[a]] <- resids_obs
      
      
      # Create a dataframe of the lm results
      lm_df <- lm_to_df(x = obspreds_mod_cover$observed,
                        y = obspreds_mod_cover$predicted) %>%
        mutate(model = i, cover = j)
      
      # Append to lm results
      lm_results <- rbind(lm_results, lm_df)
    }
  }
  
  # Combine the plots into a grid
  grid <- plot_grid(plotlist = plot_list, labels = 'auto', label_size = 8,
                    ncol = 2, nrow = 2)
  
  # Save the grid to drive
  ggsave2(grid, filename = paste0(i, '.pdf'),
          path = 'outputs/plots/observed_predicted/',
          dpi = 1000, width = 15, height = 15, units = 'cm')
  
  # Combine and save the resids plots
  grid_resid_preds <- plot_grid(plotlist = resids_pred_plots, labels = 'AUTO', 
                                label_size = 10,ncol = 2, nrow = 2)
  ggsave2(grid_resid_preds, filename = paste0(i, '_residuals_pred.png'), 
          path = 'outputs/plots/observed_predicted/residuals/',
          dpi = 1000, width = 15, height = 15, units = 'cm')
  grid_resid_obs <- plot_grid(plotlist = resids_obs_plots, labels = 'AUTO', 
                                label_size = 10,ncol = 2, nrow = 2)
  ggsave2(grid_resid_obs, filename = paste0(i, '_residuals_obs.png'), 
          path = 'outputs/plots/observed_predicted/residuals/',
          dpi = 1000, width = 15, height = 15, units = 'cm')
}


# Expand the lm results to get all the info
lm_results <- lm_results %>%
  separate_wider_delim(model, delim = '_', names = c(
    'satellite', 'bands', 'sample', 'onehot'
  ))
  
  

# Summary stats -----------------------------------------------------------

lm_results %>%
  group_by(satellite, onehot) %>%
  summarise(mean = mean(slope),
            sd = sd(slope),
            n = n())
