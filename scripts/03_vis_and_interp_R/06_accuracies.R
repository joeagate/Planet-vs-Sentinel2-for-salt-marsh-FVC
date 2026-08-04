# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(cowplot)
library(here)

# Reset wd
setwd(here())

# Source the importing function
source('src/import_from_dir.R')


# Import the data ---------------------------------------------------------

# 1hot encoded
onehot <- import_from_dir(
  directory = 'outputs/accuracies/onehot/',
  pattern = '*.csv$',
  import_function = function(x)
    read_csv(x, show_col_types = F),
  bindrows = T
) %>%
  mutate(onehot = T)

# Not 1hot encoded
notonehot <- import_from_dir(
  directory = 'outputs/accuracies/notonehot/',
  pattern = '*.csv$',
  import_function = function(x)
    read_csv(x, show_col_types = F),
  bindrows = T
) %>%
  mutate(onehot = F)


# Combine the data
accuracies <- rbind(onehot, notonehot)

accuracies



# Summary table -----------------------------------------------------------

onehot %>%
  filter(dataset == 'test') %>%
  select(cover, satellite, MAE, RMSE, R2) %>%
  mutate(
    Cover = factor(cover, levels = c('unveg', 'low', 'mid', 'high'),
                   labels = c('Unvegetated', 'Low', 'Mid', 'High')),
    Satellite = factor(satellite, levels = c('Planet', 'Sentinel')),
    .before = MAE
  ) %>%
  select(-c(cover, satellite)) %>%
  arrange(Cover, Satellite) %>%
  mutate(Cover = as.character(Cover),
         Cover = ifelse(duplicated(Cover), '', Cover)) %>%
  mutate(across(MAE:R2, function(x) round(x*100, 1) |> format(nsmall = 1))) %>%
  write_csv('outputs/tables/accuracies_onehot_notraintest.csv')

notonehot %>%
  filter(dataset == 'test') %>%
  select(cover, satellite, MAE, RMSE, R2) %>%
  mutate(
    Cover = factor(cover, levels = c('unveg', 'low', 'mid', 'high'),
                   labels = c('Unvegetated', 'Low', 'Mid', 'High')),
    Satellite = factor(satellite, levels = c('Planet', 'Sentinel')),
    .before = MAE
  ) %>%
  select(-c(cover, satellite)) %>%
  arrange(Cover, Satellite) %>%
  mutate(Cover = as.character(Cover),
         Cover = ifelse(duplicated(Cover), '', Cover)) %>%
  mutate(across(MAE:R2, function(x) round(x*100, 1) |> format(nsmall = 1))) %>%
  write_csv('outputs/tables/accuracies_notonehot_notraintest.csv')



# Compare Planet to Sentinel ----------------------------------------------

cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
cat('Comparing Planet and Sentinel\n')
cat('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')

# Mean test results
cat('\nMean test results for the satellites')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
accuracies %>%
  filter(dataset == 'test') %>%
  group_by(satellite, onehot) %>%
  summarise(across(MAE:R2, list(
    mean = mean, sd = sd, n = length
  ))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1))) %>%
  print()

# Planet to Sentinel differences
p_s2_diff <- accuracies %>%
  filter(dataset == 'test') %>%
  select(satellite, cover, onehot, MAE:R2) %>%
  pivot_longer(MAE:R2) %>%
  pivot_wider(names_from = satellite, values_from = value) %>%
  mutate(Difference = Sentinel - Planet) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1)))

# Print the full differences
cat('\nFull differences between the satellites')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
print(p_s2_diff, n = Inf)

# Mean differences
cat('\nMean differences between the satellites')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')

p_s2_diff %>%
  group_by(name) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences no mid
cat('\nMean differences between the satellites (no mid-marsh)')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
p_s2_diff %>%
  filter(cover != 'mid') %>%
  group_by(name) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences no mid
cat('\nMean differences between the satellites (mid-marsh only)')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
p_s2_diff %>%
  filter(cover == 'mid') %>%
  group_by(name) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()


# Compare 1hot to no 1hot -------------------------------------------------

cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
cat('Comparing one-hot encoded and no one-hot\n')
cat('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')

# Mean test results
cat('\nMean test results')
cat('\n~~~~~~~~~~~~~~~~~\n')
accuracies %>%
  filter(dataset == 'test') %>%
  group_by(satellite, onehot) %>%
  summarise(across(MAE:R2, list(mean = mean, sd = sd))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1))) %>%
  print()

# Planet to Sentinel differences
onehot_diff <- accuracies %>%
  filter(dataset == 'test') %>%
  select(satellite, cover, onehot, MAE:R2) %>%
  pivot_longer(MAE:R2) %>%
  pivot_wider(names_from = onehot, values_from = value) %>%
  mutate(Difference = `TRUE` - `FALSE`) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1)))

# Print the full differences
cat('\nFull differences between one hot and no one hot')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
print(onehot_diff, n = Inf)

# Mean differences
cat('\nMean differences between one hot and no one hot')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
onehot_diff %>%
  group_by(name) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences per satellite
cat('\nMean differences between one hot and no one hot per satellite')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
onehot_diff %>%
  group_by(name, satellite) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()




# Train vs Test -----------------------------------------------------------

cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
cat('\nComparing Training and Testing Data')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')

# Mean test results
cat('\nMean test results for the train and test')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
accuracies %>%
  group_by(dataset) %>%
  summarise(across(MAE:R2, list(
    mean = mean, sd = sd, n = length
  ))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1))) %>%
  print()

# Planet to Sentinel differences
train_test_diff <- accuracies %>%
  select(satellite, cover, onehot, dataset, MAE:R2) %>%
  pivot_longer(MAE:R2) %>%
  pivot_wider(names_from = dataset, values_from = value) %>%
  mutate(Difference = test - train) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), function(x)
    round(x * 100, 1)))

# Print the full differences
cat('\nFull differences between the train and test sets')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
print(train_test_diff, n = Inf)

# Mean differences
cat('\nMean differences between the train and test sets')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
train_test_diff %>%
  group_by(name) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences with and without one-hot
cat('\nMean differences between the train and test sets with/w/o one-hot')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
train_test_diff %>%
  group_by(name, onehot) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences per cover type
cat('\nMean differences between the train and test sets per cover type')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
train_test_diff %>%
  group_by(name, cover) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

# Mean differences per satellite
cat('\nMean differences between the train and test sets per satellite')
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
train_test_diff %>%
  group_by(name, satellite) %>%
  summarise(
    Mean_difference = mean(Difference, na.rm = T),
    SD = sd(Difference, na.rm = T),
    n = n()
  ) %>%
  print()

