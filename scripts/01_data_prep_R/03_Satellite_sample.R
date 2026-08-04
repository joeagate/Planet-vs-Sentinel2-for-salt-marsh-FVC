# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(here)

# Reset wd
setwd(here())

# SentinelAllBands ------------------------------------------------------------------

# Import the SentinelAllBands data
SentinelAllBands <- read_csv('data/interim/satellite_values/Combined_Sentinel_values_AllBands_B8A-NIR.csv')

# Add the bins
SentinelAllBands_bin <- SentinelAllBands %>%
  mutate(across(Unvegetated:High, function(x) cut_interval(x, n = 10), .names = '{.col}_bin'))


## 100/.1 ----

# SentinelAllBands Unvegetated sample
set.seed(123)
SentinelAllBands_sample_unveg_100 <- SentinelAllBands_bin %>%
  group_by(Unvegetated_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelAllBands_sample_unveg_100, 'data/interim/samples/Sentinel/Sentinel_sample_unveg_100_AllBands_B8A.csv')

# SentinelAllBands Low sample
set.seed(123)
SentinelAllBands_sample_low_100 <- SentinelAllBands_bin %>%
  group_by(Low_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelAllBands_sample_low_100, 'data/interim/samples/Sentinel/Sentinel_sample_low_100_AllBands_B8A.csv')

# SentinelAllBands Low sample
set.seed(123)
SentinelAllBands_sample_mid_100 <- SentinelAllBands_bin %>%
  group_by(Mid_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelAllBands_sample_mid_100, 'data/interim/samples/Sentinel/Sentinel_sample_mid_100_AllBands_B8A.csv')

# Sentinel High sample
# set.seed(123)
# Sentinel_sample_high_100 <- Sentinel_bin %>%
#   group_by(High_bin) %>%
#   sample_n(size = 100) %>%
#   ungroup()
# write_csv(Sentinel_sample_high_100, 'data/interim/samples/Sentinel/Sentinel_sample_high_100_AllBands_B8A.csv')


# SentinelCommonBands ------------------------------------------------------------------

# Import the SentinelCommonBands data
SentinelCommonBands <- read_csv('data/interim/satellite_values/Combined_Sentinel_values_CommonBands_B8A-NIR.csv')

# Add the bins
SentinelCommonBands_bin <- SentinelCommonBands %>%
  mutate(across(Unvegetated:High, function(x) cut_interval(x, n = 10), .names = '{.col}_bin'))


## 100/.1 ----

# SentinelCommonBands Unvegetated sample
set.seed(123)
SentinelCommonBands_sample_unveg_100 <- SentinelCommonBands_bin %>%
  group_by(Unvegetated_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelCommonBands_sample_unveg_100, 'data/interim/samples/Sentinel/Sentinel_sample_unveg_100_CommonBands_B8A.csv')

# SentinelCommonBands Low sample
set.seed(123)
SentinelCommonBands_sample_low_100 <- SentinelCommonBands_bin %>%
  group_by(Low_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelCommonBands_sample_low_100, 'data/interim/samples/Sentinel/Sentinel_sample_low_100_CommonBands_B8A.csv')

# SentinelCommonBands Low sample
set.seed(123)
SentinelCommonBands_sample_mid_100 <- SentinelCommonBands_bin %>%
  group_by(Mid_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(SentinelCommonBands_sample_mid_100, 'data/interim/samples/Sentinel/Sentinel_sample_mid_100_CommonBands_B8A.csv')

# Sentinel High sample
# set.seed(123)
# Sentinel_sample_high_100 <- Sentinel_bin %>%
#   group_by(High_bin) %>%
#   sample_n(size = 100) %>%
#   ungroup()
# write_csv(Sentinel_sample_high_100, 'data/interim/samples/Sentinel/Sentinel_sample_high_100_CommonBands_B8A.csv')


# PlanetAllBands ------------------------------------------------------------------

# Import the PlanetAllBands data
PlanetAllBands <- read_csv('data/interim/satellite_values/Combined_Planet_values_AllBands.csv')

# Add the bins
PlanetAllBands_bin <- PlanetAllBands %>%
  mutate(across(Unvegetated:High, function(x) cut_interval(x, n = 10), .names = '{.col}_bin'))


## 100/.1 ----

# PlanetAllBands Unvegetated sample
set.seed(123)
PlanetAllBands_sample_unveg_100 <- PlanetAllBands_bin %>%
  group_by(Unvegetated_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetAllBands_sample_unveg_100, 'data/interim/samples/Planet/Planet_sample_unveg_100_AllBands.csv')

# PlanetAllBands Low sample
set.seed(123)
PlanetAllBands_sample_low_100 <- PlanetAllBands_bin %>%
  group_by(Low_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetAllBands_sample_low_100, 'data/interim/samples/Planet/Planet_sample_low_100_AllBands.csv')

# PlanetAllBands Low sample
set.seed(123)
PlanetAllBands_sample_mid_100 <- PlanetAllBands_bin %>%
  group_by(Mid_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetAllBands_sample_mid_100, 'data/interim/samples/Planet/Planet_sample_mid_100_AllBands.csv')

# PlanetAllBands High sample
set.seed(123)
PlanetAllBands_sample_high_100 <- PlanetAllBands_bin %>%
  group_by(High_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetAllBands_sample_high_100, 'data/interim/samples/Planet/Planet_sample_high_100_AllBands.csv')


# ## 1000/.1 ----
# 
# # PlanetAllBands Unvegetated sample
# set.seed(123)
# PlanetAllBands_sample_unveg_1000 <- PlanetAllBands_bin %>%
#   group_by(Unvegetated_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetAllBands_sample_unveg_1000, 'data/interim/samples/Planet/Planet_sample_unveg_1000_AllBands.csv')
# 
# # PlanetAllBands Low sample
# set.seed(123)
# PlanetAllBands_sample_low_1000 <- PlanetAllBands_bin %>%
#   group_by(Low_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetAllBands_sample_low_1000, 'data/interim/samples/Planet/Planet_sample_low_1000_AllBands.csv')
# 
# # PlanetAllBands Low sample
# set.seed(123)
# PlanetAllBands_sample_mid_1000 <- PlanetAllBands_bin %>%
#   group_by(Mid_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetAllBands_sample_mid_1000, 'data/interim/samples/Planet/Planet_sample_mid_1000_AllBands.csv')

# Planet High sample - not enough points


# PlanetCommonBands ------------------------------------------------------------------

# Import the PlanetCommonBands data
PlanetCommonBands <- read_csv('data/interim/satellite_values/Combined_Planet_values_CommonBands.csv')

# Add the bins
PlanetCommonBands_bin <- PlanetCommonBands %>%
  mutate(across(Unvegetated:High, function(x) cut_interval(x, n = 10), .names = '{.col}_bin'))


## 100/.1 ----

# PlanetCommonBands Unvegetated sample
set.seed(123)
PlanetCommonBands_sample_unveg_100 <- PlanetCommonBands_bin %>%
  group_by(Unvegetated_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetCommonBands_sample_unveg_100, 'data/interim/samples/Planet/Planet_sample_unveg_100_CommonBands.csv')

# PlanetCommonBands Low sample
set.seed(123)
PlanetCommonBands_sample_low_100 <- PlanetCommonBands_bin %>%
  group_by(Low_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetCommonBands_sample_low_100, 'data/interim/samples/Planet/Planet_sample_low_100_CommonBands.csv')

# PlanetCommonBands Mid sample
set.seed(123)
PlanetCommonBands_sample_mid_100 <- PlanetCommonBands_bin %>%
  group_by(Mid_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetCommonBands_sample_mid_100, 'data/interim/samples/Planet/Planet_sample_mid_100_CommonBands.csv')

# PlanetCommonBands High sample
set.seed(123)
PlanetCommonBands_sample_high_100 <- PlanetCommonBands_bin %>%
  group_by(High_bin) %>%
  sample_n(size = 100) %>%
  ungroup()
write_csv(PlanetCommonBands_sample_high_100, 'data/interim/samples/Planet/Planet_sample_high_100_CommonBands.csv')


# ## 1000/.1 ----
# 
# # PlanetCommonBands Unvegetated sample
# set.seed(123)
# PlanetCommonBands_sample_unveg_1000 <- PlanetCommonBands_bin %>%
#   group_by(Unvegetated_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetCommonBands_sample_unveg_1000, 'data/interim/samples/Planet/Planet_sample_unveg_1000_CommonBands.csv')
# 
# # PlanetCommonBands Low sample
# set.seed(123)
# PlanetCommonBands_sample_low_1000 <- PlanetCommonBands_bin %>%
#   group_by(Low_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetCommonBands_sample_low_1000, 'data/interim/samples/Planet/Planet_sample_low_1000_CommonBands.csv')
# 
# # PlanetCommonBands Mid sample
# set.seed(123)
# PlanetCommonBands_sample_mid_1000 <- PlanetCommonBands_bin %>%
#   group_by(Mid_bin) %>%
#   sample_n(size = 1000) %>%
#   ungroup()
# write_csv(PlanetCommonBands_sample_mid_1000, 'data/interim/samples/Planet/Planet_sample_mid_1000_CommonBands.csv')

# Planet High sample - not enough points


