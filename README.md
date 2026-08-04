# Comparison of Planet and Sentinel-2 for the fractional vegetation cover of coastal wetland communities

## Introduction

This repository provides code used to compare the relative effectiveness of PlanetScope SuperDove and Sentinel-2 imagery for mapping the fractional vegetation cover (FVC) of coastal wetland communities. Specifically, the FVC of unvegetated, low-, mid-, and high-marsh communities was mapped in three salt marsh areas in the United Kingdom - Abbotts Hall (AH), Cobnor Point (CP), and the Adur Estuary (AE). FVC was estimated using the extreme gradient boosting (XGBoost) algorithm from a combination of the bands, vegetation indices, a digital terrain model, and one-hot encoded site variables. Training and testing data was provided by drone maps of communities across the sites created in Agate et al. (*under review*), the code of which is available at this [github repo](https://github.com/joeagate/UAV-salt-marsh-vegetation-communities).

<!-- Location of the study sites -->

This repository provides the code for Agate et al. (*under review*)

## Structure



```
.
├── data
│   ├── processed
│   │   ├── training_samples
│   │   └── wider_estuary_cover_maps
│   │       └── 1hot
│   └── temp
├── imgs
├── models
│   ├── 1hot
│   └── no1hot
├── outputs
│   ├── accuracies
│   │   ├── 1hot
│   │   └── no1hot
│   ├── bayesian_parameter_tuning
│   │   ├── Planet
│   │   └── Sentinel
│   ├── feature_importance
│   │   ├── 1hot
│   │   └── no1hot
│   ├── observed-predicted
│   │   ├── 1hot
│   │   └── no1hot
│   ├── plots
│   │   ├── feature_importance
│   │   ├── observed_predicted
│   │   └── wider_estuary
│   │       └── per_site
│   └── tables
├── scripts
│   ├── 01_data_prep_R
│   ├── 02_ML_Python
│   └── 03_vis_and_interp_R
└── src
```

## Data availability

## Contact