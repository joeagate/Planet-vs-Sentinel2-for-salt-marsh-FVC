# Comparison of Planet and Sentinel-2 for the fractional vegetation cover of coastal wetland communities

<p align="center">
    <img src="imgs/Sat upscaling FVC diagram.jpeg" width="400"/>
</p>
<p align="center">
    <em>Schematic of the training workflow</em>
</p>


This repository provides code used to compare the relative effectiveness of PlanetScope SuperDove and Sentinel-2 imagery for mapping the fractional vegetation cover (FVC) of coastal wetland communities. Specifically, the FVC of unvegetated, low-, mid-, and high-marsh communities was mapped in three salt marsh areas in the United Kingdom - Abbotts Hall (AH), Cobnor Point (CP), and the Adur Estuary (AE). FVC was estimated using the extreme gradient boosting (XGBoost) algorithm from a combination of the bands, vegetation indices, a digital terrain model, and one-hot encoded site variables. Training and testing data was provided by drone maps of communities across the sites created in [Agate et al. (2026)](https://www.mdpi.com/2072-4292/18/6/866), the code of which is available at this [github repo](https://github.com/joeagate/UAV-salt-marsh-vegetation-communities).

This repository provides the code for [Agate et al. (*under review*)]()

## Using the code in this repository

The code used to compare the Planet and Sentinel-2 imagery is available in the `scripts` folder.

Inside the `scripts` folder, `01_data_prep_R` contains the R code used for pre-processing. This code will not run as is, as the raw external data needed is not provided, but is instead intended to provide reference for how pre-processing was performed. 

`02_ML_Python` contains the Python code used to train the XGBoost FVC models as well as apply the trained models to the wider estuary areas.

`03_vis_and_interp_R` contains the R code used to assess the outputs from model training, including accuracy and feature importance, as well as to produce the figures in the publication.

`run_scripts.ipynb` provides users with a Jupyter Notebook that can run all of the analysis scripts sequentially.


## Data availability

The data used to train the FVC models are provided in this repository to allow users to pllay with model training themselves. However, the drone community maps and satellite imagery neeeded to produce this training data are not. For the satellite imagery, please refer to the related [publication](), which contains the information needed to identify the images used that can then be sourced from either the [Copernicus Data Space Explorer](https://browser.dataspace.copernicus.eu/?zoom=5&lat=51.43225&lng=12.23991&themeId=DEFAULT-THEME&visualizationUrl=U2FsdGVkX1%2FB%2BdZ3wCFHpdbJDVTkO1AZc7rvT%2BokJgxRSVF5KDoZJRiWhYy3UG8RCTzCEHgvEZU0SiSPpclOm6WlHuf5HrWeOoG65QpSzH9qVrblgUf7AJJLQqDQnNtT&datasetId=S2_L2A_CDAS&demSource3D=%22MAPZEN%22&cloudCoverage=30&dateMode=SINGLE&panel=layers) for Sentinel-2 images, or the [Planet Explorer](https://www.planet.com/explorer/) for Planet images. The classified UAV maps of communities are available upon request from Joseph Agate (see below).

## Contact

Please contact the author Joseph Agate (j.e.agate@utwente.nl) with any questions about the code in this repository.