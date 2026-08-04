# %% 
# Import libraries
import numpy as np
import joblib
import rasterio
import glob
from pathlib import Path

# %%
# Import

# Find all the files on drive
files = list(Path('./data/interim/wider_estuary/onehot').glob('*tiff'))

# Import the models
unveg_model = joblib.load('./models/onehot/Sentinel_unveg_100_CommonBands_onehot_trained_model.pkl')
low_model = joblib.load('./models/onehot/Sentinel_low_100_CommonBands_onehot_trained_model.pkl')
mid_model = joblib.load('./models/onehot/Sentinel_mid_100_CommonBands_onehot_trained_model.pkl')

# %%
# Loop through the files
for i in files:

    # Import the image
    # ================
    with rasterio.open(i) as src:
        stacked_raster = src.read()
        metadata = src.meta

    # Reshape the raster
    bands, rows, cols = stacked_raster.shape
    stacked_raster_reshaped = stacked_raster.reshape(bands, rows * cols).T

    
    # Unvegetated model
    # =================
    # Run the model on the imported image
    unveg = unveg_model.predict(stacked_raster_reshaped)
    
    # Normalise the reg model
    unveg[unveg > 1] = 1
    unveg[unveg < 0] = 0
    
    # Clip the raster to the aoi
    unveg[np.isnan(stacked_raster_reshaped[:, 1])] = np.nan
    
    # Reshape the final raster
    unveg_reshaped = unveg.reshape(rows, cols)

    
    # Low model
    # =========
    # Run the model on the imported image
    low = low_model.predict(stacked_raster_reshaped)
    
    # Normalise the reg model
    low[low > 1] = 1
    low[low < 0] = 0
    
    # Clip the raster to the aoi
    low[np.isnan(stacked_raster_reshaped[:, 1])] = np.nan
    
    # Reshape the final raster
    low_reshaped = low.reshape(rows, cols)


    # Mid model
    # =========
    # Run the model on the imported image
    mid = mid_model.predict(stacked_raster_reshaped)
    
    # Normalise the reg model
    mid[mid > 1] = 1
    mid[mid < 0] = 0
    
    # Clip the raster to the aoi
    mid[np.isnan(stacked_raster_reshaped[:, 1])] = np.nan
    
    # Reshape the final raster
    mid_reshaped = mid.reshape(rows, cols)


    # Stack the raster and export
    # ===========================
    # Stack the raster
    stacked_raster = np.stack([unveg_reshaped, low_reshaped, mid_reshaped], axis=0)
    
    # Define the metadata for export
    stacked_meta = {
        "driver": "GTiff",
        "height": stacked_raster.shape[1],
        "width": stacked_raster.shape[2],
        "count": stacked_raster.shape[0],
        "dtype": stacked_raster.dtype,
        "crs": metadata['crs'],
        "transform": metadata['transform'],
    }
    
    # Extract the name of the raster for export
    export_name = str(i).rsplit("/", 1)[-1]
    
    # Define the full export path
    export_path = './data/processed/wider_estuary_cover_maps/onehot/' + export_name
    
    # Set band names
    band_names = ["Unvegetated", "Low", "Mid"]
    
    # Export the image
    with rasterio.open(export_path, "w", **stacked_meta) as dest:
        for i in range(stacked_raster.shape[0]):
            dest.write(stacked_raster[i, :, :], indexes=i + 1)
            # Set band-specific metadata (tags)
            dest.set_band_description(i + 1, band_names[i])
    
    print(f'Exported {export_name} to drive')