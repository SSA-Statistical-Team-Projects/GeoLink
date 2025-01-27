
import os
import rasterio
import numpy as np
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
from tempfile import NamedTemporaryFile
from tqdm import tqdm
from rasterio.io import MemoryFile
import tempfile


def resample_rasters(input_files, output_folder, target_resolution=1000):
    # Ensure input files are valid
    if isinstance(input_files, str):
        input_files = [input_files]  # Handle case where input_files is a single string
    input_files = [f for f in input_files if f and os.path.exists(f)]  # Filter invalid paths
    
    if not input_files:
        raise FileNotFoundError("No valid input files provided.")

    # Create output folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    resampled_files = []  # Store paths of resampled rasters

    for input_file in tqdm(input_files, desc='Processing files'):
        # Double-check if the file exists
        if not os.path.exists(input_file):
            raise FileNotFoundError(f"Input file not found: {input_file}")

        # Create output filename with _1km appended
        base_filename = os.path.basename(input_file)
        filename_without_ext, ext = os.path.splitext(base_filename)
        output_file = os.path.join(output_folder, f'{filename_without_ext}_1km{ext}')

        # Skip if output file already exists
        if os.path.exists(output_file):
            try:
                os.remove(output_file)
            except PermissionError:
                print(f'Skipping {output_file}: File is in use.')
                continue

        # Open the input file for processing
        with rasterio.open(input_file) as src:
            # Calculate the new transform and dimensions
            transform, width, height = calculate_default_transform(
                src.crs, src.crs,
                src.width, src.height,
                *src.bounds,
                resolution=(target_resolution, target_resolution)
            )

            # Update metadata for the new raster
            kwargs = src.meta.copy()
            kwargs.update({
                'driver': 'GTiff',
                'height': height,
                'width': width,
                'transform': transform,
                'compress': 'lzw'  # Add compression to reduce file size
            })

            # Perform the resampling and save to the output file
            with rasterio.open(output_file, 'w', **kwargs) as dst:
                for i in range(1, src.count + 1):  # For each band
                    reproject(
                        source=rasterio.band(src, i),
                        destination=rasterio.band(dst, i),
                        src_transform=src.transform,
                        src_crs=src.crs,
                        dst_transform=transform,
                        dst_crs=src.crs,
                        resampling=Resampling.average
                    )

        # Append to the resampled file list and log the success
        resampled_files.append(output_file)
        print(f'Resampled raster saved to {output_file}')

    # Final validation of resampled files
    if not resampled_files:
        raise FileNotFoundError("No resampled files were created.")
    else:
        print(f"Resampled files: {resampled_files}")

    print('Resampling process completed.')
    return resampled_files



def mosaic_rasters(input_files):
  
    # Open the raster files using rasterio
    rasters = [rasterio.open(f) for f in input_files]

    # Merge the rasters using rasterio's merge function
    mosaic, out_transform = merge(rasters)

    # Get the metadata from the first raster
    out_meta = rasters[0].meta.copy()

    # Update the metadata to reflect the mosaic's size and transform
    out_meta.update({
        'driver': 'GTiff',
        'count': 1,  # Assuming single-band rasters, adjust for multi-band if necessary
        'dtype': mosaic.dtype,
        'width': mosaic.shape[2],
        'height': mosaic.shape[1],
        'crs': rasters[0].crs,
        'transform': out_transform
    })

    # Create a temporary file to store the mosaic
    with NamedTemporaryFile(delete=False, suffix='.tif') as tmpfile:
        tmpfile.close()  # Close the file so it can be accessed later

        # Use MemoryFile to create a memory-mapped file
        with MemoryFile() as memfile:
            with memfile.open(**out_meta) as dest:
                dest.write(mosaic[0], 1)  # Write the mosaic to memory-mapped file

            # Save the mosaic data to the temporary file
            with open(tmpfile.name, 'wb') as out_f:
                out_f.write(memfile.read())

        # Return the temporary file path
        return tmpfile.name


