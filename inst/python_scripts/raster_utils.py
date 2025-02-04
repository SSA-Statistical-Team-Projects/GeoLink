import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
import os
import numpy as np
from tqdm import tqdm
from rasterio.crs import CRS

def resample_raster(input_file, output_file, target_resolution):
    """
    Resample a single raster to a target resolution and EPSG:4326 CRS.
    """
    with rasterio.open(input_file) as src:
        # Set target CRS to EPSG:4326
        dst_crs = CRS.from_epsg(4326)
        
        # Calculate new transform and dimensions
        transform, width, height = calculate_default_transform(
            src.crs,
            dst_crs,
            src.width,
            src.height,
            src.bounds.left,
            src.bounds.bottom,
            src.bounds.right,
            src.bounds.top,
            resolution=(target_resolution/111320, target_resolution/111320)  # Convert to degrees
        )
        
        # Setup output metadata
        meta = src.meta.copy()
        meta.update({
            'transform': transform,
            'width': width,
            'height': height,
            'crs': dst_crs
        })
        
        with rasterio.open(output_file, 'w', **meta) as dst:
            reproject(
                source=rasterio.band(src, 1),
                destination=rasterio.band(dst, 1),
                src_transform=src.transform,
                src_crs=src.crs,
                dst_transform=transform,
                dst_crs=dst_crs,
                resampling=Resampling.nearest
            )

def resample_rasters(input_files, output_folder, target_resolution):
    """
    Resample multiple rasters to a common resolution in EPSG:4326.
    """
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
    
    resampled_files = []
    
    print("Processing files: ", end='')
    for i, input_file in enumerate(tqdm(input_files)):
        basename = os.path.basename(input_file)
        output_file = os.path.join(output_folder, f"{os.path.splitext(basename)[0]}_1km.tif")
        
        resample_raster(input_file, output_file, target_resolution)
        resampled_files.append(output_file)
        print(f"Resampled raster saved to {output_file}")
    
    print("Resampled files:", resampled_files)
    print("Resampling process completed.")
    return resampled_files

def mosaic_rasters(input_files):
    """
    Mosaic multiple rasters in EPSG:4326.
    """
    # Open all raster files
    src_files = []
    for file in input_files:
        src = rasterio.open(file)
        if src.crs != CRS.from_epsg(4326):
            raise ValueError(f"Input raster {file} must be in EPSG:4326")
        src_files.append(src)
    
    # Perform mosaic operation
    mosaic, out_trans = merge(src_files)
    
    # Create output metadata
    out_meta = src_files[0].meta.copy()
    out_meta.update({
        "height": mosaic.shape[1],
        "width": mosaic.shape[2],
        "transform": out_trans,
        "crs": CRS.from_epsg(4326)
    })
    
    # Create temporary output file
    output_path = os.path.join(os.path.dirname(input_files[0]), "mosaic_output.tif")
    with rasterio.open(output_path, "w", **out_meta) as dest:
        dest.write(mosaic)
    
    # Close all source files
    for src in src_files:
        src.close()
    
    return output_path
