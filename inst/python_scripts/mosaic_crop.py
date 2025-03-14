import sys
import os
import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
import geopandas as gpd
from pathlib import Path
import numpy as np

def mosaic_and_crop(input_files, shapefile_path, output_path):
    """
    Mosaic multiple rasters and crop to shapefile boundary
    
    Parameters:
    -----------
    input_files : list
        List of paths to input raster files
    shapefile_path : str
        Path to the shapefile used for cropping
    output_path : str
        Path where the final cropped mosaic will be saved
        
    Returns:
    --------
    str or None
        Path to the output file if successful, None otherwise
    """
    print(f"Mosaicking {len(input_files)} rasters and cropping to shapefile")
    
    # Step 1: Mosaic the rasters
    src_files = []
    for file in input_files:
        try:
            src = rasterio.open(file)
            src_files.append(src)
        except Exception as e:
            print(f"Error opening {file}: {e}")
    
    if len(src_files) == 0:
        print("No valid raster files found")
        return None
    
    if len(src_files) == 1:
        # Only one raster, no need to mosaic
        mosaic = src_files[0].read()
        mosaic_transform = src_files[0].transform
        mosaic_meta = src_files[0].meta.copy()
    else:
        # Multiple rasters, perform mosaic
        try:
            mosaic, mosaic_transform = merge(src_files)
            mosaic_meta = src_files[0].meta.copy()
            mosaic_meta.update({
                "height": mosaic.shape[1],
                "width": mosaic.shape[2],
                "transform": mosaic_transform
            })
        except Exception as e:
            print(f"Error during mosaic: {e}")
            for src in src_files:
                src.close()
            return None
    
    # Step 2: Save the mosaic to a temporary file
    output_dir = os.path.dirname(output_path)
    os.makedirs(output_dir, exist_ok=True)
    temp_mosaic = os.path.join(output_dir, "temp_mosaic.tif")
    with rasterio.open(temp_mosaic, "w", **mosaic_meta) as dst:
        dst.write(mosaic)
    
    # Close all source files
    for src in src_files:
        src.close()
    
    # Step 3: Crop the mosaic using the shapefile
    try:
        # Read the shapefile
        gdf = gpd.read_file(shapefile_path)
        
        # Ensure shapefile is in EPSG:4326
        if gdf.crs is None:
            print("Warning: Shapefile has no CRS defined. Assuming EPSG:4326")
        elif gdf.crs != "EPSG:4326":
            print(f"Reprojecting shapefile from {gdf.crs} to EPSG:4326")
            gdf = gdf.to_crs("EPSG:4326")
        
        # Get geometries for masking
        shapes = [feature for feature in gdf.geometry]
        
        if not shapes:
            print("No valid geometries found in shapefile")
            return None
            
        # Open the mosaic and crop
        with rasterio.open(temp_mosaic) as src:
            from rasterio.mask import mask
            out_image, out_transform = mask(
                src, 
                shapes, 
                crop=True, 
                all_touched=True,
                nodata=src.nodata if src.nodata is not None else 0
            )
            
            # Check if the output is valid
            if np.all(out_image == src.nodata if src.nodata is not None else 0):
                print("Warning: Cropped raster contains only nodata values")
            
            # Update metadata
            out_meta = src.meta.copy()
            out_meta.update({
                "driver": "GTiff",
                "height": out_image.shape[1],
                "width": out_image.shape[2],
                "transform": out_transform
            })
            
            # Create output directory if it doesn't exist
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
            
            # Write the cropped raster
            with rasterio.open(output_path, "w", **out_meta) as dest:
                dest.write(out_image)
        
        print(f"Successfully created mosaic and cropped to: {output_path}")
        
        # Clean up temporary mosaic
        try:
            os.remove(temp_mosaic)
        except Exception as e:
            print(f"Warning: Could not remove temporary file {temp_mosaic}: {e}")
        
        return output_path
        
    except Exception as e:
        print(f"Error during crop operation: {e}")
        import traceback
        traceback.print_exc()
        return None

# Main execution
if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: python mosaic_crop.py <shapefile_path> <output_path> <input_file1> [<input_file2> ...]")
        sys.exit(1)
    
    shapefile_path = sys.argv[1]
    output_path = sys.argv[2]
    input_files = sys.argv[3:]
    
    result = mosaic_and_crop(input_files, shapefile_path, output_path)
    
    if result:
        print(f"SUCCESS:{result}")
    else:
        print("FAILED")
        sys.exit(1)
