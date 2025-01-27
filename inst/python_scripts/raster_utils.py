import os
import rasterio
import numpy as np
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
from tempfile import NamedTemporaryFile
from tqdm import tqdm
from rasterio.io import MemoryFile
import tempfile

def resample_rasters(input_files, output_folder, target_resolution):
    import rasterio
    from rasterio.warp import calculate_default_transform, reproject, Resampling
    import os
    import shutil
    from tqdm import tqdm

    # First try to clean up the output folder if it exists
    try:
        if os.path.exists(output_folder):
            shutil.rmtree(output_folder)
    except:
        pass
    
    # Create new output folder
    try:
        os.makedirs(output_folder, exist_ok=True)
    except Exception as e:
        # If we can't create/clear the original folder, create a new one in temp
        import tempfile
        output_folder = os.path.join(tempfile.gettempdir(), f'resampled_rasters_{os.getpid()}')
        os.makedirs(output_folder, exist_ok=True)

    resampled_files = []

    for input_file in tqdm(input_files, desc="Processing files"):
        try:
            with rasterio.open(input_file) as src:
                # Force the CRS to be EPSG:4326
                dst_crs = 'EPSG:4326'
                
                transform, width, height = calculate_default_transform(
                    src.crs, dst_crs,
                    src.width, src.height,
                    *src.bounds,
                    resolution=target_resolution
                )

                output_filename = os.path.join(
                    output_folder,
                    f"resampled_{os.path.basename(input_file)}"
                )

                kwargs = src.meta.copy()
                kwargs.update({
                    'crs': dst_crs,
                    'transform': transform,
                    'width': width,
                    'height': height
                })

                with rasterio.open(output_filename, 'w', **kwargs) as dst:
                    reproject(
                        source=rasterio.band(src, 1),
                        destination=rasterio.band(dst, 1),
                        src_transform=src.transform,
                        src_crs=src.crs,
                        dst_transform=transform,
                        dst_crs=dst_crs,
                        resampling=Resampling.nearest
                    )

                print(f"Resampled raster saved to {output_filename}")
                resampled_files.append(output_filename)
        except Exception as e:
            print(f"Error processing {input_file}: {str(e)}")
            continue

    if not resampled_files:
        raise Exception("No files were successfully resampled")

    print("Resampled files:", resampled_files)
    print("Resampling process completed.")
    return resampled_files
  
  
def mosaic_rasters(input_files):
    import rasterio
    from rasterio.merge import merge
    from rasterio.warp import calculate_default_transform, reproject, Resampling
    import os
    import tempfile
    
    target_crs = 'EPSG:4326'
    processed_files = []
    
    # Process each file to ensure consistent CRS
    for input_file in input_files:
        with rasterio.open(input_file) as src:
            # Check if reprojection is needed
            needs_reprojection = False
            
            # If no CRS, assign EPSG:4326
            if src.crs is None:
                current_crs = target_crs
                needs_reprojection = True
            # If CRS different from 4326, needs reprojection
            elif src.crs.to_string() != target_crs:
                current_crs = src.crs
                needs_reprojection = True
            else:
                processed_files.append(input_file)
                continue
                
            # Create temporary file for reprojected raster
            temp_file = os.path.join(tempfile.gettempdir(), f'reprojected_{os.path.basename(input_file)}')
            
            transform, width, height = calculate_default_transform(
                current_crs, target_crs,
                src.width, src.height,
                *src.bounds
            )
            
            kwargs = src.meta.copy()
            kwargs.update({
                'crs': target_crs,
                'transform': transform,
                'width': width,
                'height': height
            })
            
            with rasterio.open(temp_file, 'w', **kwargs) as dst:
                reproject(
                    source=rasterio.band(src, 1),
                    destination=rasterio.band(dst, 1),
                    src_transform=src.transform,
                    src_crs=current_crs,
                    dst_transform=transform,
                    dst_crs=target_crs,
                    resampling=Resampling.nearest
                )
            
            processed_files.append(temp_file)
    
    # Open all processed raster files
    src_files = [rasterio.open(f) for f in processed_files]
    
    try:
        # Merge the rasters
        mosaic, out_trans = merge(src_files)
        
        # Create output filename
        output_file = os.path.join(os.path.dirname(input_files[0]), 'mosaic.tif')
        
        # Copy the metadata from the first file
        out_meta = src_files[0].meta.copy()
        
        # Update the metadata
        out_meta.update({
            "driver": "GTiff",
            "height": mosaic.shape[1],
            "width": mosaic.shape[2],
            "transform": out_trans,
            "crs": target_crs
        })
        
        # Write the mosaic
        with rasterio.open(output_file, "w", **out_meta) as dest:
            dest.write(mosaic)
        
        return output_file
        
    finally:
        # Close all open files
        for src in src_files:
            src.close()
        
        # Clean up temporary files
        for file in processed_files:
            if 'reprojected_' in os.path.basename(file):
                try:
                    os.remove(file)
                except:
                    pass
