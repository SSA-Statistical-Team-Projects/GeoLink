import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
import os
from pathlib import Path
import numpy as np
from tqdm import tqdm
from rasterio.crs import CRS
import math
from rasterio.windows import Window
import tempfile
import logging
import sys

# Configure logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# Create handlers
console_handler = logging.StreamHandler(sys.stdout)
file_handler = logging.FileHandler('raster_utils.log')

# Set level for handlers
console_handler.setLevel(logging.INFO)
file_handler.setLevel(logging.INFO)

# Create formatter
formatter = logging.Formatter('%(asctime)s - %(levelname)s: %(message)s')
console_handler.setFormatter(formatter)
file_handler.setFormatter(formatter)

# Add handlers to the logger
logger.addHandler(console_handler)
logger.addHandler(file_handler)

def validate_input_files(input_files):
    # Validate and process input file paths.
    if isinstance(input_files, (str, Path)):
        input_files = [input_files]
    
    valid_files = []
    for file in input_files:
        file_path = Path(str(file)).resolve()
        if not file_path.exists():
            logger.warning(f"Input file not found: {file_path}")
            continue
        valid_files.append(file_path)
    
    if not valid_files:
        raise ValueError("No valid input files found")
    
    return valid_files

# Rest of the functions remain unchanged...

def ensure_crs_4326(raster_path, force_check=True):
    """
    Ensures a raster is in EPSG:4326, reprojecting if necessary.
    """
    try:
        input_path = Path(raster_path).resolve()
        if not input_path.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        with rasterio.open(str(input_path)) as src:
            src_crs = src.crs
            
            # Check if reprojection is needed
            needs_reprojection = force_check or (src_crs != CRS.from_epsg(4326))
            
            if not needs_reprojection:
                return str(input_path)
            
            # Setup reprojected output path
            output_path = input_path.parent / f"{input_path.stem}_4326{input_path.suffix}"
            
            # Calculate new transform
            transform, width, height = calculate_default_transform(
                src_crs,
                CRS.from_epsg(4326),
                src.width,
                src.height,
                *src.bounds
            )
            
            # Setup output metadata
            meta = src.meta.copy()
            meta.update({
                'crs': CRS.from_epsg(4326),
                'transform': transform,
                'width': width,
                'height': height
            })
            
            # Perform reprojection
            with rasterio.open(str(output_path), 'w', **meta) as dst:
                for i in range(1, src.count + 1):
                    reproject(
                        source=rasterio.band(src, i),
                        destination=rasterio.band(dst, i),
                        src_transform=src.transform,
                        src_crs=src_crs,
                        dst_transform=transform,
                        dst_crs=CRS.from_epsg(4326),
                        resampling=Resampling.nearest
                    )
            
            logger.info(f"Reprojected {input_path} to EPSG:4326")
            return str(output_path)
    
    except Exception as e:
        logger.error(f"Error ensuring EPSG:4326 for {raster_path}: {e}")
        raise

def chunk_raster(src, chunk_size=1000):
    """
    Generator function to yield raster chunks for memory-efficient processing.
    """
    width = src.width
    height = src.height
    
    x_chunks = math.ceil(width / chunk_size)
    y_chunks = math.ceil(height / chunk_size)
    
    for x in range(x_chunks):
        for y in range(y_chunks):
            x_offset = x * chunk_size
            y_offset = y * chunk_size
            width_chunk = min(chunk_size, width - x_offset)
            height_chunk = min(chunk_size, height - y_offset)
            
            window = Window(x_offset, y_offset, width_chunk, height_chunk)
            transform = src.window_transform(window)
            
            yield window, transform

def get_raster_info(raster_path):
    """
    Get comprehensive information about a raster file for debugging purposes.
    """
    try:
        with rasterio.open(str(raster_path)) as src:
            info = {
                'filename': str(raster_path),
                'crs': str(src.crs),
                'epsg': src.crs.to_epsg(),
                'bounds': src.bounds,
                'shape': src.shape,
                'resolution': src.res,
                'transform': src.transform,
                'nodata': src.nodata,
                'dtype': src.dtypes[0],
                'memory_estimate_gb': (src.width * src.height * np.dtype(src.dtypes[0]).itemsize) / (1024**3)
            }
        return info
    except Exception as e:
        logger.error(f"Error getting raster info for {raster_path}: {e}")
        return {'error': str(e), 'filename': str(raster_path)}

def resample_raster(input_file, output_file, target_resolution, resampling_method=Resampling.nearest):
    """
    Resample a single raster to a target resolution while ensuring EPSG:4326 CRS.
    
    :param input_file: Path to input raster file
    :param output_file: Path to output raster file
    :param target_resolution: Target resolution in meters
    :param resampling_method: Resampling method (default: nearest neighbor)
    :return: Path to resampled raster file
    """
    try:
        # First ensure input is in EPSG:4326
        input_file = ensure_crs_4326(input_file)
        
        # Convert paths to Path objects and resolve them
        input_path = Path(input_file).resolve()
        output_path = Path(output_file).resolve()
        
        # Create output directory if it doesn't exist
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with rasterio.open(str(input_path)) as src:
            # Calculate new transform and dimensions
            degree_resolution = target_resolution / 111320  # Convert meters to degrees
            
            transform, width, height = calculate_default_transform(
                src.crs,
                CRS.from_epsg(4326),
                src.width,
                src.height,
                *src.bounds,
                resolution=(degree_resolution, degree_resolution)
            )
            
            # Setup output metadata
            meta = src.meta.copy()
            meta.update({
                'transform': transform,
                'width': width,
                'height': height,
                'crs': CRS.from_epsg(4326)
            })
            
            # Reproject and resample
            with rasterio.open(str(output_path), 'w', **meta) as dst:
                for i in range(1, src.count + 1):
                    reproject(
                        source=rasterio.band(src, i),
                        destination=rasterio.band(dst, i),
                        src_transform=src.transform,
                        src_crs=src.crs,
                        dst_transform=transform,
                        dst_crs=CRS.from_epsg(4326),
                        resampling=resampling_method
                    )
        
        logger.info(f"Resampled {input_path} to {target_resolution}m resolution")
        return str(output_path)
        
    except Exception as e:
        logger.error(f"Error resampling raster {input_file}: {e}")
        raise RuntimeError(f"Resampling failed for {input_file}: {e}")

def resample_rasters(input_files, output_folder, target_resolution):
    """
    Resample multiple rasters to a common resolution in EPSG:4326.
    
    :param input_files: List of input raster files or single file path
    :param output_folder: Folder to save resampled rasters
    :param target_resolution: Target resolution in meters
    :return: List of paths to resampled raster files
    """
    # Validate and prepare input files
    valid_files = validate_input_files(input_files)
    
    # Create output directory
    output_folder = Path(output_folder).resolve()
    output_folder.mkdir(parents=True, exist_ok=True)
    
    resampled_files = []
    
    # Process each file
    for input_file in tqdm(valid_files, desc="Resampling rasters"):
        try:
            output_file = output_folder / f"{input_file.stem}_resampled.tif"
            resampled_path = resample_raster(
                str(input_file),
                str(output_file),
                target_resolution
            )
            resampled_files.append(resampled_path)
        except Exception as e:
            logger.warning(f"Could not process {input_file}: {e}")
            continue
    
    if not resampled_files:
        raise ValueError("No rasters were successfully resampled")
    
    return resampled_files

def mosaic_rasters(input_files, output_path=None, chunk_size=1000, target_resolution=None):
    """
    Mosaic multiple rasters with chunked processing for memory efficiency.
    
    :param input_files: List of input raster file paths
    :param output_path: Optional path for final mosaic (if None, a temporary path is used)
    :param chunk_size: Size of processing chunks
    :param target_resolution: Force a specific target resolution (in degrees for EPSG:4326)
    :return: Path to final mosaic raster
    """
    # Validate input files
    valid_files = validate_input_files(input_files)
    
    if len(valid_files) == 1:
        logger.info("Only one raster provided. Returning as-is.")
        return str(valid_files[0])
    
    # Create temporary directory for intermediate files
    temp_dir = Path(tempfile.mkdtemp())
    temp_files = []
    
    try:
        # Ensure all rasters are in EPSG:4326
        processed_paths = []
        for file in valid_files:
            processed_path = ensure_crs_4326(file)
            processed_paths.append(processed_path)
        
        # Get information about all rasters
        raster_info = []
        for path in processed_paths:
            with rasterio.open(path) as src:
                raster_info.append({
                    'path': path,
                    'res': src.res,
                    'crs': src.crs,
                    'bounds': src.bounds,
                    'shape': src.shape
                })
        
        # Determine target resolution
        if target_resolution is None:
            # Find the coarsest resolution among all rasters to prevent data loss
            x_res = max([info['res'][0] for info in raster_info])
            y_res = max([info['res'][1] for info in raster_info])
            target_res = (x_res, y_res)
            logger.info(f"Using coarsest resolution for mosaic: {target_res}")
        else:
            # Use the specified target resolution
            if isinstance(target_resolution, (int, float)):
                target_res = (target_resolution, target_resolution)
            else:
                target_res = target_resolution
            logger.info(f"Using specified resolution for mosaic: {target_res}")
        
        # Resample rasters to the same resolution if needed
        resampled_paths = []
        for info in raster_info:
            path = info['path']
            
            # Check if resampling is needed
            if info['res'] != target_res:
                logger.info(f"Resampling {path} from {info['res']} to {target_res}")
                temp_output = temp_dir / f"resampled_{Path(path).name}"
                
                with rasterio.open(path) as src:
                    # Calculate new transform
                    transform = rasterio.transform.from_origin(
                        src.bounds.left, src.bounds.top, target_res[0], target_res[1]
                    )
                    
                    # Calculate new dimensions
                    width = max(1, round((src.bounds.right - src.bounds.left) / target_res[0]))
                    height = max(1, round((src.bounds.top - src.bounds.bottom) / target_res[1]))
                    
                    # Setup output metadata
                    meta = src.meta.copy()
                    meta.update({
                        'transform': transform,
                        'width': width,
                        'height': height,
                        'crs': CRS.from_epsg(4326)
                    })
                    
                    # Reproject and resample
                    with rasterio.open(str(temp_output), 'w', **meta) as dst:
                        for i in range(1, src.count + 1):
                            reproject(
                                source=rasterio.band(src, i),
                                destination=rasterio.band(dst, i),
                                src_transform=src.transform,
                                src_crs=src.crs,
                                dst_transform=transform,
                                dst_crs=CRS.from_epsg(4326),
                                resampling=Resampling.nearest
                            )
                
                resampled_paths.append(str(temp_output))
                temp_files.append(temp_output)
            else:
                resampled_paths.append(path)
        
        # Group files for processing
        group_size = 2  # Process two files at a time
        final_paths = []
        
        for i in range(0, len(resampled_paths), group_size):
            group = resampled_paths[i:i + group_size]
            
            if len(group) == 1:
                final_paths.append(group[0])
                continue
            
            # Create temporary output for this group
            temp_output = temp_dir / f"temp_mosaic_{i}.tif"
            
            # Open source files
            src_files = [rasterio.open(p) for p in group]
            
            try:
                # Verify all files have the same resolution before merging
                resolutions = set(src.res for src in src_files)
                if len(resolutions) > 1:
                    logger.warning(f"Files still have different resolutions: {resolutions}")
                    logger.warning("Will force resampling to exact resolution")
                    
                    # Close files and resample again with stricter parameters
                    for src in src_files:
                        src.close()
                    
                    # Resample this group with exact resolution
                    resampled_group = []
                    for j, path in enumerate(group):
                        strict_output = temp_dir / f"strict_resampled_{i}_{j}.tif"
                        with rasterio.open(path) as src:
                            # Get exact dimensions
                            exact_transform = rasterio.transform.from_origin(
                                src.bounds.left, src.bounds.top, target_res[0], target_res[1]
                            )
                            exact_width = max(1, round((src.bounds.right - src.bounds.left) / target_res[0]))
                            exact_height = max(1, round((src.bounds.top - src.bounds.bottom) / target_res[1]))
                            
                            # Create exact metadata
                            meta = src.meta.copy()
                            meta.update({
                                'transform': exact_transform,
                                'width': exact_width,
                                'height': exact_height,
                                'crs': CRS.from_epsg(4326),
                                'dtype': src.dtypes[0]
                            })
                            
                            # Perform exact resampling
                            with rasterio.open(str(strict_output), 'w', **meta) as dst:
                                for band in range(1, src.count + 1):
                                    reproject(
                                        source=rasterio.band(src, band),
                                        destination=rasterio.band(dst, band),
                                        src_transform=src.transform,
                                        src_crs=src.crs,
                                        dst_transform=exact_transform,
                                        dst_crs=CRS.from_epsg(4326),
                                        resampling=Resampling.nearest
                                    )
                        
                        resampled_group.append(str(strict_output))
                        temp_files.append(strict_output)
                    
                    # Reopen with strict resolution
                    src_files = [rasterio.open(p) for p in resampled_group]
                
                # Attempt merge with verified rasters
                mosaic, mosaic_transform = merge(src_files)
                
                # Get metadata from first file
                meta = src_files[0].meta.copy()
                meta.update({
                    'height': mosaic.shape[1],
                    'width': mosaic.shape[2],
                    'transform': mosaic_transform,
                    'crs': CRS.from_epsg(4326)
                })
                
                # Write mosaic
                with rasterio.open(str(temp_output), 'w', **meta) as dst:
                    dst.write(mosaic)
                
                final_paths.append(str(temp_output))
                temp_files.append(temp_output)
                
            except Exception as merge_error:
                logger.error(f"Error during merge operation: {merge_error}")
                # Fallback to manual mosaic
                try:
                    logger.info("Attempting manual mosaic as fallback")
                    # Create an empty template raster covering the full extent
                    all_bounds = [src.bounds for src in src_files]
                    left = min(b.left for b in all_bounds)
                    bottom = min(b.bottom for b in all_bounds)
                    right = max(b.right for b in all_bounds)
                    top = max(b.top for b in all_bounds)
                    
                    width = max(1, round((right - left) / target_res[0]))
                    height = max(1, round((top - bottom) / target_res[1]))
                    transform = rasterio.transform.from_origin(left, top, target_res[0], target_res[1])
                    
                    # Get reference metadata
                    meta = src_files[0].meta.copy()
                    meta.update({
                        'height': height,
                        'width': width,
                        'transform': transform,
                        'crs': CRS.from_epsg(4326)
                    })
                    
                    # Create empty raster
                    with rasterio.open(str(temp_output), 'w', **meta) as dst:
                        # Initialize with nodata
                        empty_data = np.full((meta['count'], height, width), meta.get('nodata', 0), dtype=meta['dtype'])
                        dst.write(empty_data)
                    
                    # Fill with data from each source
                    for src in src_files:
                        with rasterio.open(str(temp_output), 'r+') as dst:
                            # Reproject each source into the destination
                            for i in range(1, src.count + 1):
                                reproject(
                                    source=rasterio.band(src, i),
                                    destination=rasterio.band(dst, i),
                                    src_transform=src.transform,
                                    src_crs=src.crs,
                                    dst_transform=transform,
                                    dst_crs=CRS.from_epsg(4326),
                                    resampling=Resampling.nearest
                                )
                    
                    final_paths.append(str(temp_output))
                    temp_files.append(temp_output)
                    
                except Exception as fallback_error:
                    logger.error(f"Fallback mosaic also failed: {fallback_error}")
                    raise
                
            finally:
                for src in src_files:
                    src.close()
        
        # Final mosaic if needed
        if len(final_paths) == 1:
            final_output = final_paths[0]
        else:
            # Recursive call with the intermediate mosaics
            final_output = mosaic_rasters(final_paths, target_resolution=target_res)
        
        # Move to requested output location if specified
        if output_path is not None:
            output_path = Path(output_path)
            output_path.parent.mkdir(parents=True, exist_ok=True)
            os.replace(final_output, str(output_path))
            logger.info(f"Final mosaic saved to {output_path}")
            return str(output_path)
        
        return final_output
        
    except Exception as e:
        logger.error(f"Mosaic failed: {e}")
        raise
    
    finally:
        # Cleanup temporary files
        for temp_file in temp_files:
            try:
                if os.path.exists(str(temp_file)):
                    os.remove(str(temp_file))
            except Exception as cleanup_error:
                logger.warning(f"Could not remove temp file {temp_file}: {cleanup_error}")
        try:
            if os.path.exists(str(temp_dir)):
                os.rmdir(str(temp_dir))
        except Exception as cleanup_error:
            logger.warning(f"Could not remove temp directory {temp_dir}: {cleanup_error}")
