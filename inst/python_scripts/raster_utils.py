import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.merge import merge
import os
from pathlib import Path
import numpy as np
import math
from rasterio.crs import CRS
from rasterio.windows import Window
import tempfile
import concurrent.futures
import multiprocessing as mp
import warnings
import traceback

def ensure_crs_4326(raster_path, force_check=True, output_dir=None, use_tmp=False):
    """
    Ensures a raster is in EPSG:4326, reprojecting if necessary.
    
    Args:
        raster_path: Path to the input raster
        force_check: Whether to force CRS checking
        output_dir: Directory to save output (if None, uses input directory)
        use_tmp: If True, uses a temporary directory instead of output_dir
        
    Returns:
        Path to 4326-projected raster
    """
    try:
        print(f"Ensuring CRS 4326 for {raster_path}")
        input_path = Path(raster_path).resolve()
        if not input_path.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        # Determine output path
        if use_tmp:
            tmp_dir = tempfile.mkdtemp()
            output_path = Path(tmp_dir) / f"{input_path.stem}_4326{input_path.suffix}"
        elif output_dir:
            output_dir = Path(output_dir)
            output_dir.mkdir(parents=True, exist_ok=True)
            output_path = output_dir / f"{input_path.stem}_4326{input_path.suffix}"
        else:
            output_path = input_path.parent / f"{input_path.stem}_4326{input_path.suffix}"
        
        with rasterio.open(str(input_path)) as src:
            src_crs = src.crs
            
            # Print info about the source raster
            print(f"Source raster info: dims={src.width}x{src.height}, crs={src_crs}")
            
            # Quick check: if already 4326 and not forcing check, return original
            if not force_check and src_crs == CRS.from_epsg(4326):
                print("CRS is already EPSG:4326.")
                return str(input_path)
            
            # Check if reprojection is needed
            needs_reprojection = force_check or (src_crs != CRS.from_epsg(4326))
            
            if not needs_reprojection:
                print("CRS is already EPSG:4326.")
                return str(input_path)
            
            # Print min/max values in source raster
            try:
                sample_data = src.read(1, masked=True)
                print(f"Source data range: min={np.min(sample_data)}, max={np.max(sample_data)}")
                unique_vals = np.unique(sample_data)
                if len(unique_vals) < 20:  # Only print if there are few unique values
                    print(f"Unique values: {unique_vals}")
            except Exception as e:
                print(f"Warning: Could not read sample data: {e}")
            
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
            
            print(f"Reprojecting to 4326: output dims={width}x{height}")
            
            # Process in a simple way for most reliable results
            with rasterio.open(str(output_path), 'w', **meta) as dst:
                for i in range(1, src.count + 1):
                    reproject(
                        source=rasterio.band(src, i),
                        destination=rasterio.band(dst, i),
                        src_transform=src.transform,
                        src_crs=src.crs,
                        dst_transform=transform,
                        dst_crs=CRS.from_epsg(4326),
                        resampling=Resampling.nearest,
                        src_nodata=src.nodata,
                        dst_nodata=meta.get('nodata')
                    )
            
            # Verify the result
            with rasterio.open(str(output_path)) as dst:
                print(f"Output raster info: dims={dst.width}x{dst.height}, crs={dst.crs}")
                try:
                    output_data = dst.read(1, masked=True)
                    print(f"Output data range: min={np.min(output_data)}, max={np.max(output_data)}")
                    unique_vals = np.unique(output_data)
                    if len(unique_vals) < 20:  # Only print if there are few unique values
                        print(f"Unique values: {unique_vals}")
                except Exception as e:
                    print(f"Warning: Could not read output data: {e}")
            
            return str(output_path)
    
    except Exception as e:
        print(f"Error ensuring EPSG:4326 for {raster_path}: {e}")
        traceback.print_exc()
        raise


def resample_raster(input_file, output_file, target_resolution=1000, 
                    resampling_method=Resampling.nearest):
    """
    Resample a single raster to a target resolution while ensuring EPSG:4326 CRS.
    
    Args:
        input_file: Path to input raster file
        output_file: Path to output raster file
        target_resolution: Target resolution in meters (default: 1000m or 1km)
        resampling_method: Resampling method (default: nearest neighbor)
        
    Returns:
        Path to resampled raster file
    """
    try:
        print(f"Resampling raster {input_file} to {target_resolution}m resolution")
        
        # First ensure input is in EPSG:4326
        input_file = ensure_crs_4326(input_file)
        
        # Convert paths to Path objects and resolve them
        input_path = Path(input_file).resolve()
        output_path = Path(output_file).resolve()
        
        # Create output directory if it doesn't exist
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with rasterio.open(str(input_path)) as src:
            # Print info about source raster
            print(f"Source raster for resampling: dims={src.width}x{src.height}, crs={src.crs}")
            
            # Calculate new transform and dimensions
            # Convert meters to degrees (approximate at equator)
            degree_resolution = target_resolution / 111320
            print(f"Target resolution: {target_resolution}m = {degree_resolution} degrees")
            
            # Calculate new dimensions based on bounds and target resolution
            x_min, y_min, x_max, y_max = src.bounds
            width = max(1, round((x_max - x_min) / degree_resolution))
            height = max(1, round((y_max - y_min) / degree_resolution))
            
            # Calculate the transform for the new dimensions
            transform = rasterio.transform.from_bounds(x_min, y_min, x_max, y_max, width, height)
            
            print(f"Resampling to dims={width}x{height}")
            
            # Setup output metadata
            meta = src.meta.copy()
            meta.update({
                'transform': transform,
                'width': width,
                'height': height,
                'crs': CRS.from_epsg(4326)
            })
            
            # Perform resampling with the calculated transform
            with rasterio.open(str(output_path), 'w', **meta) as dst:
                for i in range(1, src.count + 1):
                    # Read source data
                    source_data = src.read(i)
                    
                    # Create destination array
                    destination_data = np.zeros((height, width), dtype=meta['dtype'])
                    
                    # Reproject
                    reproject(
                        source=source_data,
                        destination=destination_data,
                        src_transform=src.transform,
                        src_crs=src.crs,
                        dst_transform=transform,
                        dst_crs=CRS.from_epsg(4326),
                        resampling=resampling_method,
                        src_nodata=src.nodata,
                        dst_nodata=meta.get('nodata')
                    )
                    
                    # Check for data loss - print warning if most values are nodata
                    valid_count = np.sum(destination_data != meta.get('nodata'))
                    total_count = width * height
                    valid_percent = (valid_count / total_count) * 100
                    
                    print(f"Band {i}: {valid_percent:.2f}% valid data after resampling")
                    
                    if valid_percent < 10:
                        print("WARNING: Significant data loss during resampling!")
                    
                    # Write to output
                    dst.write(destination_data, indexes=i)
        
        # Verify the output
        with rasterio.open(str(output_path)) as dst:
            print(f"Resampled raster info: dims={dst.width}x{dst.height}, crs={dst.crs}")
            try:
                output_data = dst.read(1, masked=True)
                print(f"Output data range: min={np.min(output_data)}, max={np.max(output_data)}")
                unique_vals = np.unique(output_data)
                if len(unique_vals) < 20:  # Only print if there are few unique values
                    print(f"Unique values: {unique_vals}")
            except Exception as e:
                print(f"Warning: Could not read output data: {e}")
        
        print(f"Successfully resampled {input_path} to {target_resolution}m resolution")
        return str(output_path)
        
    except Exception as e:
        print(f"Error resampling raster {input_file}: {e}")
        traceback.print_exc()
        raise RuntimeError(f"Resampling failed for {input_file}: {e}")


def resample_rasters(input_files, output_folder, target_resolution=1000, n_jobs=None):
    """
    Resample multiple rasters to a common resolution in EPSG:4326.
    
    Args:
        input_files: List of input raster files or single file path
        output_folder: Folder to save resampled rasters
        target_resolution: Target resolution in meters (default: 1000m or 1km)
        n_jobs: Number of parallel jobs (not used in simplified version)
        
    Returns:
        List of paths to resampled raster files
    """
    # Basic validation
    if isinstance(input_files, (str, Path)):
        input_files = [input_files]
    
    valid_files = []
    for file in input_files:
        file_path = Path(str(file)).resolve()
        if file_path.exists():
            valid_files.append(file_path)
        else:
            warnings.warn(f"Input file not found: {file_path}")
    
    if not valid_files:
        raise ValueError("No valid input files found")
    
    # Create output directory
    output_folder = Path(output_folder).resolve()
    output_folder.mkdir(parents=True, exist_ok=True)
    
    resampled_files = []
    
    # Process each file (no parallelism for more reliable operation)
    for input_file in valid_files:
        try:
            print(f"Processing file: {input_file}")
            output_file = output_folder / f"{input_file.stem}_resampled.tif"
            resampled_path = resample_raster(
                input_file=str(input_file),
                output_file=str(output_file),
                target_resolution=target_resolution
            )
            resampled_files.append(resampled_path)
        except Exception as e:
            print(f"Could not process {input_file}: {e}")
            traceback.print_exc()
    
    if not resampled_files:
        raise ValueError("No rasters were successfully resampled")
    
    return resampled_files


def mosaic_rasters(input_files, output_file=None):
    """
    Mosaic multiple rasters into a single raster.
    
    Args:
        input_files: List of input raster file paths
        output_file: Path to output mosaic file (optional, generates temp file if None)
        
    Returns:
        Path to mosaic raster
    """
    try:
        # Basic validation
        if isinstance(input_files, (str, Path)):
            input_files = [input_files]
        
        valid_files = []
        for file in input_files:
            file_path = Path(str(file)).resolve()
            if file_path.exists():
                valid_files.append(file_path)
            else:
                warnings.warn(f"Input file not found: {file_path}")
        
        if len(valid_files) == 0:
            raise ValueError("No valid input files found")
        
        if len(valid_files) == 1:
            print(f"Only one valid file found, returning it: {valid_files[0]}")
            return str(valid_files[0])
        
        # Create output path if not provided
        if output_file is None:
            temp_dir = tempfile.mkdtemp()
            output_file = os.path.join(temp_dir, "mosaic.tif")
        
        print(f"Mosaicking {len(valid_files)} rasters to {output_file}")
        
        # Ensure output directory exists
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)
        
        # Open all input rasters
        src_files = []
        try:
            for file_path in valid_files:
                src = rasterio.open(str(file_path))
                # Verify raster has valid data
                data = src.read(1, masked=True)
                valid_data = np.sum(~data.mask)
                if valid_data == 0:
                    print(f"WARNING: File contains no valid data: {file_path}")
                    src.close()
                else:
                    print(f"File has {valid_data} valid pixels: {file_path}")
                    src_files.append(src)
            
            if len(src_files) == 0:
                raise ValueError("No input files contain valid data")
            
            # Mosaic the rasters
            mosaic, out_transform = merge(src_files)
            
            # Create metadata from first file
            meta = src_files[0].meta.copy()
            meta.update({
                "driver": "GTiff",
                "height": mosaic.shape[1],
                "width": mosaic.shape[2],
                "transform": out_transform
            })
            
            # Write mosaic
            with rasterio.open(output_file, 'w', **meta) as dst:
                dst.write(mosaic)
            
            # Verify the output mosaic
            with rasterio.open(output_file) as dst:
                print(f"Mosaic info: dims={dst.width}x{dst.height}, crs={dst.crs}")
                try:
                    mosaic_data = dst.read(1, masked=True)
                    valid_data = np.sum(~mosaic_data.mask)
                    print(f"Mosaic has {valid_data} valid pixels")
                    unique_vals = np.unique(mosaic_data.data[~mosaic_data.mask])
                    if len(unique_vals) < 20:  # Only print if there are few unique values
                        print(f"Unique values in mosaic: {unique_vals}")
                except Exception as e:
                    print(f"Warning: Could not analyze mosaic data: {e}")
            
            return output_file
            
        finally:
            # Close all open files
            for src in src_files:
                if src:
                    src.close()
    
    except Exception as e:
        print(f"Error mosaicking rasters: {e}")
        traceback.print_exc()
        if input_files and len(input_files) > 0:
            # Return the first file in case of error
            return str(input_files[0])
        else:
            raise
