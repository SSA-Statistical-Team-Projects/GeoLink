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
import subprocess
import xml.etree.ElementTree as ET
import shutil

def ensure_crs_4326(raster_path, force_check=True, output_dir=None, use_tmp=False, use_vrt=True):
    """
    Ensures a raster is in EPSG:4326, reprojecting if necessary.
    Uses VRT for reprojection when possible to avoid creating large files.
    
    Args:
        raster_path: Path to the input raster
        force_check: Whether to force CRS checking
        output_dir: Directory to save output (if None, uses input directory)
        use_tmp: If True, uses a temporary directory instead of output_dir
        use_vrt: If True, uses VRT instead of creating a new GeoTIFF file
        
    Returns:
        Path to 4326-projected raster (either VRT or GeoTIFF)
    """
    try:
        print(f"Ensuring CRS 4326 for {raster_path}")
        input_path = Path(raster_path).resolve()
        if not input_path.exists():
            raise FileNotFoundError(f"Input file not found: {input_path}")
        
        # Determine output path and extension
        extension = ".vrt" if use_vrt else input_path.suffix
        
        if use_tmp:
            tmp_dir = tempfile.mkdtemp()
            output_path = Path(tmp_dir) / f"{input_path.stem}_4326{extension}"
        elif output_dir:
            output_dir = Path(output_dir)
            output_dir.mkdir(parents=True, exist_ok=True)
            output_path = output_dir / f"{input_path.stem}_4326{extension}"
        else:
            output_path = input_path.parent / f"{input_path.stem}_4326{extension}"
        
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
            
            print(f"Reprojecting to 4326: output dims={width}x{height}")
            
            if use_vrt:
                # Create a VRT for reprojection
                try:
                    # Use GDAL command-line tools to create the VRT
                    gdal_command = [
                        "gdalwarp",
                        "-of", "VRT",
                        "-t_srs", "EPSG:4326",
                        str(input_path),
                        str(output_path)
                    ]
                    
                    # Add nodata if specified
                    if src.nodata is not None:
                        gdal_command.extend(["-srcnodata", str(src.nodata), "-dstnodata", str(src.nodata)])
                    
                    # Execute the command
                    print(f"Running: {' '.join(gdal_command)}")
                    subprocess.run(gdal_command, check=True)
                    
                    # Verify VRT was created
                    if output_path.exists():
                        print(f"Created VRT for reprojection: {output_path}")
                        
                        # Verify the VRT
                        with rasterio.open(str(output_path)) as dst:
                            print(f"VRT info: dims={dst.width}x{dst.height}, crs={dst.crs}")
                            
                        return str(output_path)
                    else:
                        print("VRT creation failed, falling back to standard reprojection")
                except Exception as e:
                    print(f"Error creating VRT: {e}")
                    print("Falling back to standard reprojection")
            
            # Fall back to standard reprojection if VRT failed or is not requested
            # Setup output metadata
            meta = src.meta.copy()
            meta.update({
                'crs': CRS.from_epsg(4326),
                'transform': transform,
                'width': width,
                'height': height,
                'driver': 'GTiff'  # Explicitly set to GTiff if not using VRT
            })
            
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


def create_vrt_raster(input_file, output_file, target_resolution=None, 
                     resampling_method="nearest", target_crs="EPSG:4326"):
    """
    Create a Virtual Raster (VRT) from an input raster with optional resampling
    
    Args:
        input_file: Path to input raster file
        output_file: Path to output VRT file
        target_resolution: Target resolution in degrees if resampling
        resampling_method: Resampling method (default: nearest)
        target_crs: Target CRS (default: EPSG:4326)
        
    Returns:
        Path to VRT file
    """
    try:
        print(f"Creating VRT for {input_file}")
        
        # Convert paths to Path objects and resolve them
        input_path = Path(input_file).resolve()
        output_path = Path(output_file).resolve()
        
        # Create output directory if it doesn't exist
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Build the gdalwarp command
        gdal_command = ["gdalwarp", "-of", "VRT"]
        
        # Add CRS if specified
        if target_crs:
            gdal_command.extend(["-t_srs", target_crs])
        
        # Add resolution if specified
        if target_resolution:
            # For 4326, convert meters to degrees (approximate at equator)
            if target_crs == "EPSG:4326" and isinstance(target_resolution, (int, float)):
                degree_resolution = target_resolution / 111320
                print(f"Target resolution: {target_resolution}m = {degree_resolution} degrees")
                gdal_command.extend(["-tr", str(degree_resolution), str(degree_resolution)])
            else:
                # Use resolution as-is if not in 4326 or if already in units compatible with target CRS
                gdal_command.extend(["-tr", str(target_resolution), str(target_resolution)])
        
        # Add resampling method
        gdal_command.extend(["-r", resampling_method])
        
        # Add input and output files
        gdal_command.extend([str(input_path), str(output_path)])
        
        # Run the command
        print(f"Running: {' '.join(gdal_command)}")
        subprocess.run(gdal_command, check=True)
        
        # Verify the VRT was created
        if not output_path.exists():
            raise FileNotFoundError(f"VRT file was not created: {output_path}")
        
        # Verify the VRT with rasterio
        with rasterio.open(str(output_path)) as vrt:
            print(f"VRT info: dims={vrt.width}x{vrt.height}, crs={vrt.crs}")
            
            # Try to read some data to verify VRT is valid
            try:
                sample_data = vrt.read(1, window=Window(0, 0, min(100, vrt.width), min(100, vrt.height)))
                print(f"VRT sample data shape: {sample_data.shape}")
            except Exception as e:
                print(f"Warning: Could not read sample data from VRT: {e}")
        
        return str(output_path)
        
    except Exception as e:
        print(f"Error creating VRT for {input_file}: {e}")
        traceback.print_exc()
        raise


def resample_raster(input_file, output_file, target_resolution=1000, 
                    resampling_method=Resampling.nearest, use_vrt=True):
    """
    Resample a single raster to a target resolution while ensuring EPSG:4326 CRS.
    Uses VRT by default for efficiency.
    
    Args:
        input_file: Path to input raster file
        output_file: Path to output raster file
        target_resolution: Target resolution in meters (default: 1000m or 1km)
        resampling_method: Resampling method (default: nearest neighbor)
        use_vrt: If True, creates a VRT instead of a GeoTIFF
        
    Returns:
        Path to resampled raster file (VRT or GeoTIFF)
    """
    try:
        print(f"Resampling raster {input_file} to {target_resolution}m resolution")
        
        # First ensure input is in EPSG:4326
        input_file = ensure_crs_4326(input_file, use_vrt=use_vrt)
        
        # If VRT is requested, create a resampled VRT
        if use_vrt:
            # Force output extension to be .vrt
            output_path = Path(output_file)
            if output_path.suffix.lower() != '.vrt':
                output_file = str(output_path.with_suffix('.vrt'))
            
            # Create VRT with target resolution
            return create_vrt_raster(
                input_file=input_file,
                output_file=output_file,
                target_resolution=target_resolution,
                resampling_method="nearest" if resampling_method == Resampling.nearest else "bilinear"
            )
        
        # If not using VRT, proceed with normal resampling
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


def resample_rasters(input_files, output_folder, target_resolution=1000, n_jobs=None, use_vrt=True):
    """
    Resample multiple rasters to a common resolution in EPSG:4326.
    Uses VRTs by default for efficiency.
    
    Args:
        input_files: List of input raster files or single file path
        output_folder: Folder to save resampled rasters
        target_resolution: Target resolution in meters (default: 1000m or 1km)
        n_jobs: Number of parallel jobs (not used in simplified version)
        use_vrt: If True, creates VRTs instead of GeoTIFFs
        
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
            
            # Determine output file extension based on use_vrt
            extension = ".vrt" if use_vrt else ".tif"
            output_file = output_folder / f"{input_file.stem}_resampled{extension}"
            
            resampled_path = resample_raster(
                input_file=str(input_file),
                output_file=str(output_file),
                target_resolution=target_resolution,
                use_vrt=use_vrt
            )
            resampled_files.append(resampled_path)
        except Exception as e:
            print(f"Could not process {input_file}: {e}")
            traceback.print_exc()
    
    if not resampled_files:
        raise ValueError("No rasters were successfully resampled")
    
    return resampled_files


def create_vrt_mosaic(input_files, output_file=None, nodata_value=None):
    """
    Create a virtual mosaic (VRT) from multiple input rasters.
    
    Args:
        input_files: List of input raster file paths
        output_file: Path to output VRT file (optional, generates temp file if None)
        nodata_value: NoData value to use in the VRT
        
    Returns:
        Path to VRT mosaic file
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
            output_file = os.path.join(temp_dir, "mosaic.vrt")
        else:
            # Ensure output has .vrt extension
            output_path = Path(output_file)
            if output_path.suffix.lower() != '.vrt':
                output_file = str(output_path.with_suffix('.vrt'))
        
        print(f"Creating VRT mosaic of {len(valid_files)} rasters to {output_file}")
        
        # Ensure output directory exists
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)
        
        # Build gdalbuildvrt command
        cmd = ["gdalbuildvrt"]
        
        # Add options
        if nodata_value is not None:
            cmd.extend(["-vrtnodata", str(nodata_value)])
        
        # Add output and input files
        cmd.append(output_file)
        cmd.extend([str(f) for f in valid_files])
        
        # Execute command
        print(f"Running: {' '.join(cmd)}")
        subprocess.run(cmd, check=True)
        
        # Verify the VRT was created
        if not os.path.exists(output_file):
            raise FileNotFoundError(f"VRT file was not created: {output_file}")
        
        # Verify the VRT with rasterio
        with rasterio.open(output_file) as vrt:
            print(f"VRT mosaic info: dims={vrt.width}x{vrt.height}, crs={vrt.crs}")
            try:
                # Try to read a small sample to verify
                sample_data = vrt.read(1, window=Window(0, 0, min(100, vrt.width), min(100, vrt.height)))
                print(f"VRT sample data shape: {sample_data.shape}")
            except Exception as e:
                print(f"Warning: Could not read sample data from VRT: {e}")
        
        return output_file
        
    except Exception as e:
        print(f"Error creating VRT mosaic: {e}")
        traceback.print_exc()
        if valid_files and len(valid_files) > 0:
            # Return the first file in case of error
            return str(valid_files[0])
        else:
            raise


def mosaic_rasters(input_files, output_file=None, use_vrt=True):
    """
    Mosaic multiple rasters into a single raster.
    Uses VRT by default for efficiency.
    
    Args:
        input_files: List of input raster file paths
        output_file: Path to output mosaic file (optional, generates temp file if None)
        use_vrt: If True, creates a VRT mosaic instead of a GeoTIFF
        
    Returns:
        Path to mosaic raster (VRT or GeoTIFF)
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
            if use_vrt:
                output_file = os.path.join(temp_dir, "mosaic.vrt")
            else:
                output_file = os.path.join(temp_dir, "mosaic.tif")
        else:
            # Adjust extension based on use_vrt
            if use_vrt:
                output_path = Path(output_file)
                if output_path.suffix.lower() != '.vrt':
                    output_file = str(output_path.with_suffix('.vrt'))
        
        # If VRT is requested, create a VRT mosaic
        if use_vrt:
            return create_vrt_mosaic(valid_files, output_file)
        
        # Otherwise, create a physical mosaic (original implementation)
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


def optimize_vrt_for_performance(vrt_file, max_datasets_open=500):
    """
    Optimize a VRT file for better performance by adjusting parameters.
    
    Args:
        vrt_file: Path to VRT file
        max_datasets_open: Maximum number of datasets to keep open
        
    Returns:
        Path to optimized VRT file
    """
    try:
        vrt_path = Path(vrt_file)
        if not vrt_path.exists() or vrt_path.suffix.lower() != '.vrt':
            print(f"File is not a VRT or doesn't exist: {vrt_file}")
            return vrt_file
        
        # Parse the VRT XML
        tree = ET.parse(vrt_file)
        root = tree.getroot()
        
        # Add a hint about the maximum number of datasets to keep open
        hint_elem = ET.Element('OpenOptions')
        max_open_elem = ET.Element('Option')
        max_open_elem.set('name', 'GDAL_MAX_DATASET_POOL_SIZE')
        max_open_elem.text = str(max_datasets_open)
        hint_elem.append(max_open_elem)
        
        # Add other optimization options if needed
        cache_elem = ET.Element('Option')
        cache_elem.set('name', 'GDAL_CACHEMAX')
        cache_elem.text = '500'  # 500MB cache
        hint_elem.append(cache_elem)
        
        # Add the hints to the root
        root.append(hint_elem)
        
        # Create a backup of the original VRT
        backup_file = f"{vrt_file}.bak"
        shutil.copy2(vrt_file, backup_file)
        
        # Write the modified VRT
        tree.write(vrt_file)
        
        print(f"Optimized VRT file: {vrt_file}")
        return vrt_file
        
    except Exception as e:
        print(f"Error optimizing VRT: {e}")
        traceback.print_exc()
        return vrt_file
