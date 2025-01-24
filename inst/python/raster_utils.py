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
    if isinstance(input_files, str):
        input_files = [input_files]
    input_files = [f for f in input_files if f and os.path.exists(f)]
    
    if not input_files:
        raise FileNotFoundError("No valid input files provided.")

    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    resampled_files = []

    # Force EPSG:4326 as target CRS
    target_crs = 'EPSG:4326'

    for input_file in tqdm(input_files, desc='Processing files'):
        if not os.path.exists(input_file):
            continue

        base_filename = os.path.basename(input_file)
        filename_without_ext, ext = os.path.splitext(base_filename)
        output_file = os.path.join(output_folder, f'{filename_without_ext}_1km{ext}')

        try:
            if os.path.exists(output_file):
                os.remove(output_file)
        except:
            continue

        try:
            with rasterio.open(input_file) as src:
                transform, width, height = calculate_default_transform(
                    src.crs, target_crs,
                    src.width, src.height,
                    *src.bounds,
                    resolution=(target_resolution, target_resolution)
                )

                kwargs = src.meta.copy()
                kwargs.update({
                    'driver': 'GTiff',
                    'height': height,
                    'width': width,
                    'transform': transform,
                    'crs': target_crs,
                    'compress': 'lzw'
                })

                with rasterio.open(output_file, 'w', **kwargs) as dst:
                    for i in range(1, src.count + 1):
                        reproject(
                            source=rasterio.band(src, i),
                            destination=rasterio.band(dst, i),
                            src_transform=src.transform,
                            src_crs=src.crs,
                            dst_transform=transform,
                            dst_crs=target_crs,
                            resampling=Resampling.average
                        )

                resampled_files.append(output_file)
                print(f'Resampled raster saved to {output_file}')

        except Exception as e:
            print(f"Error processing {input_file}: {str(e)}")
            if os.path.exists(output_file):
                try:
                    os.remove(output_file)
                except:
                    pass
            continue

    if not resampled_files:
        raise FileNotFoundError("No resampled files were created.")
    
    return resampled_files

def mosaic_rasters(input_files):
    rasters = []
    
    try:
        # Force EPSG:4326 as target CRS
        target_crs = 'EPSG:4326'
        
        for file in input_files:
            try:
                with rasterio.open(file) as src:
                    if src.crs.to_string() != target_crs:
                        print(f"Reprojecting {file} to {target_crs}")
                        with NamedTemporaryFile(delete=False, suffix='.tif') as tmp:
                            temp_path = tmp.name
                        
                        transform, width, height = calculate_default_transform(
                            src.crs, target_crs,
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
                        
                        with rasterio.open(temp_path, 'w', **kwargs) as dst:
                            reproject(
                                source=rasterio.band(src, 1),
                                destination=rasterio.band(dst, 1),
                                src_transform=src.transform,
                                src_crs=src.crs,
                                dst_transform=transform,
                                dst_crs=target_crs
                            )
                        rasters.append(rasterio.open(temp_path))
                    else:
                        rasters.append(rasterio.open(file))
            except Exception as e:
                print(f"Error processing {file}: {str(e)}")
                continue

        if not rasters:
            raise ValueError("No valid rasters to mosaic")

        mosaic, out_transform = merge(rasters)
        out_meta = rasters[0].meta.copy()
        out_meta.update({
            'driver': 'GTiff',
            'height': mosaic.shape[1],
            'width': mosaic.shape[2],
            'transform': out_transform,
            'crs': target_crs,
            'compress': 'lzw'
        })

        with NamedTemporaryFile(delete=False, suffix='.tif') as tmpfile:
            output_path = tmpfile.name

        try:
            with rasterio.open(output_path, 'w', **out_meta) as dest:
                dest.write(mosaic)
        except Exception as e:
            print(f"Error writing mosaic: {str(e)}")
            if os.path.exists(output_path):
                try:
                    os.remove(output_path)
                except:
                    pass
            raise

    finally:
        for raster in rasters:
            try:
                raster.close()
                if hasattr(raster, 'name') and os.path.exists(raster.name):
                    try:
                        os.remove(raster.name)
                    except:
                        pass
            except:
                pass

    return output_path
