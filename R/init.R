.onLoad <- function(libname, pkgname) {
  # Close any open sink connections
  while (sink.number() > 0) {
    sink()
  }

  tryCatch({
    # Create package environment to store configuration
    pkg_env <- new.env(parent = emptyenv())

    # Specify the environment name for our geospatial distribution
    geo_env_name <- "geolink_env"
    pkg_env$conda_env_name <- geo_env_name

    # Check if conda is available
    conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)

    if (is.null(conda_bin) || !file.exists(conda_bin)) {
      packageStartupMessage("Conda not found. Installing Miniconda...")
      reticulate::install_miniconda()
      conda_bin <- reticulate::conda_binary()

      if (!file.exists(conda_bin)) {
        stop("Miniconda installation failed.")
      }
    }

    packageStartupMessage("Using Conda at: ", conda_bin)

    # Check if our environment exists
    envs <- reticulate::conda_list()
    env_exists <- geo_env_name %in% envs$name

    if (!env_exists) {
      packageStartupMessage("Creating new conda environment for geospatial processing: ", geo_env_name)

      # Create environment with geospatial packages from conda-forge
      # Include specific versions of dependencies needed for GDAL/rasterio
      system2(conda_bin,
              c("create", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "python=3.10",
                "numpy",
                "gdal>=3.6.0",
                "libtiff>=4.6.1",  # Specifically include proper libtiff version
                "rasterio",
                "tqdm",
                "certifi"),
              stdout = TRUE, stderr = TRUE)

      # Verify environment was created
      envs <- reticulate::conda_list()
      env_exists <- geo_env_name %in% envs$name

      if (!env_exists) {
        # If creation failed, try a simplified approach
        packageStartupMessage("Standard creation failed. Trying simplified approach...")
        reticulate::conda_create(geo_env_name)

        # Install base packages
        packages <- c("numpy", "tqdm", "certifi")
        for (pkg in packages) {
          packageStartupMessage("Installing ", pkg, " to conda environment...")
          reticulate::conda_install(geo_env_name, pkg, channel = "conda-forge")
        }

        # Install proper dependencies first
        packageStartupMessage("Installing critical GDAL dependencies...")
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff>=4.6.1",  # Specific version required
                  "proj",
                  "libgdal",
                  "libspatialite"),
                stdout = TRUE, stderr = TRUE)

        # Then install the main geospatial packages
        packageStartupMessage("Installing geospatial packages to conda environment...")
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "gdal>=3.6.0",
                  "rasterio"),
                stdout = TRUE, stderr = TRUE)
      }
    } else {
      packageStartupMessage("Using existing conda environment: ", geo_env_name)

      # Even if environment exists, check for and update the libtiff dependency
      packageStartupMessage("Verifying GDAL dependencies are up to date...")
      system2(conda_bin,
              c("install", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "libtiff>=4.6.1"),  # Ensure proper libtiff version
              stdout = TRUE, stderr = TRUE)
    }

    # Get python path from this environment
    py_path <- reticulate::conda_python(geo_env_name)
    pkg_env$python_path <- py_path

    # Use this conda environment
    reticulate::use_condaenv(geo_env_name, required = TRUE)

    # Set environment variable to ensure conda libs are used instead of system libs
    Sys.setenv(LD_LIBRARY_PATH = paste0(
      file.path(dirname(dirname(py_path)), "lib"),
      ":",
      Sys.getenv("LD_LIBRARY_PATH")
    ))

    # Verify the environment works and has required packages
    # Also helps to "warm up" the environment
    success <- tryCatch({
      reticulate::py_run_string("
import sys
print('Python executable:', sys.executable)
print('Python version:', sys.version)

# Check key packages
import numpy
print('NumPy version:', numpy.__version__)

try:
    import rasterio
    print('Rasterio version:', rasterio.__version__)
except ImportError as e:
    print('Error importing Rasterio:', e)
    # This will be raised to R
    raise ImportError('Rasterio package not available')

try:
    import gdal
    print('GDAL version:', gdal.VersionInfo())
except ImportError:
    try:
        from osgeo import gdal
        print('GDAL version (from osgeo):', gdal.VersionInfo())
    except ImportError as e:
        print('Error importing GDAL:', e)
        # This will be raised to R
        raise ImportError('GDAL package not available')

# Explicitly check libtiff version through GDAL
try:
    from osgeo import gdal
    drivers = [gdal.GetDriver(i).ShortName for i in range(gdal.GetDriverCount())]
    if 'GTiff' in drivers:
        print('GTiff driver is available')
    else:
        print('WARNING: GTiff driver not found')
except Exception as e:
    print('Error checking GDAL drivers:', e)
")
      TRUE
    }, error = function(e) {
      packageStartupMessage("Environment verification failed: ", conditionMessage(e))
      # Don't stop here, we'll continue and try to fix
      FALSE
    })

    if (!success) {
      # Try to fix the environment
      packageStartupMessage("Attempting to fix package installation...")

      # Reinstall critical dependencies with specific versions
      packageStartupMessage("Reinstalling GDAL dependencies with specific versions...")
      system2(conda_bin,
              c("install", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "--force-reinstall",
                "libtiff>=4.6.1",
                "gdal>=3.6.0",
                "rasterio"),
              stdout = TRUE, stderr = TRUE)

      # Use pip as a last resort
      pip_path <- file.path(dirname(py_path), "pip")
      if (!file.exists(pip_path)) {
        pip_path <- file.path(dirname(py_path), "pip.exe")
      }

      if (file.exists(pip_path)) {
        packageStartupMessage("Using pip at: ", pip_path)
        system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
        # In some cases, pip can resolve dependency issues better than conda
        # But install minimal packages to avoid conflicts
        system2(pip_path, c("install", "--upgrade", "rasterio"), stdout = TRUE)
      }
    }

    # Store environment in package namespace
    assign("pkg_env", pkg_env, envir = parent.env(environment()))

    packageStartupMessage("Python environment setup completed successfully")
    packageStartupMessage("Using Python at: ", pkg_env$python_path)
  }, error = function(e) {
    warning(paste(
      "Python environment setup failed:",
      conditionMessage(e),
      "Details:",
      conditionMessage(e)
    ))
  })
}
