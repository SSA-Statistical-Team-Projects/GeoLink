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
      message("Conda not found. Installing Miniconda...")
      reticulate::install_miniconda()
      conda_bin <- reticulate::conda_binary()

      if (!file.exists(conda_bin)) {
        stop("Miniconda installation failed.")
      }
    }

    message("Using Conda at: ", conda_bin)

    # Check if our environment exists
    envs <- reticulate::conda_list()
    env_exists <- geo_env_name %in% envs$name

    if (!env_exists) {
      message("Creating new conda environment for geospatial processing: ", geo_env_name)

      # Create environment with geospatial packages from conda-forge
      system2(conda_bin,
              c("create", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "python=3.10", "numpy", "rasterio", "gdal", "tqdm", "certifi"),
              stdout = TRUE, stderr = TRUE)

      # Verify environment was created
      envs <- reticulate::conda_list()
      env_exists <- geo_env_name %in% envs$name

      if (!env_exists) {
        # If creation failed, try a simplified approach
        message("Standard creation failed. Trying simplified approach...")
        reticulate::conda_create(geo_env_name)

        # Install packages individually
        packages <- c("numpy", "tqdm", "certifi")
        for (pkg in packages) {
          message("Installing ", pkg, " to conda environment...")
          reticulate::conda_install(geo_env_name, pkg, channel = "conda-forge")
        }

        # Install rasterio and gdal separately (they're more complex)
        message("Installing geospatial packages to conda environment...")
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name, "-c", "conda-forge", "rasterio", "gdal"),
                stdout = TRUE, stderr = TRUE)
      }
    } else {
      message("Using existing conda environment: ", geo_env_name)
    }

    # Get python path from this environment
    py_path <- reticulate::conda_python(geo_env_name)
    pkg_env$python_path <- py_path

    # Use this conda environment
    reticulate::use_condaenv(geo_env_name, required = TRUE)

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
")
      TRUE
    }, error = function(e) {
      message("Environment verification failed: ", conditionMessage(e))
      # Don't stop here, we'll continue and try to fix
      FALSE
    })

    if (!success) {
      # Try to fix the environment
      message("Attempting to fix package installation...")

      # Use pip directly for some packages that might be problematic with conda
      pip_path <- file.path(dirname(py_path), "pip")
      if (!file.exists(pip_path)) {
        pip_path <- file.path(dirname(py_path), "pip.exe")
      }

      if (file.exists(pip_path)) {
        message("Using pip at: ", pip_path)
        system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
        system2(pip_path, c("install", "rasterio", "numpy"), stdout = TRUE)
      }
    }

    # Store environment in package namespace
    assign("pkg_env", pkg_env, envir = parent.env(environment()))

    message("Python environment setup completed successfully")
    message("Using Python at: ", pkg_env$python_path)
  }, error = function(e) {
    warning(paste(
      "Python environment setup failed:",
      conditionMessage(e),
      "Details:",
      conditionMessage(e)
    ))
  })
}
