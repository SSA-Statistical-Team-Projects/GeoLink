.onLoad <- function(libname, pkgname) {
  # Check if there are open sink connections before closing them
  if (sink.number() > 0) {
    # Close any open sink connections
    while (sink.number() > 0) {
      sink()
    }
  }

  tryCatch({
    # Create package environment to store configuration
    pkg_env <- new.env(parent = emptyenv())

    # Specify the environment name for our geospatial distribution
    geo_env_name <- "geolink_env"
    pkg_env$conda_env_name <- geo_env_name

    # Detect operating system
    os_type <- Sys.info()["sysname"]
    pkg_env$os_type <- os_type
    packageStartupMessage("Detected operating system: ", os_type)

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

      # OS-specific package customizations
      if (os_type == "Windows") {
        packageStartupMessage("Configuring for Windows...")
        # Windows-specific approach
        reticulate::conda_create(geo_env_name)

        # Install base packages
        packageStartupMessage("Installing base packages...")
        reticulate::conda_install(geo_env_name,
                                  c("python=3.10", "numpy", "tqdm", "certifi"),
                                  channel = "conda-forge")

        # Install GDAL with Windows-specific settings
        packageStartupMessage("Installing geospatial packages for Windows...")
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff>=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio"),
                stdout = TRUE, stderr = TRUE)
      } else if (os_type == "Darwin") {
        packageStartupMessage("Configuring for macOS...")
        # macOS-specific approach
        system2(conda_bin,
                c("create", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "python=3.10",
                  "numpy",
                  "gdal>=3.6.0",
                  "libtiff>=4.6.1",
                  "rasterio",
                  "tqdm",
                  "certifi"),
                stdout = TRUE, stderr = TRUE)
      } else if (os_type == "Linux") {
        packageStartupMessage("Configuring for Linux...")
        # Check if this is Ubuntu
        is_ubuntu <- FALSE
        if (file.exists("/etc/os-release")) {
          os_info <- readLines("/etc/os-release")
          is_ubuntu <- any(grepl("Ubuntu", os_info))
        }

        if (is_ubuntu) {
          packageStartupMessage("Detected Ubuntu Linux...")
          # Ubuntu-specific approach - MODIFIED to use libtiff6
          system2(conda_bin,
                  c("create", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "python=3.10",
                    "numpy",
                    "gdal>=3.6.0",
                    "libtiff6",
                    "rasterio",
                    "tqdm",
                    "certifi"),
                  stdout = TRUE, stderr = TRUE)
        } else {
          # Generic Linux approach - MODIFIED to use libtiff6
          packageStartupMessage("Using generic Linux configuration...")
          system2(conda_bin,
                  c("create", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "python=3.10",
                    "numpy",
                    "gdal>=3.6.0",
                    "libtiff6",
                    "rasterio",
                    "tqdm",
                    "certifi"),
                  stdout = TRUE, stderr = TRUE)
        }
      } else {
        # Generic approach for other OS
        packageStartupMessage("Using generic configuration for ", os_type, "...")
        system2(conda_bin,
                c("create", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "python=3.10",
                  "numpy",
                  "gdal>=3.6.0",
                  "libtiff>=4.6.1",
                  "rasterio",
                  "tqdm",
                  "certifi"),
                stdout = TRUE, stderr = TRUE)
      }

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

        # Install proper dependencies first - with OS-specific tweaks
        packageStartupMessage("Installing critical GDAL dependencies...")
        if (os_type == "Windows") {
          # Windows often needs special handling
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "libtiff>=4.6.1",
                    "proj",
                    "libgdal"),
                  stdout = TRUE, stderr = TRUE)
        } else if (os_type == "Linux") {
          # Linux systems - MODIFIED to use libtiff6
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "libtiff6",
                    "proj",
                    "libgdal",
                    "libspatialite"),
                  stdout = TRUE, stderr = TRUE)
        } else {
          # Unix-like systems (macOS)
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "libtiff>=4.6.1",
                    "proj",
                    "libgdal",
                    "libspatialite"),
                  stdout = TRUE, stderr = TRUE)
        }

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

      # OS-specific libtiff update
      if (os_type == "Linux") {
        # For Linux systems - MODIFIED to use libtiff6
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff6"),  # Use specific libtiff6 version
                stdout = TRUE, stderr = TRUE)
      } else {
        # For Windows and macOS
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff>=4.6.1"),  # Ensure proper libtiff version
                stdout = TRUE, stderr = TRUE)
      }
    }

    # Get python path from this environment
    py_path <- reticulate::conda_python(geo_env_name)
    pkg_env$python_path <- py_path

    # Use this conda environment
    reticulate::use_condaenv(geo_env_name, required = TRUE)

    # Set environment variable to ensure conda libs are used instead of system libs
    # This is implemented differently per OS
    if (os_type == "Windows") {
      # Windows uses PATH instead of LD_LIBRARY_PATH
      Sys.setenv(PATH = paste0(
        file.path(dirname(dirname(py_path)), "Library", "bin"),
        ";",
        Sys.getenv("PATH")
      ))
    } else if (os_type == "Darwin") {
      # macOS library path
      Sys.setenv(DYLD_LIBRARY_PATH = paste0(
        file.path(dirname(dirname(py_path)), "lib"),
        ":",
        Sys.getenv("DYLD_LIBRARY_PATH")
      ))
    } else {
      # Linux and others use LD_LIBRARY_PATH
      Sys.setenv(LD_LIBRARY_PATH = paste0(
        file.path(dirname(dirname(py_path)), "lib"),
        ":",
        Sys.getenv("LD_LIBRARY_PATH")
      ))
    }

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
      # Try to fix the environment - with OS-specific fixes
      packageStartupMessage("Attempting to fix package installation...")

      # Reinstall critical dependencies with specific versions
      packageStartupMessage("Reinstalling GDAL dependencies with specific versions...")

      if (os_type == "Windows") {
        # Windows-specific fix
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "--force-reinstall",
                  "libtiff>=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio"),
                stdout = TRUE, stderr = TRUE)
      } else if (os_type == "Linux") {
        # Linux-specific fix - MODIFIED to use libtiff6
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "--force-reinstall",
                  "libtiff6",
                  "gdal>=3.6.0",
                  "rasterio"),
                stdout = TRUE, stderr = TRUE)
      } else {
        # macOS fix
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "--force-reinstall",
                  "libtiff>=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio"),
                stdout = TRUE, stderr = TRUE)
      }

      # Use pip as a last resort
      pip_path <- file.path(dirname(py_path), "pip")
      if (os_type == "Windows") {
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
    # Make sure it's available in the package namespace regardless of OS
    pkg_namespace <- asNamespace(pkgname)
    assign("pkg_env", pkg_env, envir = pkg_namespace)

    packageStartupMessage("Python environment setup completed successfully")
    packageStartupMessage("Using Python at: ", pkg_env$python_path)
    packageStartupMessage("Operating system: ", pkg_env$os_type)
  }, error = function(e) {
    warning(paste(
      "Python environment setup failed:",
      conditionMessage(e),
      "Details:",
      conditionMessage(e)
    ))
  })
}
