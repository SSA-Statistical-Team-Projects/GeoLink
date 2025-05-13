.onLoad <- function(libname, pkgname) {
  # Better sink management - fixed for Ubuntu
  tryCatch({
    # Check if sink.number() is available and if there are any sinks to close
    if (exists("sink.number") && is.function(sink.number) && sink.number() > 0) {
      # Use a safer approach for closing sinks
      for (i in 1:sink.number()) {
        # Suppress warnings completely when closing sinks
        suppressWarnings(sink(file = NULL, type = "output"))
      }
    }
  }, error = function(e) {
    # If anything goes wrong, just log it and continue
    packageStartupMessage("Note: Could not properly close sink connections: ", conditionMessage(e))
  })

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
        output <- system2(conda_bin,
                          c("install", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            shQuote("libtiff>=4.6.1"),
                            shQuote("gdal>=3.6.0"),
                            "rasterio"),
                          stdout = TRUE, stderr = TRUE)

        if (length(output) > 0) {
          packageStartupMessage("Installation output: ", paste(output[1:min(5, length(output))], collapse="\n"))
        }
      } else if (os_type == "Darwin") {
        packageStartupMessage("Configuring for macOS...")
        # macOS-specific approach
        output <- system2(conda_bin,
                          c("create", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            "python=3.10",
                            "numpy",
                            shQuote("gdal>=3.6.0"),
                            shQuote("libtiff>=4.6.1"),
                            "rasterio",
                            "tqdm",
                            "certifi"),
                          stdout = TRUE, stderr = TRUE)

        if (length(output) > 0) {
          packageStartupMessage("Installation output: ", paste(output[1:min(5, length(output))], collapse="\n"))
        }
      } else if (os_type == "Linux") {
        packageStartupMessage("Configuring for Linux...")
        # Check if this is Ubuntu
        is_ubuntu <- FALSE
        if (file.exists("/etc/os-release")) {
          os_info <- readLines("/etc/os-release")
          is_ubuntu <- any(grepl("Ubuntu", os_info))
        }

        # Create base environment first for Linux/Ubuntu
        packageStartupMessage("Creating base Python environment...")
        output <- system2(conda_bin,
                          c("create", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            "python=3.10",
                            "numpy",
                            "tqdm",
                            "certifi",
                            "pip"),
                          stdout = TRUE, stderr = TRUE)

        if (length(output) > 0) {
          packageStartupMessage("Environment creation output: ", paste(output[1:min(5, length(output))], collapse="\n"))
        }

        # Get python and pip paths
        py_path <- reticulate::conda_python(geo_env_name)
        pkg_env$python_path <- py_path

        pip_path <- file.path(dirname(py_path), "pip")
        if (!file.exists(pip_path)) {
          pip_path <- file.path(dirname(py_path), "pip3")
        }

        if (file.exists(pip_path)) {
          packageStartupMessage("Installing dependencies via pip for Linux...")

          # Install required system libraries if possible
          if (is_ubuntu) {
            packageStartupMessage("Detected Ubuntu Linux, installing system dependencies...")
            # We can't guarantee system installs will work in R package, so we focus on pip
          }

          # Install base dependencies through pip
          packageStartupMessage("Installing base dependencies with pip...")
          output <- system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Pip upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          # Install GDAL and rasterio through pip
          packageStartupMessage("Installing GDAL and libtiff6 dependencies...")
          output <- system2(pip_path, c("install", "wheel", "setuptools"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Wheel/setuptools install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          # First install critical dependencies for GDAL and rasterio
          packageStartupMessage("Installing critical dependencies for geospatial libraries...")
          output <- system2(pip_path, c("install", "cython"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Cython install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          # Then install GDAL
          packageStartupMessage("Installing GDAL...")
          output <- system2(pip_path, c("install", "--no-binary=gdal", shQuote("GDAL>=3.6.0")), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("GDAL install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          # Now install rasterio
          packageStartupMessage("Installing rasterio...")
          output <- system2(pip_path, c("install", "--no-binary=rasterio", "rasterio"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Rasterio install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          # Install additional required packages
          packageStartupMessage("Installing additional required packages...")
          output <- system2(pip_path, c("install", "pyproj", "fiona", "shapely"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Additional packages install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        } else {
          packageStartupMessage("Pip not found, falling back to conda for Linux...")
          # Fall back to conda if pip is not available
          if (is_ubuntu) {
            packageStartupMessage("Using conda for Ubuntu Linux...")
            output <- system2(conda_bin,
                              c("install", "-y", "-n", geo_env_name,
                                "-c", "conda-forge",
                                shQuote("gdal>=3.6.0"),
                                "libtiff6",
                                "rasterio"),
                              stdout = TRUE, stderr = TRUE)
            if (length(output) > 0) {
              packageStartupMessage("Conda install output: ", paste(output[1:min(5, length(output))], collapse="\n"))
            }
          } else {
            packageStartupMessage("Using conda for generic Linux...")
            output <- system2(conda_bin,
                              c("install", "-y", "-n", geo_env_name,
                                "-c", "conda-forge",
                                shQuote("gdal>=3.6.0"),
                                "libtiff6",
                                "rasterio"),
                              stdout = TRUE, stderr = TRUE)
            if (length(output) > 0) {
              packageStartupMessage("Conda install output: ", paste(output[1:min(5, length(output))], collapse="\n"))
            }
          }
        }
      } else {
        # Generic approach for other OS
        packageStartupMessage("Using generic configuration for ", os_type, "...")
        output <- system2(conda_bin,
                          c("create", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            "python=3.10",
                            "numpy",
                            shQuote("gdal>=3.6.0"),
                            shQuote("libtiff>=4.6.1"),
                            "rasterio",
                            "tqdm",
                            "certifi"),
                          stdout = TRUE, stderr = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("Conda install output: ", paste(output[1:min(5, length(output))], collapse="\n"))
        }
      }

      # Verify environment was created
      envs <- reticulate::conda_list()
      env_exists <- geo_env_name %in% envs$name

      if (!env_exists) {
        # If creation failed, try a simplified approach
        packageStartupMessage("Standard creation failed. Trying simplified approach...")
        reticulate::conda_create(geo_env_name)

        # Install base packages
        packages <- c("numpy", "tqdm", "certifi", "pip")
        for (pkg in packages) {
          packageStartupMessage("Installing ", pkg, " to conda environment...")
          reticulate::conda_install(geo_env_name, pkg, channel = "conda-forge")
        }

        # Get python and pip paths
        py_path <- reticulate::conda_python(geo_env_name)
        pip_path <- file.path(dirname(py_path), "pip")
        if (os_type == "Windows") {
          pip_path <- file.path(dirname(py_path), "pip.exe")
        }

        # For Linux and Ubuntu, try to use pip
        if (os_type == "Linux" && file.exists(pip_path)) {
          packageStartupMessage("Trying fallback pip installation for Linux...")
          output <- system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Pip upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "wheel", "setuptools", "cython"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Dependencies install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", shQuote("GDAL>=3.6.0")), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("GDAL install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "rasterio"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Rasterio install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        } else {
          # Install proper dependencies first - with OS-specific tweaks
          packageStartupMessage("Installing critical GDAL dependencies...")
          if (os_type == "Windows") {
            # Windows often needs special handling
            output <- system2(conda_bin,
                              c("install", "-y", "-n", geo_env_name,
                                "-c", "conda-forge",
                                shQuote("libtiff>=4.6.1"),
                                "proj",
                                "libgdal"),
                              stdout = TRUE, stderr = TRUE)
            if (length(output) > 0) {
              packageStartupMessage("Windows dependencies output: ", paste(output[1:min(3, length(output))], collapse="\n"))
            }
          } else if (os_type == "Linux") {
            # Linux systems - MODIFIED to use libtiff6
            output <- system2(conda_bin,
                              c("install", "-y", "-n", geo_env_name,
                                "-c", "conda-forge",
                                "libtiff6",
                                "proj",
                                "libgdal",
                                "libspatialite"),
                              stdout = TRUE, stderr = TRUE)
            if (length(output) > 0) {
              packageStartupMessage("Linux dependencies output: ", paste(output[1:min(3, length(output))], collapse="\n"))
            }
          } else {
            # Unix-like systems (macOS)
            output <- system2(conda_bin,
                              c("install", "-y", "-n", geo_env_name,
                                "-c", "conda-forge",
                                shQuote("libtiff>=4.6.1"),
                                "proj",
                                "libgdal",
                                "libspatialite"),
                              stdout = TRUE, stderr = TRUE)
            if (length(output) > 0) {
              packageStartupMessage("macOS dependencies output: ", paste(output[1:min(3, length(output))], collapse="\n"))
            }
          }

          # Then install the main geospatial packages
          packageStartupMessage("Installing geospatial packages to conda environment...")
          output <- system2(conda_bin,
                            c("install", "-y", "-n", geo_env_name,
                              "-c", "conda-forge",
                              shQuote("gdal>=3.6.0"),
                              "rasterio"),
                            stdout = TRUE, stderr = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Geospatial packages output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        }
      }
    } else {
      packageStartupMessage("Using existing conda environment: ", geo_env_name)

      # Even if environment exists, check for and update the dependencies
      packageStartupMessage("Verifying GDAL dependencies are up to date...")

      # Get python and pip paths
      py_path <- reticulate::conda_python(geo_env_name)
      pkg_env$python_path <- py_path

      # OS-specific dependency update
      if (os_type == "Linux") {
        # For Linux systems, attempt pip update if available
        pip_path <- file.path(dirname(py_path), "pip")
        if (!file.exists(pip_path)) {
          pip_path <- file.path(dirname(py_path), "pip3")
        }

        if (file.exists(pip_path)) {
          packageStartupMessage("Updating dependencies with pip for Linux...")
          output <- system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Pip upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "--upgrade", shQuote("GDAL>=3.6.0")), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("GDAL upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "--upgrade", "rasterio"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Rasterio upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        } else {
          # Fall back to conda
          packageStartupMessage("Pip not found, using conda for updates...")
          output <- system2(conda_bin,
                            c("install", "-y", "-n", geo_env_name,
                              "-c", "conda-forge",
                              "libtiff6"),  # Use specific libtiff6 version
                            stdout = TRUE, stderr = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Conda update output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        }
      } else {
        # For Windows and macOS, use conda
        output <- system2(conda_bin,
                          c("install", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            shQuote("libtiff>=4.6.1")),  # Ensure proper libtiff version
                          stdout = TRUE, stderr = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("Libtiff update output: ", paste(output[1:min(3, length(output))], collapse="\n"))
        }
      }
    }

    # Get python path from this environment (even if already set above)
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

      # Get current python path
      py_path <- pkg_env$python_path

      if (os_type == "Linux") {
        # For Linux try pip first
        pip_path <- file.path(dirname(py_path), "pip")
        if (os_type == "Windows") {
          pip_path <- file.path(dirname(py_path), "pip.exe")
        }

        if (file.exists(pip_path)) {
          packageStartupMessage("Using pip for Linux repair...")
          output <- system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Pip upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "--upgrade", "--force-reinstall", shQuote("GDAL>=3.6.0")), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("GDAL reinstall output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }

          output <- system2(pip_path, c("install", "--upgrade", "--force-reinstall", "rasterio"), stdout = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Rasterio reinstall output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        } else {
          # Fall back to conda for Linux
          packageStartupMessage("Pip not found, using conda for repair...")
          output <- system2(conda_bin,
                            c("install", "-y", "-n", geo_env_name,
                              "-c", "conda-forge",
                              "--force-reinstall",
                              "libtiff6",
                              shQuote("gdal>=3.6.0"),
                              "rasterio"),
                            stdout = TRUE, stderr = TRUE)
          if (length(output) > 0) {
            packageStartupMessage("Conda repair output: ", paste(output[1:min(3, length(output))], collapse="\n"))
          }
        }
      } else if (os_type == "Windows") {
        # Windows-specific fix
        output <- system2(conda_bin,
                          c("install", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            "--force-reinstall",
                            shQuote("libtiff>=4.6.1"),
                            shQuote("gdal>=3.6.0"),
                            "rasterio"),
                          stdout = TRUE, stderr = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("Windows repair output: ", paste(output[1:min(3, length(output))], collapse="\n"))
        }
      } else {
        # macOS fix
        output <- system2(conda_bin,
                          c("install", "-y", "-n", geo_env_name,
                            "-c", "conda-forge",
                            "--force-reinstall",
                            shQuote("libtiff>=4.6.1"),
                            shQuote("gdal>=3.6.0"),
                            "rasterio"),
                          stdout = TRUE, stderr = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("macOS repair output: ", paste(output[1:min(3, length(output))], collapse="\n"))
        }
      }

      # Use pip as a last resort for all platforms
      pip_path <- file.path(dirname(py_path), "pip")
      if (os_type == "Windows") {
        pip_path <- file.path(dirname(py_path), "pip.exe")
      }

      if (file.exists(pip_path)) {
        packageStartupMessage("Using pip as last resort at: ", pip_path)
        output <- system2(pip_path, c("install", "--upgrade", "pip"), stdout = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("Pip upgrade output: ", paste(output[1:min(3, length(output))], collapse="\n"))
        }

        # In some cases, pip can resolve dependency issues better than conda
        # But install minimal packages to avoid conflicts
        output <- system2(pip_path, c("install", "--upgrade", "rasterio"), stdout = TRUE)
        if (length(output) > 0) {
          packageStartupMessage("Rasterio install output: ", paste(output[1:min(3, length(output))], collapse="\n"))
        }
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
