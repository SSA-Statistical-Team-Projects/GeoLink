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

    # Detect if running in a virtual machine
    is_vm <- FALSE
    vm_type <- "none"

    # Check for common VM indicators
    if (os_type == "Linux") {
      # Check for common VM indicators in Linux
      if (file.exists("/proc/cpuinfo")) {
        cpu_info <- readLines("/proc/cpuinfo")
        if (any(grepl("hypervisor", cpu_info, ignore.case = TRUE)) ||
            any(grepl("VMware", cpu_info, ignore.case = TRUE)) ||
            any(grepl("VirtualBox", cpu_info, ignore.case = TRUE)) ||
            any(grepl("QEMU", cpu_info, ignore.case = TRUE))) {
          is_vm <- TRUE

          if (any(grepl("VMware", cpu_info, ignore.case = TRUE))) {
            vm_type <- "VMware"
          } else if (any(grepl("VirtualBox", cpu_info, ignore.case = TRUE))) {
            vm_type <- "VirtualBox"
          } else if (any(grepl("QEMU", cpu_info, ignore.case = TRUE))) {
            vm_type <- "QEMU/KVM"
          } else {
            vm_type <- "Unknown"
          }
        }
      }

      # Additional check for Docker
      if (file.exists("/.dockerenv") || any(grepl("docker", readLines("/proc/1/cgroup", warn = FALSE), ignore.case = TRUE))) {
        is_vm <- TRUE
        vm_type <- "Docker"
      }
    }

    if (is_vm) {
      packageStartupMessage("Detected virtual environment: ", vm_type)
      pkg_env$is_vm <- TRUE
      pkg_env$vm_type <- vm_type
    } else {
      pkg_env$is_vm <- FALSE
    }

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

    # Install osmdata R package if not available
    if (!requireNamespace("osmdata", quietly = TRUE)) {
      packageStartupMessage("Installing osmdata package...")

      # First make sure proj4 and dependencies are installed
      if (os_type == "Linux") {
        # For Ubuntu/Debian, suggest system packages if needed
        if (file.exists("/etc/os-release")) {
          os_info <- readLines("/etc/os-release")
          if (any(grepl("Ubuntu", os_info)) || any(grepl("Debian", os_info))) {
            packageStartupMessage("Note: You may need to install system libraries with:")
            packageStartupMessage("sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev")
          }
        }
      }

      # Try to install the osmdata package
      tryCatch({
        utils::install.packages("osmdata", repos = "https://cloud.r-project.org")
      }, error = function(e) {
        packageStartupMessage("Failed to install osmdata: ", conditionMessage(e))
        packageStartupMessage("You may need to install system dependencies before installing osmdata.")
      })
    }

    # Check if our conda environment exists
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

        # Install GDAL with Windows-specific settings - libtiff 4.6.1 specifically
        packageStartupMessage("Installing geospatial packages for Windows...")
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio>=1.3.8"),
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
                  "libtiff=4.6.1",
                  "rasterio>=1.3.8",
                  "tqdm",
                  "certifi"),
                stdout = TRUE, stderr = TRUE)
      } else if (os_type == "Linux") {
        packageStartupMessage("Configuring for Linux...")
        # Check if this is Ubuntu or Debian
        is_ubuntu <- FALSE
        is_debian <- FALSE
        distro_name <- "Unknown Linux"

        if (file.exists("/etc/os-release")) {
          os_info <- readLines("/etc/os-release")
          is_ubuntu <- any(grepl("Ubuntu", os_info))
          is_debian <- any(grepl("Debian", os_info))

          # Extract distribution name for more helpful messages
          id_line <- grep("^ID=", os_info, value = TRUE)
          if (length(id_line) > 0) {
            distro_name <- gsub("^ID=|\"", "", id_line[1])
          }

          name_line <- grep("^NAME=", os_info, value = TRUE)
          if (length(name_line) > 0) {
            distro_full_name <- gsub("^NAME=|\"", "", name_line[1])
            packageStartupMessage("Detected Linux distribution: ", distro_full_name)
          }
        }

        pkg_env$linux_distro <- distro_name

        # VM-specific optimization for Linux
        if (is_vm) {
          packageStartupMessage("Applying VM-specific optimizations for ", vm_type, "...")

          # For Docker/containers, use minimal installation
          if (vm_type == "Docker") {
            system2(conda_bin,
                    c("create", "-y", "-n", geo_env_name,
                      "-c", "conda-forge",
                      "python=3.10",
                      "numpy",
                      "gdal>=3.6.0",
                      "libtiff=4.6.1",
                      "rasterio>=1.3.8",
                      "tqdm",
                      "certifi"),
                    stdout = TRUE, stderr = TRUE)
          } else {
            # Other VMs - use parallel installation to speed up the process
            system2(conda_bin,
                    c("create", "-y", "-n", geo_env_name,
                      "-c", "conda-forge",
                      "--no-deps",
                      "python=3.10"),
                    stdout = TRUE, stderr = TRUE)

            # Install dependencies in small groups to improve reliability
            system2(conda_bin,
                    c("install", "-y", "-n", geo_env_name,
                      "-c", "conda-forge",
                      "numpy", "tqdm", "certifi"),
                    stdout = TRUE, stderr = TRUE)

            # Install EXACT version of libtiff
            system2(conda_bin,
                    c("install", "-y", "-n", geo_env_name,
                      "-c", "conda-forge",
                      "libtiff=4.6.1", "proj"),
                    stdout = TRUE, stderr = TRUE)

            system2(conda_bin,
                    c("install", "-y", "-n", geo_env_name,
                      "-c", "conda-forge",
                      "gdal>=3.6.0", "rasterio>=1.3.8"),
                    stdout = TRUE, stderr = TRUE)
          }
        } else if (is_ubuntu || is_debian) {
          packageStartupMessage("Configuring for ", distro_name, "...")
          # Ubuntu/Debian-specific approach - use optimized settings for better compatibility
          system2(conda_bin,
                  c("create", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "python=3.10",
                    "numpy",
                    "gdal>=3.6.0",
                    "libtiff=4.6.1",
                    "rasterio>=1.3.8",
                    "libspatialite",
                    "tqdm",
                    "certifi"),
                  stdout = TRUE, stderr = TRUE)
        } else {
          # Generic Linux approach with more dependencies for better compatibility
          packageStartupMessage("Using generic Linux configuration...")
          system2(conda_bin,
                  c("create", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "python=3.10",
                    "numpy",
                    "gdal>=3.6.0",
                    "libtiff=4.6.1",
                    "rasterio>=1.3.8",
                    "libspatialite",
                    "sqlite",
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
                  "libtiff=4.6.1",
                  "rasterio>=1.3.8",
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
                    "libtiff=4.6.1",
                    "proj",
                    "libgdal"),
                  stdout = TRUE, stderr = TRUE)
        } else if (os_type == "Linux" && pkg_env$is_vm) {
          # VM-specific handling for Linux
          packageStartupMessage("Using VM-optimized dependency installation...")
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "--no-deps",
                    "libtiff=4.6.1"),
                  stdout = TRUE, stderr = TRUE)

          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "proj", "libgdal", "libspatialite"),
                  stdout = TRUE, stderr = TRUE)
        } else {
          # Unix-like systems (macOS and Linux)
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "libtiff=4.6.1",
                    "proj",
                    "libgdal",
                    "libspatialite"),
                  stdout = TRUE, stderr = TRUE)
        }

        # Then install the main geospatial packages
        packageStartupMessage("Installing geospatial packages to conda environment...")
        if (os_type == "Linux" && pkg_env$is_vm) {
          # For VMs, install one at a time to reduce memory pressure
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "gdal>=3.6.0"),
                  stdout = TRUE, stderr = TRUE)

          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "rasterio>=1.3.8"),
                  stdout = TRUE, stderr = TRUE)
        } else {
          # Standard installation
          system2(conda_bin,
                  c("install", "-y", "-n", geo_env_name,
                    "-c", "conda-forge",
                    "gdal>=3.6.0",
                    "rasterio>=1.3.8"),
                  stdout = TRUE, stderr = TRUE)
        }
      }
    } else {
      packageStartupMessage("Using existing conda environment: ", geo_env_name)

      # Even if environment exists, update the libtiff dependency to exact 4.6.1 version
      packageStartupMessage("Updating GDAL dependencies to compatible versions...")
      system2(conda_bin,
              c("install", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "libtiff=4.6.1"),  # Ensure proper libtiff version
              stdout = TRUE, stderr = TRUE)

      # Also update GDAL and rasterio to compatible versions
      system2(conda_bin,
              c("install", "-y", "-n", geo_env_name,
                "-c", "conda-forge",
                "gdal>=3.6.0",
                "rasterio>=1.3.8"),
              stdout = TRUE, stderr = TRUE)
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
      # Add both lib and lib64 directories for better compatibility with various Linux distros
      lib_path <- file.path(dirname(dirname(py_path)), "lib")
      lib64_path <- file.path(dirname(dirname(py_path)), "lib64")

      # Check if lib64 exists and add it if it does
      if (dir.exists(lib64_path)) {
        Sys.setenv(LD_LIBRARY_PATH = paste0(
          lib_path, ":", lib64_path, ":", Sys.getenv("LD_LIBRARY_PATH")
        ))
      } else {
        Sys.setenv(LD_LIBRARY_PATH = paste0(
          lib_path, ":", Sys.getenv("LD_LIBRARY_PATH")
        ))
      }
    }

    # If on Linux, add explicit system library path for Ubuntu/Debian
    if (os_type == "Linux") {
      if (file.exists("/etc/os-release")) {
        os_info <- readLines("/etc/os-release")
        if (any(grepl("Ubuntu", os_info)) || any(grepl("Debian", os_info))) {
          # Add system libraries to path
          Sys.setenv(LD_LIBRARY_PATH = paste0(
            "/usr/lib/x86_64-linux-gnu", ":", Sys.getenv("LD_LIBRARY_PATH")
          ))
        }
      }
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
        # Try to get more detailed version info
        gdal_info = gdal.VersionInfo('RELEASE_NAME')
        print('GDAL detailed version:', gdal_info)
    else:
        print('WARNING: GTiff driver not found')
        raise ImportError('GTiff driver not available - libtiff may be missing')
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

      if (os_type == "Linux" && pkg_env$is_vm) {
        # VM-specific repair for Linux
        packageStartupMessage("Applying VM-specific fixes...")

        # Try removing and reinstalling the packages
        system2(conda_bin,
                c("remove", "-y", "-n", geo_env_name,
                  "gdal", "rasterio", "libtiff", "--force"),
                stdout = TRUE, stderr = TRUE)

        # Install dependencies one by one
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "libtiff=4.6.1"),
                stdout = TRUE, stderr = TRUE)

        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "gdal>=3.6.0"),
                stdout = TRUE, stderr = TRUE)

        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "rasterio>=1.3.8"),
                stdout = TRUE, stderr = TRUE)
      } else if (os_type == "Windows") {
        # Windows-specific fix
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "--force-reinstall",
                  "libtiff=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio>=1.3.8"),
                stdout = TRUE, stderr = TRUE)
      } else {
        # macOS and general Linux fix
        system2(conda_bin,
                c("install", "-y", "-n", geo_env_name,
                  "-c", "conda-forge",
                  "--force-reinstall",
                  "libtiff=4.6.1",
                  "gdal>=3.6.0",
                  "rasterio>=1.3.8"),
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

    # Make sure osmdata is loaded
    if (requireNamespace("osmdata", quietly = TRUE)) {
      packageStartupMessage("osmdata package is available")
    } else {
      packageStartupMessage("WARNING: osmdata package is not available, some functions may not work")
      packageStartupMessage("Try installing manually with: install.packages('osmdata')")

      # For Ubuntu/Debian, suggest system packages
      if (os_type == "Linux" && file.exists("/etc/os-release")) {
        os_info <- readLines("/etc/os-release")
        if (any(grepl("Ubuntu", os_info)) || any(grepl("Debian", os_info))) {
          packageStartupMessage("You may need to run the following first:")
          packageStartupMessage("sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev")
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

    if (os_type == "Linux") {
      packageStartupMessage("Linux distribution: ", pkg_env$linux_distro)
      if (pkg_env$is_vm) {
        packageStartupMessage("Running in virtual environment: ", pkg_env$vm_type)
      }
    }
  }, error = function(e) {
    warning(paste(
      "Python environment setup failed:",
      conditionMessage(e),
      "Details:",
      conditionMessage(e)
    ))
  })
}
