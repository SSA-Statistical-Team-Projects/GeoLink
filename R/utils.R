.onLoad <- function(libname, pkgname) {
  # Create package environment to store configuration
  pkg_env <- new.env(parent = emptyenv())

  # Store environment in package namespace immediately
  pkg_namespace <- asNamespace(pkgname)
  assign("pkg_env", pkg_env, envir = pkg_namespace)

  # Close any open sink connections
  if (sink.number() > 0) {
    while (sink.number() > 0) {
      sink()
    }
  }

  tryCatch({
    # Load configuration
    config <- read_config()
    pkg_env$config <- config

    # Get OS info from config
    os_type <- config$runtime$os_type
    is_ubuntu <- config$runtime$is_ubuntu
    pkg_env$os_type <- os_type

    packageStartupMessage("Detected operating system: ", os_type)

    # Special handling for Linux/Ubuntu systems
    if (os_type == "Linux" && is_ubuntu && config$os$linux$ubuntu$enabled) {
      packageStartupMessage("Ubuntu/Debian system detected. Checking for GDAL/Rasterio...")

      # Check if GDAL and Rasterio are already installed
      gdal_installed <- suppressWarnings(
        system("python3 -c \"from osgeo import gdal\" 2>/dev/null", ignore.stdout = TRUE) == 0
      )

      rasterio_installed <- suppressWarnings(
        system("python3 -c \"import rasterio\" 2>/dev/null", ignore.stdout = TRUE) == 0
      )

      if (!gdal_installed || !rasterio_installed) {
        # Packages not installed, prompt user to install if configured to do so
        if (config$installation$prompt_user) {
          cat("\n")
          cat("GeoLink requires GDAL and Rasterio Python packages to function properly.\n")
          cat("These packages need to be installed with sudo privileges.\n")
          cat("\n")
          cat("Would you like to install the required packages now? (y/n): ")
          install_choice <- tolower(readline())

          if (startsWith(install_choice, "y")) {
            # Create installation script from configuration
            install_script <- create_installation_script(config)

            # Find appropriate terminal executor
            terminal_exec <- get_terminal_executor(config)

            if (!is.null(terminal_exec)) {
              # Open a terminal to run the installation script
              if (terminal_exec == "x-terminal-emulator") {
                system(paste("x-terminal-emulator -e", shQuote(install_script)), wait = FALSE)
              } else if (terminal_exec == "gnome-terminal") {
                system(paste("gnome-terminal -- ", shQuote(install_script)), wait = FALSE)
              } else if (terminal_exec == "xterm") {
                system(paste("xterm -e", shQuote(install_script)), wait = FALSE)
              }

              # Wait for installation to complete
              cat("\n")
              cat("Please complete the installation in the terminal window that opened.\n")
              cat("Press Enter here when the installation is complete: ")
              readline()
            } else {
              # If no graphical terminal is available, use the current terminal
              cat("No graphical terminal found. Running installation in the current terminal.\n")
              cat("You will need to provide your sudo password when prompted.\n")
              system(install_script)
            }

            # Verify installation was successful
            if (config$installation$verify_imports) {
              gdal_installed <- suppressWarnings(
                system("python3 -c \"from osgeo import gdal\" 2>/dev/null", ignore.stdout = TRUE) == 0
              )

              rasterio_installed <- suppressWarnings(
                system("python3 -c \"import rasterio\" 2>/dev/null", ignore.stdout = TRUE) == 0
              )

              if (gdal_installed && rasterio_installed) {
                packageStartupMessage("GDAL and Rasterio successfully installed!")

                if (config$python$use_system_python) {
                  # Use system Python
                  python_path <- Sys.which("python3")
                  pkg_env$python_path <- python_path
                  reticulate::use_python(python_path, required = TRUE)
                  packageStartupMessage("Using system Python at: ", python_path)
                  return()
                }
              } else {
                packageStartupMessage("Installation appears to have failed. Trying alternative methods...")
              }
            }
          } else {
            packageStartupMessage("Installation declined. Trying alternative setup methods...")
          }
        }
      } else {
        packageStartupMessage("GDAL and Rasterio already installed in system Python.")
        if (config$python$use_system_python) {
          # Use system Python
          python_path <- Sys.which("python3")
          pkg_env$python_path <- python_path
          reticulate::use_python(python_path, required = TRUE)
          packageStartupMessage("Using system Python at: ", python_path)
          return()
        }
      }
    }

    # Fall back to conda setup if ubuntu-specific setup failed or not applicable
    if (config$python$fallback_to_conda) {
      geo_env_name <- config$python$conda_env_name
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

      # Always remove existing environment on Linux to avoid compatibility issues
      if (env_exists && os_type == "Linux") {
        packageStartupMessage("Existing environment found on Linux. Removing to avoid version conflicts...")
        system2(conda_bin, c("remove", "-y", "--name", geo_env_name, "--all"), stdout = TRUE, stderr = TRUE)
        env_exists <- FALSE
      }

      if (!env_exists) {
        packageStartupMessage("Creating new conda environment for geospatial processing: ", geo_env_name)

        # OS-specific package customizations
        if (os_type == "Windows") {
          create_windows_conda_env(conda_bin, geo_env_name, config)
        } else if (os_type == "Darwin") {
          create_macos_conda_env(conda_bin, geo_env_name, config)
        } else if (os_type == "Linux") {
          create_linux_conda_env(conda_bin, geo_env_name, config)
        } else {
          # Generic approach for other OS
          create_generic_conda_env(conda_bin, geo_env_name, config)
        }

        # Verify environment was created
        envs <- reticulate::conda_list()
        env_exists <- geo_env_name %in% envs$name

        if (!env_exists) {
          stop("Failed to create conda environment: ", geo_env_name)
        }
      }

      # Get python path from this environment
      py_path <- reticulate::conda_python(geo_env_name)
      pkg_env$python_path <- py_path

      # Use this conda environment
      reticulate::use_condaenv(geo_env_name, required = TRUE)

      # OS-specific environment variable handling
      if (os_type == "Linux") {
        packageStartupMessage("Setting up system library paths for Linux compatibility...")
        # On Linux, we want system libraries to be used instead of conda libraries
        Sys.unsetenv("LD_LIBRARY_PATH")
        packageStartupMessage("Cleared LD_LIBRARY_PATH to ensure system libraries are used")
      } else {
        # For non-Linux systems, use the conda libraries
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
        }
      }

      # Verify the environment works
      if (config$installation$verify_imports) {
        success <- verify_python_environment(py_path, config)

        # If verification failed and we're on Ubuntu, suggest manual installation
        if (!success && os_type == "Linux" && is_ubuntu) {
          handle_verification_failure(config)
        }
      }
    }

    packageStartupMessage("Python environment setup completed")
    packageStartupMessage("Using Python at: ", pkg_env$python_path)
    packageStartupMessage("Operating system: ", pkg_env$os_type)
  }, error = function(e) {
    # Ensure we have fallback values in pkg_env
    if (!"python_path" %in% names(pkg_env)) {
      tryCatch({
        pkg_env$python_path <- reticulate::conda_python(pkg_env$conda_env_name)
      }, error = function(e2) {
        pkg_env$python_path <- "PYTHON_NOT_FOUND"
      })
    }

    warning(paste(
      "Python environment setup failed:",
      conditionMessage(e),
      "Details:",
      conditionMessage(e)
    ))
  })

  # Verify that pkg_env was properly assigned in the namespace
  if (!exists("pkg_env", envir = pkg_namespace)) {
    packageStartupMessage("WARNING: pkg_env was not properly assigned. Re-assigning now.")
    assign("pkg_env", pkg_env, envir = pkg_namespace)
  }
}

# Helper function to create installation script
create_installation_script <- function(config) {
  install_script <- tempfile(fileext = ".sh")

  # Get system dependencies from config
  system_deps <- paste(config$os$linux$ubuntu$dependencies$system, collapse = " ")

  # Get pip dependencies from config
  pip_deps <- config$os$linux$ubuntu$dependencies$pip
  pip_deps_str <- if (length(pip_deps) > 0) {
    paste(pip_deps, collapse = " ")
  } else {
    ""
  }

  # Create installation script
  cat('#!/bin/bash
echo "========== GeoLink Installation Script =========="
echo "Installing system packages for GDAL and Rasterio."
echo "Sudo password will be required."

# Update package lists
echo "Updating package lists..."
sudo apt-get update

# Install the required packages
echo "Installing Python GDAL and Rasterio packages..."
sudo apt-get install -y ', system_deps, '

', if (nchar(pip_deps_str) > 0) {
  paste0('# Install pip packages
echo "Installing pip packages..."
pip3 install --user ', pip_deps_str, '
')
}, '

# Verify installation with proper string escaping
echo "Verifying installation..."
python3 -c "from osgeo import gdal; print(\'GDAL Version:\', gdal.VersionInfo())"
python3 -c "import rasterio; print(\'Rasterio Version:\', rasterio.__version__)"

echo "Installation complete. Please return to R to continue."
echo "Press Enter to close this terminal."
read
', file = install_script)

  # Make the script executable
  Sys.chmod(install_script, "755")

  return(install_script)
}

# Helper function for creating Windows conda environment
create_windows_conda_env <- function(conda_bin, env_name, config) {
  packageStartupMessage("Configuring for Windows...")
  reticulate::conda_create(env_name)
  packageStartupMessage("Installing base packages...")
  reticulate::conda_install(env_name,
                            c(paste0("python=", config$python$version), "numpy", "tqdm", "certifi"),
                            channel = "conda-forge")
  packageStartupMessage("Installing geospatial packages for Windows...")
  system2(conda_bin,
          c("install", "-y", "-n", env_name,
            "-c", "conda-forge",
            paste0("libtiff>=", config$gdal$min_version),
            paste0("gdal>=", config$gdal$min_version),
            "rasterio"),
          stdout = TRUE, stderr = TRUE)
}

# Helper function for creating macOS conda environment
create_macos_conda_env <- function(conda_bin, env_name, config) {
  packageStartupMessage("Configuring for macOS...")
  system2(conda_bin,
          c("create", "-y", "-n", env_name,
            "-c", "conda-forge",
            paste0("python=", config$python$version),
            "numpy",
            paste0("gdal>=", config$gdal$min_version),
            paste0("libtiff>=", config$gdal$min_version),
            "rasterio",
            "tqdm",
            "certifi"),
          stdout = TRUE, stderr = TRUE)
}

# Helper function for creating Linux conda environment
create_linux_conda_env <- function(conda_bin, env_name, config) {
  packageStartupMessage("Configuring for general Linux...")
  # Create a base conda environment with conda-forge
  packageStartupMessage("Creating base conda environment...")
  system2(conda_bin,
          c("create", "-y", "-n", env_name,
            "-c", "conda-forge",
            paste0("python=", config$python$version),
            "numpy=1.24.3",
            "pip",
            "wheel",
            "tqdm",
            "certifi"),
          stdout = TRUE, stderr = TRUE)

  # Try to install GDAL and rasterio using conda
  packageStartupMessage("Installing GDAL and rasterio via conda-forge...")
  system2(conda_bin,
          c("install", "-y", "-n", env_name,
            "-c", "conda-forge",
            paste0("gdal=", config$gdal$min_version),
            paste0("rasterio=", config$rasterio$min_version)),
          stdout = TRUE, stderr = TRUE)
}

# Helper function for creating a generic conda environment
create_generic_conda_env <- function(conda_bin, env_name, config) {
  packageStartupMessage("Using generic configuration...")
  system2(conda_bin,
          c("create", "-y", "-n", env_name,
            "-c", "conda-forge",
            paste0("python=", config$python$version),
            "numpy",
            paste0("gdal>=", config$gdal$min_version),
            paste0("libtiff>=", config$gdal$min_version),
            "rasterio",
            "tqdm",
            "certifi"),
          stdout = TRUE, stderr = TRUE)
}

# Helper function to verify Python environment
verify_python_environment <- function(py_path, config) {
  success <- tryCatch({
    # Simplified verification check
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
    rasterio_ok = True
except ImportError as e:
    print('Error importing Rasterio:', e)
    rasterio_ok = False

try:
    from osgeo import gdal
    print('GDAL version (from osgeo):', gdal.VersionInfo())
    gdal_ok = True
except ImportError as e:
    try:
        import gdal
        print('GDAL version:', gdal.VersionInfo())
        gdal_ok = True
    except ImportError as e:
        print('Error importing GDAL:', e)
        gdal_ok = False

if not (rasterio_ok and gdal_ok):
    raise ImportError('One or more required packages not available')
")
    TRUE
  }, error = function(e) {
    packageStartupMessage("Environment verification failed: ", conditionMessage(e))
    packageStartupMessage("Error details: ", reticulate::py_last_error())
    FALSE
  })

  return(success)
}

# Helper function to handle verification failure
handle_verification_failure <- function(config) {
  packageStartupMessage("Conda environment verification failed. Try manual installation.")
  packageStartupMessage("Please run the following command in a terminal:")
  packageStartupMessage("sudo apt-get install -y ", paste(config$os$linux$ubuntu$dependencies$system, collapse = " "))
  packageStartupMessage("Then restart R and try loading the package again.")

  # Ask if user wants to try again with terminal
  if (config$installation$prompt_user) {
    cat("\n")
    cat("Would you like to open a terminal to try the installation again? (y/n): ")
    retry_choice <- tolower(readline())

    if (startsWith(retry_choice, "y")) {
      # Create a terminal installation script with more aggressive installation
      retry_script <- tempfile(fileext = ".sh")

      # Get system dependencies from config
      system_deps <- paste(config$os$linux$ubuntu$dependencies$system, collapse = " ")

      # Add development packages
      dev_deps <- "python3-dev"

      # Get pip dependencies from config
      pip_deps <- c("gdal", "rasterio")
      if (length(config$os$linux$ubuntu$dependencies$pip) > 0) {
        pip_deps <- c(pip_deps, config$os$linux$ubuntu$dependencies$pip)
      }
      pip_deps_str <- paste(pip_deps, collapse = " ")

      cat('#!/bin/bash
echo "========== GeoLink Installation Retry =========="
echo "Installing system packages for GDAL and Rasterio."
echo "Sudo password will be required."

# Update package lists
echo "Updating package lists..."
sudo apt-get update

# Install the required packages
echo "Installing Python GDAL and Rasterio packages..."
sudo apt-get install -y ', system_deps, '

# Install development packages that might be needed
echo "Installing development packages..."
sudo apt-get install -y ', dev_deps, '

# Try pip installation as a fallback
echo "Trying pip installation as a fallback..."
pip3 install --user ', pip_deps_str, '

# Verify installation with proper string escaping
echo "Verifying installation..."
python3 -c "from osgeo import gdal; print(\'GDAL Version:\', gdal.VersionInfo())"
python3 -c "import rasterio; print(\'Rasterio Version:\', rasterio.__version__)"

echo "Installation complete. Please restart R to continue."
echo "Press Enter to close this terminal."
read
', file = retry_script)

      # Make the script executable
      Sys.chmod(retry_script, "755")

      # Find appropriate terminal executor
      terminal_exec <- get_terminal_executor(config)

      if (!is.null(terminal_exec)) {
        # Open a terminal to run the installation script
        if (terminal_exec == "x-terminal-emulator") {
          system(paste("x-terminal-emulator -e", shQuote(retry_script)), wait = FALSE)
        } else if (terminal_exec == "gnome-terminal") {
          system(paste("gnome-terminal -- ", shQuote(retry_script)), wait = FALSE)
        } else if (terminal_exec == "xterm") {
          system(paste("xterm -e", shQuote(retry_script)), wait = FALSE)
        }
      } else {
        cat("No graphical terminal found. Running installation in the current terminal.\n")
        system(retry_script)
      }

      packageStartupMessage("Please restart R after completing the installation in the terminal.")
    }
  }
}
