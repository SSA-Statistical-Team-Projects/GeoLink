#' @description
#' Internal function called when the GeoLink package is loaded.
#' Initializes package environment and displays startup message.
#'
#' @param libname Character string giving the library directory where the package
#'                defining the namespace was found.
#' @param pkgname Character string giving the name of the package.
#'
#' @details
#' This function creates a package-level environment to store configuration and
#' state information, including:
#' \itemize{
#'   \item conda_env_name: Name of the conda environment for this package
#'   \item python_initialized: Boolean flag indicating Python setup status
#'   \item os_type: Operating system type for platform-specific configurations
#'   \item python_path: Path to Python executable (set after initialization)
#' }
#'
#' @return NULL
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  pkg_env <- new.env(parent = emptyenv())

  pkg_env$conda_env_name <- "geolink_env"
  pkg_env$python_initialized <- FALSE
  pkg_env$os_type <- Sys.info()["sysname"]
  pkg_env$python_path <- NULL

  pkg_namespace <- asNamespace(pkgname)
  assign("pkg_env", pkg_env, envir = pkg_namespace)

  packageStartupMessage(
    "GeoLink loaded. Python environment will be initialized when needed.\n",
    "To manually initialize: geolink_setup_python()"
  )
}

#' Setup Python Environment for GeoLink
#'
#' @description
#' Configures and installs the Python environment required for GeoLink's
#' geospatial operations. This includes installing Miniconda (if needed),
#' creating a conda environment, and installing necessary Python packages.
#'
#' @param force Logical. If TRUE, forces reinstallation even if the environment
#'              already exists. Default is FALSE.
#' @param minimal Logical. If TRUE, installs only essential packages (Python and NumPy).
#'                If FALSE, installs full geospatial stack. Default is FALSE.
#' @param verbose Logical. If TRUE, displays detailed installation progress messages.
#'                Default is TRUE.
#'
#' @return Invisible TRUE if setup succeeds, stops with error if setup fails.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks if Python environment is already initialized
#'   \item Installs Miniconda if not present
#'   \item Creates a conda environment named "geolink_env"
#'   \item Installs Python packages based on the minimal parameter:
#'     \itemize{
#'       \item Minimal: Python 3.10, NumPy
#'       \item Full: Adds GDAL, Rasterio, PyProj, Fiona, Shapely (platform-specific)
#'     }
#'   \item Activates the environment for use with reticulate
#' }
#'
#' Platform-specific considerations:
#' \itemize{
#'   \item Windows: Installs a reduced set of packages due to compatibility
#'   \item Unix/Linux/macOS: Installs full geospatial stack
#' }
#'
#' @examples
#' \dontrun{
#' # Basic setup with full geospatial packages
#' geolink_setup_python()
#'
#' # Minimal setup for basic operations
#' geolink_setup_python(minimal = TRUE)
#'
#' # Force reinstallation
#' geolink_setup_python(force = TRUE)
#'
#' # Quiet installation
#' geolink_setup_python(verbose = FALSE)
#' }
#'
#' @export
#' @importFrom reticulate conda_binary install_miniconda conda_list
#' @importFrom reticulate conda_create conda_install use_condaenv conda_python
geolink_setup_python <- function(force = FALSE, minimal = FALSE, verbose = TRUE) {
  pkg_env <- get("pkg_env", envir = asNamespace("GeoLink"))

  if (pkg_env$python_initialized && !force) {
    if (verbose) {
      message("Python environment already initialized.")
      message("Use force = TRUE to reinstall.")
    }
    return(invisible(TRUE))
  }

  if (verbose) {
    message("Setting up Python environment for GeoLink...")
    message("This may take several minutes on first installation.")
  }

  conda_bin <- tryCatch(
    reticulate::conda_binary(),
    error = function(e) NULL
  )

  if (is.null(conda_bin) || !file.exists(conda_bin)) {
    if (verbose) {
      message("Conda not found. Installing Miniconda...")
      message("This is a one-time installation.")
    }

    tryCatch({
      reticulate::install_miniconda()
      conda_bin <- reticulate::conda_binary()
    }, error = function(e) {
      stop("Failed to install Miniconda: ", e$message)
    })

    if (!file.exists(conda_bin)) {
      stop("Miniconda installation failed. Please install manually.")
    }
  }

  envs <- tryCatch({
    reticulate::conda_list()
  }, error = function(e) {
    if (verbose) {
      message("Warning: Could not list conda environments: ", e$message)
    }
    NULL
  })

  env_exists <- FALSE
  if (!is.null(envs) && is.data.frame(envs) && "name" %in% names(envs)) {
    env_exists <- pkg_env$conda_env_name %in% envs$name
  }

  if (!env_exists || force) {
    if (env_exists && force && verbose) {
      message("Removing existing environment for fresh installation...")
    }

    if (verbose) {
      message("Creating conda environment: ", pkg_env$conda_env_name)
    }

    tryCatch({
      reticulate::conda_create(
        envname = pkg_env$conda_env_name,
        packages = c("python=3.10", "numpy"),
        conda = conda_bin
      )
    }, error = function(e) {
      stop("Failed to create conda environment: ", e$message)
    })

    if (!minimal) {
      if (verbose) {
        message("Installing geospatial packages...")
        message("This may take 5-10 minutes on first installation.")
      }

      # Platform-specific packages
      if (pkg_env$os_type == "Windows") {
        packages <- c("gdal>=3.6.0", "rasterio", "libtiff>=4.6.1")
        if (verbose) {
          message("Windows detected: Installing core geospatial packages")
        }
      } else {
        packages <- c("gdal>=3.6.0", "rasterio", "pyproj", "fiona", "shapely")
        if (verbose) {
          message("Unix-like system detected: Installing full geospatial stack")
        }
      }

      tryCatch({
        reticulate::conda_install(
          envname = pkg_env$conda_env_name,
          packages = packages,
          channel = "conda-forge",
          conda = conda_bin
        )
      }, error = function(e) {
        warning("Failed to install some geospatial packages: ", e$message)
        message("You may need to install these manually using conda.")
      })
    }
  } else {
    if (verbose) {
      message("Environment '", pkg_env$conda_env_name, "' already exists.")
    }
  }

  if (verbose) {
    message("Activating Python environment...")
  }

  tryCatch({
    reticulate::use_condaenv(pkg_env$conda_env_name, required = TRUE)
  }, error = function(e) {
    stop("Failed to activate conda environment: ", e$message)
  })

  pkg_env$python_initialized <- TRUE
  pkg_env$python_path <- reticulate::conda_python(pkg_env$conda_env_name)

  if (verbose) {
    message("\n", paste(rep("=", 50), collapse = ""))
    message("Python environment setup completed successfully!")
    message("Environment name: ", pkg_env$conda_env_name)
    message("Python path: ", pkg_env$python_path)
    message(paste(rep("=", 50), collapse = ""), "\n")
  }

  return(invisible(TRUE))
}

#' Check Python Environment Status
#'
#' @description
#' Checks whether the Python environment for GeoLink has been initialized.
#'
#' @return Logical. TRUE if the Python environment is initialized and ready,
#'         FALSE otherwise.
#'
#' @details
#' This function checks the internal package state to determine if the Python
#' environment has been set up. It does not verify the actual existence or
#' functionality of the Python installation.
#'
#' @examples
#' \dontrun{
#' # Check if Python is ready
#' if (!geolink_python_ready()) {
#'   geolink_setup_python()
#' }
#'
#' # Use in conditional logic
#' if (geolink_python_ready()) {
#'   message("Ready for Python-dependent operations")
#' }
#' }
#'
#' @export
geolink_python_ready <- function() {
  pkg_env <- get("pkg_env", envir = asNamespace("GeoLink"))
  return(pkg_env$python_initialized)
}

#' Ensure Python Environment is Initialized
#'
#' @description
#' Internal function that checks if the Python environment is initialized
#' and automatically sets it up if not. This provides lazy loading of the
#' Python environment.
#'
#' @return NULL (invisible). Side effect: initializes Python if needed.
#'
#' @details
#' This function is called internally by Python-dependent functions to ensure
#' the environment is ready before attempting operations. It provides automatic
#' setup with default parameters.
#'
#' @keywords internal
ensure_python_initialized <- function() {
  if (!geolink_python_ready()) {
    message("Python environment not initialized. Setting up now...")
    message("This is a one-time setup that may take several minutes.")
    geolink_setup_python(verbose = TRUE)
  }
  invisible(NULL)
}

#' Execute GDAL Operation Safely
#'
#' @description
#' Internal wrapper function that ensures Python/GDAL is initialized before
#' executing GDAL-dependent operations.
#'
#' @param operation_fn Function to execute that requires GDAL/Python
#' @param ... Additional arguments passed to operation_fn
#'
#' @return The result of operation_fn
#'
#' @details
#' This function provides a safe execution context for GDAL operations by:
#' \enumerate{
#'   \item Ensuring Python environment is initialized
#'   \item Executing the provided function with error handling
#'   \item Returning results or propagating errors appropriately
#' }
#'
#' @examples
#' \dontrun{
#' # Internal use only
#' result <- safe_gdal_operation(function(x) {
#'   # GDAL-dependent operation
#'   gdal_translate(x)
#' }, input_file)
#' }
#'
#' @keywords internal
safe_gdal_operation <- function(operation_fn, ...) {
  ensure_python_initialized()

  tryCatch({
    operation_fn(...)
  }, error = function(e) {
    if (grepl("python|gdal|rasterio", tolower(e$message))) {
      stop("GDAL operation failed. Try reinstalling with: geolink_setup_python(force = TRUE)\n",
           "Original error: ", e$message)
    } else {
      stop(e)
    }
  })
}

#' Configure GeoLink Settings
#'
#' @description
#' Sets and saves user preferences for GeoLink package behavior, including
#' Python usage and automatic installation settings.
#'
#' @param use_python Logical. If TRUE, enables Python-dependent features.
#'                   If FALSE, restricts to R-only operations. Default is TRUE.
#' @param auto_install Logical. If TRUE, automatically installs Python dependencies
#'                     when needed. If FALSE, requires manual initialization. Default is FALSE.
#'
#' @return NULL (invisible). Side effect: saves configuration to disk.
#'
#' @details
#' Configuration is saved to the user's config directory (platform-specific):
#' \itemize{
#'   \item Windows: \%LOCALAPPDATA\%/GeoLink/config.yml
#'   \item macOS: ~/Library/Application Support/GeoLink/config.yml
#'   \item Linux: ~/.config/GeoLink/config.yml
#' }
#'
#' The configuration persists across R sessions and package reloads.
#'
#' @examples
#' \dontrun{
#' # Enable automatic Python setup
#' geolink_configure(use_python = TRUE, auto_install = TRUE)
#'
#' # Disable Python features (R-only mode)
#' geolink_configure(use_python = FALSE)
#'
#' # Check current configuration
#' config <- load_config()
#' print(config)
#' }
#'
#' @export
#' @importFrom rappdirs user_config_dir
#' @importFrom yaml write_yaml
geolink_configure <- function(use_python = TRUE, auto_install = FALSE) {
  if (!is.logical(use_python)) {
    stop("use_python must be TRUE or FALSE")
  }
  if (!is.logical(auto_install)) {
    stop("auto_install must be TRUE or FALSE")
  }

  config <- list(
    use_python = use_python,
    auto_install = auto_install,
    config_version = "1.0",
    last_modified = Sys.time()
  )

  config_dir <- rappdirs::user_config_dir("GeoLink")
  config_path <- file.path(config_dir, "config.yml")

  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }

  tryCatch({
    yaml::write_yaml(config, config_path)
    message("Configuration saved successfully")
    message("Location: ", config_path)
    message("Settings:")
    message("  - use_python: ", use_python)
    message("  - auto_install: ", auto_install)
  }, error = function(e) {
    warning("Failed to save configuration: ", e$message)
  })

  invisible(NULL)
}

#' Load GeoLink Configuration
#'
#' @description
#' Internal function to load user configuration from disk. Returns default
#' configuration if no saved configuration exists.
#'
#' @return List containing configuration settings:
#' \itemize{
#'   \item use_python: Logical, whether Python features are enabled
#'   \item auto_install: Logical, whether to auto-install Python dependencies
#'   \item config_version: Character, version of configuration format
#'   \item last_modified: POSIXct, when configuration was last modified
#' }
#'
#' @details
#' This function is called internally by other GeoLink functions to determine
#' package behavior. If no configuration file exists, sensible defaults are
#' returned (Python enabled, auto-install disabled).
#'
#' @examples
#' \dontrun{
#' # Internal use
#' config <- load_config()
#' if (config$use_python) {
#'   # Execute Python-dependent code
#' }
#' }
#'
#' @keywords internal
#' @importFrom rappdirs user_config_dir
#' @importFrom yaml read_yaml
load_config <- function() {
  config_path <- file.path(rappdirs::user_config_dir("GeoLink"), "config.yml")

  if (file.exists(config_path)) {
    tryCatch({
      config <- yaml::read_yaml(config_path)

      if (!is.list(config)) {
        warning("Invalid configuration file, using defaults")
        config <- NULL
      }

      if (!is.null(config)) {
        if (!"use_python" %in% names(config)) config$use_python <- TRUE
        if (!"auto_install" %in% names(config)) config$auto_install <- FALSE
        if (!"config_version" %in% names(config)) config$config_version <- "1.0"
        return(config)
      }
    }, error = function(e) {
      warning("Failed to load configuration: ", e$message)
    })
  }

  return(list(
    use_python = TRUE,
    auto_install = FALSE,
    config_version = "1.0",
    last_modified = NULL
  ))
}

#' Get Python Environment Information
#'
#' @description
#' Returns detailed information about the current Python environment setup
#' for GeoLink, useful for debugging and verification.
#'
#' @return A list containing:
#' \itemize{
#'   \item initialized: Logical, whether Python is initialized
#'   \item env_name: Character, name of the conda environment
#'   \item python_path: Character, path to Python executable (if initialized)
#'   \item os_type: Character, operating system type
#'   \item config: List, current configuration settings
#' }
#'
#' @examples
#' \dontrun{
#' # Get environment information
#' info <- geolink_python_info()
#' print(info)
#'
#' # Check specific details
#' if (info$initialized) {
#'   cat("Python location:", info$python_path, "\n")
#' }
#' }
#'
#' @export
geolink_python_info <- function() {
  pkg_env <- get("pkg_env", envir = asNamespace("GeoLink"))
  config <- load_config()

  info <- list(
    initialized = pkg_env$python_initialized,
    env_name = pkg_env$conda_env_name,
    python_path = pkg_env$python_path,
    os_type = pkg_env$os_type,
    config = config
  )

  class(info) <- c("geolink_python_info", "list")
  return(info)
}

#' Print Method for GeoLink Python Information
#'
#' @description
#' Custom print method for geolink_python_info objects.
#'
#' @param x A geolink_python_info object
#' @param ... Additional arguments (unused)
#'
#' @return NULL (invisible). Side effect: prints information.
#'
#' @export
#' @method print geolink_python_info
print.geolink_python_info <- function(x, ...) {
  cat("GeoLink Python Environment Information\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  cat("Status:", ifelse(x$initialized, "Initialized", "Not initialized"), "\n")
  cat("Environment Name:", x$env_name, "\n")
  if (x$initialized && !is.null(x$python_path)) {
    cat("Python Path:", x$python_path, "\n")
  }
  cat("Operating System:", x$os_type, "\n")
  cat("\nConfiguration:\n")
  cat("  Use Python:", x$config$use_python, "\n")
  cat("  Auto Install:", x$config$auto_install, "\n")
  invisible(x)
}

#' Reset Python Environment
#'
#' @description
#' Resets the Python environment for GeoLink, useful for troubleshooting
#' or changing Python configurations.
#'
#' @param remove_env Logical. If TRUE, attempts to remove the conda environment
#'                   completely. Default is FALSE.
#' @param verbose Logical. If TRUE, displays detailed messages. Default is TRUE.
#'
#' @return NULL (invisible).
#'
#' @examples
#' \dontrun{
#' # Soft reset (just marks as uninitialized)
#' geolink_reset_python()
#'
#' # Hard reset (removes environment)
#' geolink_reset_python(remove_env = TRUE)
#' }
#'
#' @export
geolink_reset_python <- function(remove_env = FALSE, verbose = TRUE) {
  pkg_env <- get("pkg_env", envir = asNamespace("GeoLink"))

  if (verbose) {
    message("Resetting Python environment...")
  }

  pkg_env$python_initialized <- FALSE
  pkg_env$python_path <- NULL

  if (remove_env) {
    if (verbose) {
      message("Note: Automatic environment removal not supported by reticulate.")
      message("Please manually remove conda environment '", pkg_env$conda_env_name,
              "' using conda command line:")
      message("  conda env remove -n ", pkg_env$conda_env_name)
    }
  }

  if (verbose) {
    message("Python environment reset complete.")
    message("Run geolink_setup_python() to reinitialize.")
  }

  invisible(NULL)
}
