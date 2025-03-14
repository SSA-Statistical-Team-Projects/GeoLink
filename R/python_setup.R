# Add this to R/python-setup.R (new file)
load_python_utils <- function() {
  # Access the package environment that was set up in .onLoad
  if (!exists("pkg_env") || is.null(pkg_env$python_path)) {
    stop("Python environment not initialized. Please report this as a bug.")
  }

  # Get the conda environment name
  conda_env_name <- pkg_env$conda_env_name

  # Ensure we're using the right Python
  reticulate::use_condaenv(conda_env_name, required = TRUE)

  # Get the path to the python_scripts directory
  package_path <- system.file("python_scripts", package = "geolink")

  # If we're in development mode, look in inst/python_scripts
  if (package_path == "") {
    package_path <- file.path("inst", "python_scripts")
    if (!dir.exists(package_path)) {
      stop("Cannot find Python scripts directory. Please ensure 'raster_utils.py' is in 'inst/python_scripts/'")
    }
  }

  # Load Python script
  python_utils_path <- file.path(package_path, "raster_utils.py")

  # Check if file exists
  if (!file.exists(python_utils_path)) {
    stop(sprintf("Error: raster_utils.py not found at %s", python_utils_path))
  }

  # Try to add the directory to Python's path before importing
  dirname_utils <- dirname(python_utils_path)
  reticulate::py_run_string(sprintf("import sys; sys.path.insert(0, r'%s')", dirname_utils))

  # Import the module
  module_name <- tools::file_path_sans_ext(basename(python_utils_path))

  tryCatch({
    # Try importing as a module first
    raster_utils <- reticulate::import_from_path(module_name, path = dirname_utils)
    return(raster_utils)
  }, error = function(e) {
    # If that fails, try source_python
    message("Module import failed. Trying source_python...")
    tryCatch({
      reticulate::source_python(python_utils_path)
      return(TRUE)
    }, error = function(e2) {
      stop("Failed to load Python utilities: ", e2$message)
    })
  })
}
