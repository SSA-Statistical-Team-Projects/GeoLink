.onLoad <- function(libname, pkgname) {
  tryCatch({
    # Use reticulate to find Python
    python_path <- reticulate::py_discover_config()$python

    # If Python found, create virtual environment
    if (!is.null(python_path) && file.exists(python_path)) {
      # Construct the virtual environment path in inst/python/virtual_env
      venv_path <- file.path(system.file(package = pkgname), "python", "virtual_env")

      # Create virtual environment directory if not exists
      if (!dir.exists(venv_path)) {
        dir.create(venv_path, recursive = TRUE)
      }

      # Create virtual environment
      reticulate::virtualenv_create(
        envname = venv_path,
        python = python_path
      )

      # Install required packages
      reticulate::virtualenv_install(
        venv_path,
        packages = c("numpy", "rasterio", "tqdm"),
        ignore_installed = TRUE
      )

      # Use the virtual environment
      reticulate::use_virtualenv(venv_path, required = FALSE)
    } else {
      warning("No suitable Python installation found. Some package features may be limited.")
    }
  }, error = function(e) {
    warning("Could not set up Python virtual environment: ", e$message)
  })
}

  tryCatch({
    env_status <- verify_python_env()
    if (!env_status$venv_exists) {
      warning("Virtual environment not found or not properly set up.")
    }
  }, error = function(e) {
    warning("Error verifying Python environment: ", e$message)
  })
