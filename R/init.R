.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    install.packages("reticulate")
  }
  library(reticulate)

  setup_python_environment <- function() {
    # Define the virtual environment directory in the package's inst/python folder
    venv_dir <- file.path(system.file("python", package = pkgname))

    # Ensure the directory exists
    if (!dir.exists(venv_dir)) {
      dir.create(venv_dir, recursive = TRUE)
    }

    # Check for Python
    python_path <- tryCatch({
      py_discover_config()$python
    }, error = function(e) NULL)

    if (is.null(python_path)) {
      # Automatically install Python if not found
      message("Python not found. Installing Python 3.13.0...")
      install_python(version = "3.13.0", force = TRUE)
      python_path <- reticulate::install_python(version = "3.13.0")
    }

    # Re-check after installing Python
    python_path <- tryCatch({
      py_discover_config()$python
    }, error = function(e) NULL)

    if (is.null(python_path)) {
      stop("Failed to install or find a suitable Python version.")
    }

    # Use the Python installation and create a virtual environment
    message("Using Python at: ", python_path)
    use_python(python_path, required = TRUE)

    if (!dir.exists(file.path(venv_dir, "bin"))) {
      message("Creating virtual environment...")
      virtualenv_create(venv_dir, python = python_path)
    } else {
      message("Virtual environment already exists.")
    }

    # Activate the virtual environment and install required Python packages
    use_virtualenv(venv_dir, required = TRUE)
    message("Installing required Python packages...")
    py_install(c("rasterio", "tqdm", "numpy"), envname = venv_dir, pip = TRUE)

    message("Python environment is set up and ready.")
  }

  # Call the setup function
  tryCatch({
    setup_python_environment()
  }, error = function(e) {
    stop("Failed to initialize the Python environment: ", e$message)
  })
}
