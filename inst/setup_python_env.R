# Setup Python virtual environment during package installation
setup_python_env <- function() {
  # Path where virtual environment will be created
  venv_path <- system.file(".venv", package = "GeoLink")

  # Ensure directory exists
  if (!dir.exists(venv_path)) {
    dir.create(venv_path, recursive = TRUE)
  }

  # Create virtual environment
  reticulate::virtualenv_create(
    envname = venv_path,
    python = Sys.which("python3")
  )

  # Install required packages
  reticulate::virtualenv_install(
    venv_path,
    packages = c("numpy", "pandas", "geopandas"),
    ignore_installed = TRUE
  )
}

# Run during package installation
.onAttach <- function(libname, pkgname) {
  setup_python_env()
}
