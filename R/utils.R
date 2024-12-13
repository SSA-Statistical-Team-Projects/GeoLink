# In R/utils.R or a similar file
#' Verify Python Virtual Environment
#'
#' Checks the status of the Python virtual environment for the package
#'
#' @return A list containing virtual environment details
#' @export
verify_python_env <- function() {
  venv_path <- file.path(system.file(package = "GeoLink"), "python", "virtual_env")

  list(
    venv_exists = dir.exists(venv_path),
    venv_path = venv_path,
    python_config = tryCatch(
      reticulate::py_config(),
      error = function(e) NULL
    )
  )
}

# Optional: Add a print method for easy debugging
print.python_env_status <- function(x) {
  cat("Virtual Environment Status:\n")
  cat("Path:", x$venv_path, "\n")
  cat("Exists:", x$venv_exists, "\n")
  if (!is.null(x$python_config)) {
    cat("Python Version:", x$python_config$version, "\n")
  } else {
    cat("Python configuration not available\n")
  }
}
