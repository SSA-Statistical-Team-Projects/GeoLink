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
