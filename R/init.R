.onLoad <- function(libname, pkgname) {
  tryCatch({
    # Detect system Python installations
    python_paths <- c(
      Sys.which("python"),
      Sys.which("python3"),
      reticulate::py_discover_config()$python
    )

    # Find Anaconda/Miniconda installations
    possible_base_paths <- c(
      Sys.getenv("LOCALAPPDATA"),
      Sys.getenv("APPDATA"),
      Sys.getenv("USERPROFILE"),
      Sys.getenv("ProgramData"),
      Sys.getenv("ProgramFiles"),
      Sys.getenv("ProgramFiles(x86)")
    )

    # Search for conda installations
    conda_paths <- list.files(
      path = possible_base_paths,
      pattern = "^(Ana|Mini)conda3$",
      full.names = TRUE,
      recursive = FALSE
    )

    # Combine all found Python paths
    all_paths <- c(
      python_paths,
      file.path(conda_paths, "python.exe")
    )

    # Filter for existing paths
    valid_paths <- unique(all_paths[file.exists(all_paths)])

    if (length(valid_paths) > 0) {
      python_path <- valid_paths[1]
      Sys.setenv(RETICULATE_PYTHON = python_path)

      # Set up virtual environment
      venv_path <- file.path(system.file(package = pkgname), "python", "virtual_env")
      if (!dir.exists(venv_path)) {
        dir.create(venv_path, recursive = TRUE)
      }

      reticulate::virtualenv_create(
        envname = venv_path,
        python = python_path
      )

      reticulate::virtualenv_install(
        venv_path,
        packages = c("numpy", "rasterio", "tqdm"),
        ignore_installed = TRUE
      )

      reticulate::use_virtualenv(venv_path, required = FALSE)
    } else {
      warning("No Python installation found.")
    }
  }, error = function(e) {
    warning("Python environment setup failed: ", e$message)
  })
}
