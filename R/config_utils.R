#' Read package configuration
#'
#' Reads the configuration file for the package based on the detected operating system
#'
#' @return A list containing configuration settings
#' @keywords internal
read_config <- function() {
  # Find the config file in the package
  config_path <- system.file("extdata", "config.yaml", package = "geolink")

  if (!file.exists(config_path)) {
    warning("Configuration file not found. Using default settings.")
    return(list(
      package = list(name = "geolink"),
      os = list(linux = list(ubuntu = list(enabled = TRUE))),
      python = list(conda_env_name = "geolink_env", use_system_python = TRUE),
      gdal = list(required = TRUE),
      rasterio = list(required = TRUE),
      installation = list(prompt_user = TRUE, use_sudo = TRUE)
    ))
  }

  # Attempt to read the YAML file
  if (!requireNamespace("yaml", quietly = TRUE)) {
    warning("yaml package not available. Using default settings.")
    return(list(
      package = list(name = "geolink"),
      os = list(linux = list(ubuntu = list(enabled = TRUE))),
      python = list(conda_env_name = "geolink_env", use_system_python = TRUE),
      gdal = list(required = TRUE),
      rasterio = list(required = TRUE),
      installation = list(prompt_user = TRUE, use_sudo = TRUE)
    ))
  }

  # Read the configuration
  config <- yaml::read_yaml(config_path)

  # Detect OS
  os_type <- Sys.info()["sysname"]

  # Check if we're on Ubuntu/Debian for Linux
  is_ubuntu <- FALSE
  if (os_type == "Linux") {
    tryCatch({
      if (file.exists("/etc/os-release")) {
        os_info <- readLines("/etc/os-release")
        if (any(grepl("Ubuntu|Debian", os_info))) {
          is_ubuntu <- TRUE
        }
      }
    }, error = function(e) {
      # Proceed with generic Linux handling
    })
  }

  # Add runtime detected properties
  config$runtime <- list(
    os_type = os_type,
    is_ubuntu = is_ubuntu,
    r_version = R.version.string
  )

  return(config)
}

#' Get terminal executor for Ubuntu
#'
#' Determines the best terminal emulator to use on Ubuntu systems
#'
#' @param config The package configuration
#' @return A character string with the terminal command to use
#' @keywords internal
get_terminal_executor <- function(config) {
  terminal_options <- config$installation$terminal_options

  for (terminal in terminal_options) {
    if (system(paste0("which ", terminal, " >/dev/null 2>&1")) == 0) {
      return(terminal)
    }
  }

  # Fallback to NULL if no terminal is found
  return(NULL)
}
