#' Load Configuration File
#'
#' Loads a YAML configuration file from the package config directory.
#'
#' @param config_file Character. Name of config file (with or without .yaml extension).
#'   Common values: "site_protocols", "defaults"
#' @param config_dir Character. Path to config directory. Defaults to package config/
#'
#' @return List containing parsed YAML configuration
#'
#' @export
#'
#' @keywords internal
#'
load_config <- function(config_file, config_dir = NULL) {
  # Ensure .yaml extension
  if (!grepl("\\.yaml$", config_file)) {
    config_file <- paste0(config_file, ".yaml")
  }

  # Determine config directory
  if (is.null(config_dir)) {
    config_dir <- system.file("config", package = "BATD")
  }

  # Build full path
  config_path <- file.path(config_dir, config_file)

  # Check if file exists
  if (!file.exists(config_path)) {
    warning("Configuration file not found: ", config_path)
    return(NULL)
  }

  # Load and parse YAML
  tryCatch(
    {
      yaml::read_yaml(config_path)
    },
    error = function(e) {
      warning("Error parsing YAML configuration: ", e$message)
      return(NULL)
    }
  )
}

#' Get Site Protocol Information
#'
#' Retrieves protocol definitions for a specified site from site_protocols.yaml
#'
#' @param site Character. Site name (e.g., "UCLA", "KKI")
#' @param config List. Pre-loaded config (optional). If NULL, loads from file.
#'
#' @return List with site information including protocols, or NULL if site not found
#'
#' @export
#'
#' @keywords internal
#'
get_site_protocols <- function(site, config = NULL) {
  # Load config if not provided
  if (is.null(config)) {
    config <- load_config("site_protocols")
  }

  if (is.null(config) || !("sites" %in% names(config))) {
    warning("Unable to load site protocols configuration")
    return(NULL)
  }

  # Get site from config
  site_info <- config$sites[[site]]

  if (is.null(site_info)) {
    warning("Site '", site, "' not found in configuration. Using default 'NA'")
    site_info <- config$sites[["NA"]]
  }

  return(site_info)
}

#' Get Protocol Name
#'
#' Retrieves the human-readable protocol name for a given protocol ID at a site.
#'
#' @param protocol_id Numeric or character. Protocol ID
#' @param site Character. Site name
#' @param config List. Pre-loaded config (optional)
#'
#' @return Character. Protocol name, or NA_character_ if not found
#'
#' @export
#'
#' @keywords internal
#'
get_protocol_name <- function(protocol_id, site, config = NULL) {
  site_info <- get_site_protocols(site, config)

  if (is.null(site_info) || !("protocols" %in% names(site_info))) {
    return(NA_character_)
  }

  # Find matching protocol
  for (protocol in site_info$protocols) {
    if (protocol$id == protocol_id) {
      return(protocol$name)
    }
  }

  return(NA_character_)
}

#' Get Default Configuration
#'
#' Retrieves default analysis and output parameters from defaults.yaml
#'
#' @param section Character. Configuration section (e.g., "analysis", "extraction", "plot")
#'   If NULL, returns entire config
#' @param config List. Pre-loaded config (optional)
#'
#' @return List containing requested defaults, or NULL if not found
#'
#' @export
#'
#' @keywords internal
#'
get_defaults <- function(section = NULL, config = NULL) {
  # Load config if not provided
  if (is.null(config)) {
    config <- load_config("defaults")
  }

  if (is.null(config)) {
    warning("Unable to load defaults configuration")
    return(NULL)
  }

  # Return section or entire config
  if (is.null(section)) {
    return(config)
  }

  return(config[[section]])
}

#' Validate Configuration
#'
#' Validates that required configuration fields are present and well-formed
#'
#' @param config List. Configuration to validate
#' @param config_type Character. Type of config: "site_protocols" or "defaults"
#'
#' @return Logical. TRUE if valid, FALSE otherwise
#'
#' @export
#'
#' @keywords internal
#'
validate_configuration <- function(config, config_type = "site_protocols") {
  if (is.null(config) || !is.list(config)) {
    return(FALSE)
  }

  if (config_type == "site_protocols") {
    # Check required fields
    if (!("sites" %in% names(config))) {
      warning("Site protocols config missing 'sites' key")
      return(FALSE)
    }

    if (!is.list(config$sites) || length(config$sites) == 0) {
      warning("Site protocols config has empty sites list")
      return(FALSE)
    }

    # Validate each site has protocols
    for (site_name in names(config$sites)) {
      site <- config$sites[[site_name]]
      if (!is.list(site) || !("protocols" %in% names(site))) {
        warning("Site '", site_name, "' missing 'protocols' field")
        return(FALSE)
      }
    }

    return(TRUE)
  }

  if (config_type == "defaults") {
    # Basic validation for defaults
    return(is.list(config) && length(config) > 0)
  }

  return(FALSE)
}
