#' Common Extraction Helpers
#'
#' Shared utility functions for extracting and processing Brain Gauge data
#' used by both NF and OF format extraction functions.
#'
#' @keywords internal

#' Normalize Demographic Information
#'
#' Standardizes demographic values to consistent format across old and new Brain Gauge formats.
#'
#' @param race Character string specifying race. Accepts "Default", "caucasian", etc.
#' @param gender Character string specifying gender. Accepts "M", "F", "Male", "Female"
#' @param handedness Character string specifying handedness. Accepts "R", "L", "Right", "Left"
#'
#' @return A list with normalized values
#'
#' @keywords internal
#' @export

normalize_demographics <- function(race = NA_character_, gender = NA_character_, handedness = NA_character_) {
  
  # Normalize race
  if (is.na(race) || race == "") {
    race <- NA_character_
  } else if (tolower(race) == "default") {
    race <- "caucasian"
  }
  
  # Normalize gender
  if (is.na(gender) || gender == "") {
    gender <- NA_character_
  } else if (gender == "M") {
    gender <- "Male"
  } else if (gender == "F") {
    gender <- "Female"
  }
  
  # Normalize handedness
  if (is.na(handedness) || handedness == "") {
    handedness <- NA_character_
  } else if (handedness == "R") {
    handedness <- "Right"
  } else if (handedness == "L") {
    handedness <- "Left"
  }
  
  list(
    race = race,
    gender = gender,
    handedness = handedness
  )
}

#' Standardize Column Types
#'
#' Converts extracted dataframe columns to appropriate types (numeric, character, etc.)
#' Handles NAs and invalid conversions gracefully.
#'
#' @param data A dataframe with extracted Brain Gauge data
#' @param numeric_cols Character vector of column names to convert to numeric
#'
#' @return Dataframe with standardized column types
#'
#' @keywords internal
#' @export

standardize_column_types <- function(data, numeric_cols = NULL) {
  
  if (is.null(numeric_cols)) {
    numeric_cols <- c(
      "id", "protocol", "trialNumber", "value", "expected", "response",
      "correctResponse", "responseTime", "numberofPracticeTrials",
      "numberofTestTrials", "ISI", "interval_between_adaptive_and_test",
      "stim1amplitude", "stim1frequency", "stim1duration",
      "stim2amplitude", "stim2frequency", "stim2duration",
      "astim1amplitude", "astim1frequency", "astim1duration",
      "astim2amplitude", "astim2frequency", "astim2duration",
      "run", "session"
    )
  }
  
  # Convert to character first to handle mixed types
  data <- suppressWarnings(
    data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  )
  
  # Selectively convert columns to numeric
  cols_to_convert <- numeric_cols[numeric_cols %in% names(data)]
  data[cols_to_convert] <- suppressWarnings(
    sapply(data[cols_to_convert], as.numeric)
  )
  
  # Handle NAs in numberofPracticeTrials
  if ("numberofPracticeTrials" %in% names(data)) {
    data$numberofPracticeTrials[is.na(data$numberofPracticeTrials)] <- 0
  }
  
  data
}

#' Extract Participant Metadata (Format-Aware)
#'
#' Extracts participant demographic information from raw data.
#' Handles different field names between NF and OF formats.
#'
#' @param data A dataframe with raw extracted data
#' @param format Character string: either "NF" or "OF"
#'
#' @return A single-row dataframe with participant details
#'
#' @keywords internal
#' @export

extract_participant_metadata <- function(data, format = c("NF", "OF")) {
  
  format <- match.arg(format)
  
  if (format == "NF") {
    # NF format field names
    field_map <- list(
      id = "number",
      race = "race",
      gender = "gender",
      handedness = "handedness",
      birthYear = "birthYear"
    )
  } else {
    # OF format field names
    field_map <- list(
      id = "Subject_Number",
      race = "Race",
      gender = "Gender",
      handedness = "Handedness",
      birthYear = "Birthdate"
    )
  }
  
  result <- data.frame(
    id = NA_character_,
    race = NA_character_,
    gender = NA_character_,
    handedness = NA_character_,
    birthYear = NA_character_,
    stringsAsFactors = FALSE
  )
  
  for (output_name in names(field_map)) {
    input_name <- field_map[[output_name]]
    if (input_name %in% names(data)) {
      result[[output_name]] <- as.character(data[[input_name]][1])
    }
  }
  
  # Normalize demographics
  demographics <- normalize_demographics(
    race = result$race,
    gender = result$gender,
    handedness = result$handedness
  )
  
  result$race <- demographics$race
  result$gender <- demographics$gender
  result$handedness <- demographics$handedness
  
  # Extract year from birthYear if it's a full date (OF format)
  if (!is.na(result$birthYear) && nchar(result$birthYear) > 4) {
    result$birthYear <- stringr::str_sub(result$birthYear, -4, -1)
  }
  
  result
}

#' Account for Repeated Protocol Runs
#'
#' Identifies when a participant completed the same protocol multiple times
#' within a session and assigns run numbers accordingly.
#'
#' @param data A dataframe with extracted data
#'
#' @return Dataframe with new "run" column indicating which run of each protocol
#'
#' @keywords internal
#' @export

account_for_repeated_runs <- function(data) {
  
  participants <- unique(data$id)
  run_output_p <- data.frame()
  
  for (p in seq_along(participants)) {
    current_p <- data[data$id == participants[p], ]
    sessions <- unique(current_p$session)
    
    for (s in seq_along(sessions)) {
      current_s <- current_p[current_p$session == sessions[s], ]
      times <- unique(current_s$time)
      protocols_completed_in_session <- list()
      
      for (t in seq_along(times)) {
        current_t <- current_p[current_p$time == times[t], ]
        protocols_completed_in_session <- append(
          protocols_completed_in_session,
          current_t$protocolName[1]
        )
        repeats <- sum(protocols_completed_in_session == current_t$protocolName[1])
        
        t_repeated <- if (repeats > 1) {
          rep(repeats, times = nrow(current_t))
        } else {
          rep(1, times = nrow(current_t))
        }
        
        current_p[current_p$time == times[t], "run"] <- t_repeated
      }
    }
    
    run_output_p <- rbind(run_output_p, current_p)
  }
  
  run_output_p
}

#' Validate Extracted Data Quality
#'
#' Performs comprehensive validation checks on extracted Brain Gauge data.
#'
#' @param data A dataframe with extracted data
#' @param format Character string: either "NF" or "OF"
#' @param site Character string specifying the site
#'
#' @return Invisibly returns TRUE if all checks pass. Raises warnings/errors otherwise.
#'
#' @keywords internal
#' @export

validate_extracted_data <- function(data, format = c("NF", "OF"), site = NA_character_) {
  
  format <- match.arg(format)
  
  required_cols <- c(
    "id", "date", "time", "protocol", "protocolName",
    "trialNumber", "value", "response", "correctResponse", "responseTime"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(invisible(FALSE))
  }
  
  # Check for data quality issues
  if (all(is.na(data$protocolName))) {
    warning("All protocol names are NA - extraction may have failed")
    return(invisible(FALSE))
  }
  
  if (nrow(data) == 0) {
    warning("Extracted dataframe is empty")
    return(invisible(FALSE))
  }
  
  invisible(TRUE)
}
