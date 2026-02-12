#' Common Extraction Helpers
#'
#' Shared utility functions for extracting and processing Brain Gauge data
#' used by both NF and OF format extraction functions.
#'
#' @keywords internal

#' Normalize Demographic Information
#'
#' Standardizes demographic values to consistent format across old and new Brain Gauge formats.
#' Can accept either a dataframe with demographic columns or individual parameters.
#'
#' @param data Optional dataframe with columns: race, gender, handedness
#' @param race Character string specifying race. Accepts "Default", "caucasian", etc.
#' @param gender Character string specifying gender. Accepts "M", "F", "Male", "Female"
#' @param handedness Character string specifying handedness. Accepts "R", "L", "Right", "Left"
#'
#' @return A dataframe or list with normalized values
#'
#' @export

normalize_demographics <- function(data = NULL, race = NA_character_, gender = NA_character_, handedness = NA_character_) {

  # If data is provided, extract from dataframe
  if (!is.null(data)) {
    if ("race" %in% names(data)) race <- data$race[1]
    if ("gender" %in% names(data)) gender <- data$gender[1]
    if ("handedness" %in% names(data)) handedness <- data$handedness[1]
  }

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

  result <- list(
    race = race,
    gender = gender,
    handedness = handedness
  )

  # If data was provided, return a dataframe, otherwise return a list
  if (!is.null(data)) {
    data$race <- result$race
    data$gender <- result$gender
    data$handedness <- result$handedness
    return(data)
  } else {
    return(result)
  }
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

#' Account for Repeated Protocol Runs
#'
#' Identifies when a participant completed the same protocol multiple times
#' within a session and assigns run numbers accordingly.
#'
#' @param data A dataframe with extracted data
#'
#' @return Dataframe with new "run" column indicating which run of each protocol
#'
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
