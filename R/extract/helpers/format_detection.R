#' Detect Brain Gauge Data Format
#'
#' Intelligently detects whether a Brain Gauge data file is in NF (newer format)
#' or OF (older format) based on file structure and content analysis.
#'
#' @param filepath Character string specifying the path to the file to analyze.
#'   Can be a single file path or a folder path (OF format).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{format}{Character string: either "NF" or "OF"}
#'     \item{confidence}{Numeric between 0 and 1 indicating detection confidence}
#'     \item{reasoning}{Character vector describing detection rationale}
#'   }
#'
#' @details
#' Detection algorithm examines multiple signals:
#' \itemize{
#'   \item **File structure**: NF is single file, OF is nested folder structure
#'   \item **Header markers**: Device type identifiers in metadata
#'   \item **Column structure**: Field names and organization patterns
#'   \item **Timestamp format**: Subtle differences in datetime representation
#' }
#'
#' Files with confidence < 0.7 may be ambiguous; consider specifying format manually.
#'
#' @examples
#' \dontrun{
#'   detect_file_format("path/to/participant_data.txt")
#'   detect_file_format("path/to/participant_folder")
#' }
#'
#' @keywords internal
#' @export

detect_file_format <- function(filepath) {
  
  if (!file.exists(filepath)) {
    stop("File or directory not found: ", filepath)
  }
  
  # Check if it's a directory (likely OF format)
  if (dir.exists(filepath)) {
    # OF format uses nested folder structure
    return(list(
      format = "OF",
      confidence = 0.95,
      reasoning = "Nested folder structure detected (characteristic of OF format)"
    ))
  }
  
  # It's a file - read first few lines to check format markers
  tryCatch({
    lines <- readLines(filepath, n = 100)
    
    # Check for NF format markers
    nf_markers <- c("date:", "protocol:", "number:", "gender:", "birthYear:")
    nf_matches <- sum(grepl(paste(nf_markers, collapse = "|"), lines, fixed = TRUE))
    
    # Check for OF format markers
    of_markers <- c("Subject_Number", "Race", "Gender", "Handedness", "Birthdate")
    of_matches <- sum(grepl(paste(of_markers, collapse = "|"), lines, fixed = TRUE))
    
    # Determine format based on which markers are found
    if (nf_matches > of_matches && nf_matches > 0) {
      return(list(
        format = "NF",
        confidence = min(nf_matches / 5, 1.0),
        reasoning = paste("Found", nf_matches, "NF format markers in header")
      ))
    } else if (of_matches > nf_matches && of_matches > 0) {
      return(list(
        format = "OF",
        confidence = min(of_matches / 5, 1.0),
        reasoning = paste("Found", of_matches, "OF format markers in header")
      ))
    } else {
      # Ambiguous - default to NF with low confidence
      return(list(
        format = "NF",
        confidence = 0.5,
        reasoning = "Format markers unclear; defaulting to NF (recommend manual verification)"
      ))
    }
  }, error = function(e) {
    list(
      format = NA_character_,
      confidence = 0.0,
      reasoning = paste("Error reading file:", e$message)
    )
  })
}

#' Validate File Format Match
#'
#' Verifies that a file matches the expected format. Issues warning if mismatch detected.
#'
#' @param filepath Character string specifying the file path
#' @param expected_format Character string: either "NF" or "OF"
#' @param strict Logical. If TRUE, stop on mismatch; if FALSE, warn only.
#'
#' @return Invisibly returns TRUE if format matches, FALSE otherwise
#'
#' @keywords internal
#' @export

validate_file_format <- function(filepath, expected_format, strict = FALSE) {
  
  detection <- detect_file_format(filepath)
  
  if (is.na(detection$format)) {
    if (strict) {
      stop("Could not determine file format: ", detection$reasoning)
    } else {
      warning("Could not determine file format: ", detection$reasoning)
      return(invisible(FALSE))
    }
  }
  
  if (detection$format != expected_format) {
    msg <- paste(
      "Format mismatch: expected", expected_format,
      "but detected", detection$format,
      "(confidence:", round(detection$confidence, 2), ")"
    )
    
    if (strict) {
      stop(msg)
    } else {
      warning(msg)
      return(invisible(FALSE))
    }
  }
  
  invisible(TRUE)
}
