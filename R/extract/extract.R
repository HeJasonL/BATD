#' Unified Brain Gauge Data Extraction
#'
#' Intelligently extracts data from Brain Gauge files in either NF (newer format)
#' or OF (older format). Auto-detects the format and routes to the appropriate
#' extraction function. This is the recommended entry point for most users.
#'
#' @param list_of_filenames A character vector of file or folder paths to extract.
#'   Can be a mix of NF files (.txt) and OF folder structures.
#' @param site Character string specifying the site where data was collected
#'   (e.g., "JHU", "UCLA", "KKI"). Used for protocol name mapping.
#' @param format Character string specifying expected format: "auto" (default) for
#'   automatic detection, or explicitly "NF" or "OF". Use manual specification if
#'   auto-detection is unreliable.
#'
#' @return A dataframe containing extracted trial-level data with columns for:
#'   - Participant details (id, race, gender, handedness, birthYear)
#'   - Session info (date, time, session, run)
#'   - Protocol info (protocol, protocolName, numberofPracticeTrials, etc.)
#'   - Stimulus characteristics (stim1amplitude, stim2amplitude, etc.)
#'   - Performance metrics (value, expected, response, correctResponse, responseTime)
#'   - Extraction metadata (format, extractedBy, Site)
#'
#' @details
#' The function automatically detects file format using `detect_file_format()`:
#' - **NF format**: Single .txt file per participant with Brain Gauge device data
#' - **OF format**: Nested folder structure for Brain Gauge Research Edition data
#'
#' For files with ambiguous format (confidence < 0.7), the function will warn and
#' use the detected format. To override, specify `format` parameter explicitly.
#'
#' All output is standardized to common dataframe structure regardless of source format.
#'
#' @examples
#' \dontrun{
#'   # Auto-detect format (recommended)
#'   data <- BATD_extract(
#'     list.files(pattern = "\\.txt$"),
#'     site = "UCLA"
#'   )
#'
#'   # Explicitly specify format if auto-detection fails
#'   data <- BATD_extract(
#'     list.files(pattern = "\\.txt$"),
#'     site = "JHU",
#'     format = "NF"
#'   )
#'
#'   # Extract from old format folders
#'   data <- BATD_extract(
#'     "path/to/participant_folder",
#'     site = "KKI",
#'     format = "OF"
#'   )
#' }
#'
#' @seealso
#'   [BATD_extract_NF()] for NF-specific extraction with fine-grained control
#'   [BATD_extract_OF()] for OF-specific extraction with fine-grained control
#'   [detect_file_format()] for format detection details
#'
#' @export

BATD_extract <- function(list_of_filenames, site, format = "auto") {
  
  # Validate inputs
  if (missing(site)) {
    stop("Argument 'site' is required. Specify the site where data was collected ",
         "(e.g., 'JHU', 'UCLA', 'KKI')")
  }
  
  if (!is.character(list_of_filenames)) {
    stop("list_of_filenames must be a character vector")
  }
  
  format <- match.arg(format, choices = c("auto", "NF", "OF"))
  
  # If explicit format specified, use that for all files
  if (format != "auto") {
    if (format == "NF") {
      return(BATD_extract_NF(list_of_filenames, site))
    } else {
      return(BATD_extract_OF(list_of_filenames, site))
    }
  }
  
  # Auto-detect format for each file
  detected_formats <- sapply(list_of_filenames, function(filepath) {
    detection <- detect_file_format(filepath)
    if (detection$confidence < 0.7) {
      warning(
        "Low confidence format detection for ", filepath, ": ",
        detection$reasoning, ". Defaulting to ", detection$format
      )
    }
    detection$format
  })
  
  # Check if all files are same format
  unique_formats <- unique(detected_formats)
  
  if (length(unique_formats) == 1) {
    # All files same format - route to appropriate function
    if (unique_formats[1] == "NF") {
      return(BATD_extract_NF(list_of_filenames, site))
    } else {
      return(BATD_extract_OF(list_of_filenames, site))
    }
  } else {
    # Mixed formats - need to process separately and combine
    message("Mixed formats detected. Processing each format separately...")
    
    nf_files <- list_of_filenames[detected_formats == "NF"]
    of_files <- list_of_filenames[detected_formats == "OF"]
    
    results_list <- list()
    
    if (length(nf_files) > 0) {
      message("Extracting ", length(nf_files), " NF format file(s)...")
      results_list$nf <- BATD_extract_NF(nf_files, site)
    }
    
    if (length(of_files) > 0) {
      message("Extracting ", length(of_files), " OF format file(s)...")
      results_list$of <- BATD_extract_OF(of_files, site)
    }
    
    # Combine results
    combined <- data.table::rbindlist(results_list, fill = TRUE)
    return(as.data.frame(combined))
  }
}
