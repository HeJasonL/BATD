#' BATD_plot_all (Wrapper)
#'
#' Plot the extracted data from "BATD_extract_NF" or "BATD_extract_OF" for multiple participants. This function is practically the same as BATD_analyze, but applies it to multiple participants within a for loop.
#' We recommend users to always use "BATD_plot_all", even for single participants.
#'
#' @param x Numeric vector.
#'
#' @return Factor variable.
#'
#' @examples
#'
#' @export

# Source the refactored implementation
source(system.file("R/plot/plot_all.R", package = "BATD"), local = TRUE)
