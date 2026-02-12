#' BATD_plot (Wrapper)
#'
#' Plot extracted data for from BATD_extract_NF or BATD_extract_OF for a single participant. This function only requires a dataframe containing the performance output of participants who have had their data extracted using either BATD_extract_NF or BATD_extract_OF.
#' BATD_plot is used to analyze the datafrom a single participant. In order to analyze data from multiple participants, we recommend users use BATD_plot_all.
#'
#' @param data referring to the dataframe extracted from BATD_extract_NF and BATD_extract_OF
#'
#' @return BATD plot does not return any plots in the R environment. Instead, BATD_plot creates a folder called plot in the users working directory, and saves a series of plots with the filename pattern of id_session_X.pdf, with id being the participant id and X being the session.
#'
#' @examples
#' Examples are currently NA
#'
#' @export

# Source the refactored implementation
source(system.file("R/plot/plot.R", package = "BATD"), local = TRUE)
