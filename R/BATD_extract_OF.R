#' BATD_extract_OF
#'
#' Takes a list of filenames in a working directory which contain the performance data recorded from Cortical Metrics. These performance files are typically .txt files. 'BATD_extract_OF' was specifically designed to analyze data from the Brain Gauge Research Editions and any version prior.
#' For more information about Cortical Metrics Brain Gauage, see: https://www.corticalmetrics.com/braingaugeresearch. BATD_extract_OF requires two inputs, with the first being the list of participants and the second being a string variable referring to the site where the data was collected.
#' While both 'BATD_extract_NF' and 'BATD_extract_OF' return a combined dataframe for all the participants that have had their data extracted, the individual and combined files are saved in folders created in the original working directory.
#' In the working directory, folders 'output' and 'combined' will be created to store the individual files that were extracted and a file containing all the individual files combined respectively. These files are in .csv format.
#'
#' @param list_of_filenames a list object containing the filenames of the .txt files containing participant performance from Brain Gauge
#' @param Site refers to the site where the data was collected. In the past and in the future, the original codes will be patched for different sites. For Site, put the site location inbetween two quotation marks. For e.g., 'JHU' refers to the Johns Hopkins University
#'
#' @return BATD_extract_OF returns a dataframe containing the extracted data for each participant. Protocols and protocol names are provided as columns, along with other relevant metrics.
#'
#' @seealso [BATD_extract()] for recommended usage with automatic format detection
#'
#' @examples
#' currently NA
#'
#' @export

BATD_extract_OF <- function(list_of_filenames, Site){
  # This is a wrapper function that maintains backward compatibility
  # The actual implementation has been moved to R/extract/extract_of.R
  source(system.file("R/extract/extract_of.R", package = "BATD"), local = TRUE)
  BATD_extract_OF(list_of_filenames, Site)
}
