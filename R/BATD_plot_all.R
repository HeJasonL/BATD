#' BATD_plot_all
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

BATD_plot_all <- function(extracted_Data){

  uniqueParticipants <- unique(extracted_Data$id)
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)]
  participants_outPut_list <- list()

  for(x in 1:length(uniqueParticipants)){

    data <- extracted_Data[extracted_Data$id==uniqueParticipants[x],]
    protocolsCompleted <- as.character(unique(data$protocolName))
    print(paste0("Now plotting participant:", uniqueParticipants[x]))
    BATD_plot(data)

  }


}



