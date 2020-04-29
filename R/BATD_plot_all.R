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

  ##Version
  Version <- c("BATD_V.1.5")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    print("Note: Debugging on")
    extracted_Data <- ARBA1
  }


  extracted_Data <- extracted_Data[!is.na(extracted_Data$id),]
  uniqueParticipants <- unique(extracted_Data$id)
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)]
  participants_outPut_list <- list()

  for(x in 1:length(uniqueParticipants)){
    data <- extracted_Data[extracted_Data$id==uniqueParticipants[x],]
    protocolsCompleted <- as.character(unique(data$protocolName))
    print(paste0("Now plotting participant:", uniqueParticipants[x]))

    data <- data[data$session==1,]

    BATD_plot(data)
  }

}
