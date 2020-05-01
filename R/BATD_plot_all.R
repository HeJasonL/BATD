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

    #identify the number of unique sessions
    uniqueSessions <- unique(data$session)
    for(s in sessions){
      sessionData <- data[data$session==uniqueSessions[s],]

      #identify the number of unique runs
      uniqueRuns <- unique(sessionData$run)
      for(r in 1:length(uniqueRuns)){
        sessionData_for_given_run <- sessionData[sessionData$run==uniqueRuns[r],]
        print(paste0("Now plotting participant:", uniqueParticipants[x], " session: ", s, " run: ", r))
        BATD_plot(sessionData_for_given_run)
      }

    }

  }
}
