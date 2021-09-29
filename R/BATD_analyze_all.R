#' BATD_analyze_all
#'
#' Analyze the extracted data from "BATD_extract_NF" or "BATD_extract_OF" for multiple participants. This function is practically the same as "BATD_analyze", but applies it to multiple participants within a for loop.
#' Over and above "BATD_analyze", "BATD_analyze_all" also appends information about the participant that completed the protocol, as well as parameters of the protocol itself.
#' For this reason, we recommend users to always use BATD_analyze_all, even for single participants.
#'
#' @param dataframe BATD_analyze all requires the dataframe created by BATD_extract
#'
#' @return like BATD_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
#' However, BATD_analyze_all will return a dataframe with multiple rows, referring to each participant in the dataframe entered, separated by sessions if they completed multiple sessions.
#'
#' @examples
#' Examples are currently NA
#'
#' @export

BATD_analyze_all <- function(dataframe) {

  ##Version
  Version <- c("BATD_V.1.6")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    print("Note: Debugging on")
    dataframe <- combined_lurie_data
  }

  dataframe <- dataframe[!is.na(dataframe$protocolName),] #do not run through protocols without names (WARNING)
  uniqueParticipants <- unique(dataframe$id)
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)]

  participants_outPut_list <- list()
  participants_detail_list <- list()

  for (x in 1:length(uniqueParticipants)) {

    # subset by participant ----
    data <- dataframe[dataframe$id == uniqueParticipants[x], ]  #Subset to the nth participant
    data <- data[!is.na(data$id), ]  #remove rows where the id is NA (an old fix that I'm afraid to remove)

    # subset by sessions ----
    sessions <- unique(data$session)  #identify the number of sessions
    sessions_outPut_list <- list()  #create a list to store session data

    for (s in 1:length(sessions)) {
      sessionData <- data[data$session == sessions[s], ]  #Subset to the nth session
      sessionData <- sessionData[!is.na(sessionData$id), ]  #remove rows where the id is NA (an old fix that I'm afraid to remove)

      analyzed_sessions_data <- as.data.frame(BATD_analyze(sessionData)) #Run the data through the BATD_analyze function

      #Append information to the analyzed data
      analyzed_sessions_data$session <- sessions[s]
      analyzed_sessions_data$analyzedBy <- Version

      sessions_outPut_list[[s]] <- analyzed_sessions_data
    }

    # Combine the output from the sessions_outPut_list
    sessionsData_combined <- plyr::rbind.fill(sessions_outPut_list)  #combine the sessions output from sessions_outPut_list
    participants_outPut_list[[x]] <- sessionsData_combined
  }


  all <- plyr::rbind.fill(participants_outPut_list)
  all <- all[!is.na(all$id), ]

  # all <- all[, c(1:7, #id:extractedBy
  #                53, #analyzedBy
  #                52, #session
  #                8, #run
  #                9:51)]


  return(all)

}
