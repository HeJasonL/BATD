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

BATD_analyze_all <- function(dataframe){

  uniqueParticipants <- unique(dataframe$id)
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)]

  participants_outPut_list <- list()
  participants_detail_list <- list()

  for(x in 1:length(uniqueParticipants)){

    #Analyze performance data
    data <- dataframe[dataframe$id==uniqueParticipants[x],]
    protocolsCompleted <- as.character(unique(data$protocolName))
    participants_outPut_list[[x]] <- as.data.frame(BATD_analyze(data))

    #Extract participant details (not yet integrated) -----
    id <- data$id[1]
    race <- data$race[1]
    gender <- data$gender[1]
    handedness <- data$handedness[1]
    birthYear <- data$birthYear[1]
    participantDetails <- cbind.data.frame(id, race, gender, handedness, birthYear)
    participants_detail_list[[x]] <- participantDetails
  }

  allperformance <- plyr::rbind.fill(participants_outPut_list)
  alldetails <- plyr::rbind.fill(participants_detail_list)

  all <- rowr::cbind.fill(alldetails, allperformance)

  # baseDirectory <- getwd()
  # dir.create("Combined Data", showWarnings = FALSE)
  # setwd(paste0(getwd(),"/Combined Data"))
  # write.csv(all, "Vibrotactile_data_combined.csv")
  # setwd(baseDirectory)

  return(all)

}
