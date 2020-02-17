#' BATD_analyze
#'
#' Analyze extracted data for from "BATD_extract_NF" or "BATD_extract_OF" for a single participant. This function only requires a dataframe containing the performance output of participants who have had their data extracted using either BATD_extract_NF or BATD_extract_OF.
#' BATD_analyze is used to analyze the datafrom a single participant. In order to analyze data from multiple participants, we recommend users use "BATD_analyze_all".
#'
#' @param dataframe refers to a dataframe produced from "BATD_extract_NF" or "BATD_extract_OF"
#'
#' @return BATD_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
#'
#' @examples
#' Examples are currently NA
#'
#' @export

BATD_analyze <- function(dataframe){

  library(dplyr) #for some reason I can't call 'lead' or 'lag' without reading in the dplyr library
  '%ni%' <- Negate('%in%') #create the function for %not in%

    data <- dataframe
    data <- data[!is.na(data$protocolName),]
    protocolsCompleted <- as.character(unique(data$protocolName))
    protocolsCompleted <- protocolsCompleted[!is.na(protocolsCompleted)]

    #Create external lists -----
    analyzed_protocols_list <- list()

    #For loops which subsets into the nth protocol completed for a given session ----
    for(p in 1:length(protocolsCompleted)){
      #Basic cleaning of dataframe ----
      protocol <- protocolsCompleted[p]
      protocolData <- data[data$protocolName==protocolsCompleted[p],] #Subset to relevant protocol
      sessions <- c(1:unique(protocolData$session)) #identify how many sessions were completed for the nth protocol [might need future adjustment]

      sessions_list <- list()

      for(s in 1:length(sessions)){
        sessionData <- protocolData[protocolData$session == s,]
        sessionData <- sessionData
        sessionData <- sessionData[!is.na(sessionData$trialNumber),]
        sessionData$responseTime <- as.numeric(as.character(sessionData$responseTime)) #turn responseTime to numeric
        sessionData$correctResponse <- as.numeric(as.character(sessionData$correctResponse)) #turn correctResponse to numeric
        sessionData$value <- as.numeric(as.character(sessionData$value)) #turn string variables into numeric

        #General variables ----
        medianRT <- median(sessionData$responseTime, na.rm = TRUE)
        sdRT <- sd(sessionData$responseTime, na.rm = TRUE)
        accuracy <- (sum(sessionData$correctResponse, na.rm = TRUE)/nrow(sessionData))*100

        #Reversals ----
        a <- sessionData$value
        b <- lag(sessionData$value,1)
        reversals <- as.data.frame(cbind(b,a))
        colnames(reversals) <- c("valueprior", "value")
        reversals$valuediff <- reversals$value-reversals$valueprior #value differnece
        reversals$valuediffprior <- lead(reversals$valuediff,1)
        reversals$sign <- sign(reversals$valuediff)
        reversals$signs <- lead(sign(reversals$valuediff),1)
        reversals$signs[reversals$signs==0] <- -1
        reversals$reversals  <-reversals$sign+reversals$signs
        reversals$reversals[reversals$reversals==0] <- "Reversals"
        reversals <- length(which(reversals$reversals=="Reversals"))

        #Key variables ----
        #For the reaction time protocols
        if(protocol %in% c("Simple Reaction Time","Choice Reaction Time")){
          if(sum(sessionData$correctResponse) >= 6){
            correctResponses <- sessionData$responseTime[sessionData$correctResponse==1]
            middle <- length(correctResponses)/2
            median6 <- correctResponses[(middle-3):(middle+2)]
            meanRT <- mean(median6)

          } else {

            meanRT <- NA}}
        #For the tactile threshold protocols
        if(protocol %ni% c("Simple Reaction Time","Choice Reaction Time")){
          threshold <- mean(sessionData$value[(nrow(sessionData)-4):(nrow(sessionData))])
        }
        if(protocol %in% c("Dynamic Detection Threshold")){
          threshold <- mean(sessionData$value[sessionData$correctResponse==1])
        }
        #Column bind variables -----
        if(protocol %in% c("Simple Reaction Time","Choice Reaction Time")){
          outPut <- cbind(accuracy, medianRT, sdRT, meanRT)}else{outPut <- cbind(accuracy, medianRT, sdRT, threshold, reversals)}

        #Add a tag to the end of the column names to specify which protocol the outPut is from ----
        tag <- ifelse(protocol=="Simple Reaction Time", "_SRT",
                      ifelse(protocol=="Choice Reaction Time", "_CRT",
                             ifelse(protocol=="Static Detection Threshold", "_SDT",
                                    ifelse(protocol=="Static Detection Threshold with Adaptation ISI 30", "_SDT30",
                                           ifelse(protocol=="Static Detection Threshold with Adaptation ISI 100", "_SDT100",
                                                  ifelse(protocol=="Dynamic Detection Threshold", "_DDT",
                                                         ifelse(protocol=="Amplitude Discrimination Threshold without Adaptation", "_ADT",
                                                                ifelse(protocol=="Amplitude Discrimination with Single Site Adaptation", "_ADTssa",
                                                                       ifelse(protocol=="Amplitude Discrimination with Dual Site Adaptation", "_ADTdsa",
                                                                              ifelse(protocol=="Simultaneous Frequency Discrimination", "_SMFD",
                                                                                     ifelse(protocol=="Sequential Frequency Discrimination", "_SQFD",
                                                                                            ifelse(protocol=="Simultaneous Amplitude Discrimination", "_SMAD",
                                                                                                   ifelse(protocol=="Sequential Amplitude Discrimination", "_SQAD",
                                                                                                          ifelse(protocol=="Temporal Order Judgement", "_TOJ",
                                                                                                                 ifelse(protocol=="Temporal Order Judgement with Carrier", "_TOJwc",
                                                                                                                        ifelse(protocol=="Duration Discrimination", "_DD", NA))))))))))))))))
        colnames(outPut) <- paste0(colnames(outPut), tag)



        #Store the output for the given session----
        sessions_list[[s]] <- outPut
      }

      sessionsCombined <-  as.data.frame(do.call(rbind, sessions_list)) #rowbind the session(s)
      analyzed_protocols_list[[p]] <- sessionsCombined

    }

    participant_output <- do.call(rowr::cbind.fill, c(analyzed_protocols_list, fill = NA))
    participant_output$session <- 1:nrow(participant_output)

    return(participant_output)
}