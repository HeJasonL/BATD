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

  ##VERSION ----
  Version <- c("BATD_V.1.5")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    print("Note: Debugging on")
    dataframe <- ARBA1
    # dataframe <- dataframe[dataframe$id=="pond-0440",]
    # dataframe <- dataframe[dataframe$session==1,]
    # print(paste("now analyzing:", dataframe$id[1]))
  }

  '%ni%' <- Negate('%in%') #create the function for %not in%
  library(dplyr) #for some reason I can't call 'lead' or 'lag' without reading in the dplyr library

  ## SECTION 1 (setup) ----
  dataframe <- dataframe #redundant code but useful for debugging (ignore)
  unique_number_of_runs <- unique(dataframe$run) #identify the unique number of runs completed

  list_of_protocols_by_run <- list()
  for(r in unique_number_of_runs){
    #Subset the data by the run and analyze the data

    data <- dataframe[dataframe$run==r,] #Subset to the current run
    protocolsCompleted <- as.character(unique(data$protocolName)) #identify the number of protocols completed
    protocolsCompleted <- protocolsCompleted[!is.na(protocolsCompleted)] #legacy: remove any NAs (haven't tested without this line yet)

    ## SECTION 2 (extract the participant and protocol details) ----

    #Extract participant details
    id <- as.character(data$id[1])
    race <- as.character(data$race[1])
    gender <- as.character(data$gender[1])
    handedness <- as.character(data$handedness[1])
    birthYear <- as.character(data$birthYear[1])

    participant_details <- cbind(id, race, gender, handedness, birthYear)

    #Extract protocol details
    dateTested <- as.character(dataframe$date[1])
    extractedBy <- as.character(dataframe$extractedBy[1])
    run <- r

    protocolDetails <- cbind(dateTested, extractedBy, run)

    ## SECTION 2 (Analyze each of the protocols completed identified in SECTION 1)----

    analyzed_protocols_list <- list()  # Create an external list to put the analyzed protocols

    for(p in 1:length(protocolsCompleted)){  #For loops which subsets into the nth protocol completed for a given session

      #Basic cleaning of dataframe ----
      protocol <- protocolsCompleted[p] #state the current protocol (legacy: haven't tried running without this yet)
      protocolData <- data[data$protocolName==protocolsCompleted[p],] #Subset to relevant protocol
      if(nrow(data)==0){next} #(legacy)

      sessionData <- protocolData

      #Change performance column values to numeric ----
      sessionData <- sessionData[!is.na(sessionData$trialNumber),] #remove any trials without a trial number (legacy)
      sessionData$responseTime <- as.numeric(as.character(sessionData$responseTime)) #turn responseTime to numeric
      sessionData$correctResponse <- as.numeric(as.character(sessionData$correctResponse)) #turn correctResponse to numeric
      sessionData$value <- as.numeric(as.character(sessionData$value)) #turn string variables into numeric

      numberofPracticeTrials <- as.numeric(as.character(sessionData$numberofPracticeTrials[1])) + 1 #adding one so that the trials start AFTER the n of practice trials

      #here we remove the practice trials if the n > 10, this is because some sites actually ran a whole protocol as a practice, rather than the first n-numnber of trials (usually 3)
      #If practice trials were ran as a whole protocol, thye are just treated as a protocol
      if(numberofPracticeTrials < 9){
        sessionData <- sessionData[numberofPracticeTrials:nrow(sessionData),] #remove practice trials
        sessionData$trialNumber <- 1:nrow(sessionData) #reset trial numbers
      }

      sessionData_for_thresholds <- sessionData
      sessionData <- sessionData[!is.na(sessionData$response),] #some protocols had a last trial where a response was not made, this response is not included (CONSIDER MAKING THIS BASED ON SITE)

      #General variables ----
      medianRT <- median(sessionData$responseTime, na.rm = TRUE)
      sdRT <- sd(sessionData$responseTime, na.rm = TRUE)
      accuracy <- (sum(sessionData$correctResponse, na.rm = TRUE)/nrow(sessionData))*100

      #Reversals ----
      a <- sessionData$value
      b <- lag(sessionData$value,1)
      reversals <- as.data.frame(cbind(b,a))
      colnames(reversals) <- c("valueprior", "value")
      reversals$valuediff <- reversals$value-reversals$valueprior #value difference
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
        #Estimate Threshold
      if(protocol %ni% c("Simple Reaction Time","Choice Reaction Time")){
        sessionData <- sessionData_for_thresholds
        threshold <- mean(sessionData$value[(nrow(sessionData)-4):(nrow(sessionData))])
      }
      #Estimate Threshold for dynamic detection threshold (done differently)
      if(protocol %in% c("Dynamic Detection Threshold")){
        sessionData <- sessionData_for_thresholds
        threshold <- mean(sessionData$value[sessionData$correctResponse==1])
      }
      if(protocol %in% c("Simultaneous Amplitude Discrimination",
                         "Sequential Amplitude Discrimination",
                         "Dual Staircase Amplitude Discrimination (up)",
                         "Dual Staircase Amplitude Discrimination (down)")){
        threshold <- threshold - 100 #remove the standard stimulus from threshold
      }
      if(protocol %in% c("Duration Discrimination")){
        threshold <- threshold - 500
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
                                                                            ifelse(protocol=="Dual Staircase Amplitude Discrimination (up)", "_ADTdsa_up",
                                                                               ifelse(protocol=="Dual Staircase Amplitude Discrimination (down)", "_ADTdsa_down",
                                                                                  ifelse(protocol=="Simultaneous Frequency Discrimination", "_SMFD",
                                                                                    ifelse(protocol=="Sequential Frequency Discrimination", "_SQFD",
                                                                                          ifelse(protocol=="Simultaneous Amplitude Discrimination", "_SMAD",
                                                                                                 ifelse(protocol=="Sequential Amplitude Discrimination", "_SQAD",
                                                                                                        ifelse(protocol=="Temporal Order Judgement", "_TOJ",
                                                                                                               ifelse(protocol=="Temporal Order Judgement with Carrier", "_TOJwc",
                                                                                                                      ifelse(protocol=="Duration Discrimination", "_DD", NA))))))))))))))))))
      colnames(outPut) <- paste0(colnames(outPut), tag)
      analyzed_protocols_list[[p]] <- outPut
    }

    participant_output <- do.call(cbind, analyzed_protocols_list)

    #Convert the analyzed data to numeric ----
    participant_output <- suppressWarnings(as.data.frame(participant_output))
    participant_output[,1:ncol(participant_output)] <- suppressWarnings(sapply(participant_output[,1:ncol(participant_output)], suppressWarnings(as.character)))
    participant_output[,1:ncol(participant_output)] <- suppressWarnings(sapply(participant_output[,1:ncol(participant_output)], suppressWarnings(as.numeric)))

    #Append additional details ----
    participant_output <- cbind(participant_details, protocolDetails, participant_output)
    list_of_protocols_by_run[[r]] <- participant_output
  }

  participant_output <- plyr::rbind.fill(list_of_protocols_by_run)
  participant_output


    return(participant_output)
}
