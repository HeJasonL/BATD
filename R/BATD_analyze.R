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

  ##Version
  Version <- c("BATD_V.1.5")

  '%ni%' <- Negate('%in%') #create the function for %not in%
  library(dplyr) #for some reason I can't call 'lead' or 'lag' without reading in the dplyr library

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    print("Note: Debugging on")
    dataframe <- ARBA1[ARBA1$id=="pond-0440",]
    dataframe <- dataframe[dataframe$session==1,]
  }

  #SECTION 1 ----

  data <- dataframe
  data <- data[!is.na(data$protocolName),]
  protocolsCompleted <- as.character(unique(data$protocolName))
  protocolsCompleted <- protocolsCompleted[!is.na(protocolsCompleted)]

  #Identify whether there were any protocols completed more than once ----
  list_of_protocols_completed_with_runs <- list()
  for(p in 1:length(protocolsCompleted)){
    protocolName <- protocolsCompleted[p]
    numberofRuns <- nlevels(as.factor(data$orderCompleted[data$protocolName==protocolsCompleted[p]]))
    ProtocolRuns <- as.data.frame(cbind(protocolName, numberofRuns))

    list_of_protocols_completed_with_runs[[p]] <- ProtocolRuns
  }

  protocols_completed_by_runs <- plyr::rbind.fill(list_of_protocols_completed_with_runs)
  protocols_completed_by_runs$numberofRuns <- as.numeric(as.character(protocols_completed_by_runs$numberofRuns))
  names_of_protocols_completed_more_than_once <- protocols_completed_by_runs$protocolName[protocols_completed_by_runs$numberofRuns > 1]

  #Create a column called run (and give number referring to the run number) ----

  #protocols completed more than once
  if(length(names_of_protocols_completed_more_than_once) > 0){

    list_of_protocols_completed_more_than_once <- list()
    for(p in 1:length(names_of_protocols_completed_more_than_once)){
      protocol_completed_more_than_once <- data[data$protocolName==names_of_protocols_completed_more_than_once[p],]
      number_of_runs <- length(unique(protocol_completed_more_than_once$orderCompleted))
      run_number <- unique(protocol_completed_more_than_once$orderCompleted)

      list_of_runs <- list()
      for(r in 1:number_of_runs){
        currentRun <- protocol_completed_more_than_once[protocol_completed_more_than_once$orderCompleted==run_number[r],]
        currentRun$run <- r
        list_of_runs[[r]] <- currentRun
      }

      protocol_completed_more_than_once <- plyr::rbind.fill(list_of_runs)
      list_of_protocols_completed_more_than_once[[p]] <- protocol_completed_more_than_once
    }

    protocols_completed_more_than_once <- plyr::rbind.fill(list_of_protocols_completed_more_than_once)
  }


  #protocols completed once
  if(length(names_of_protocols_completed_more_than_once) > 0){
    protocol_completed_just_once <- data[data$protocolName!=names_of_protocols_completed_more_than_once[p],]
    protocol_completed_just_once$run <- 1
  } else {
    protocol_completed_just_once <- data
    protocol_completed_just_once$run <- 1
  }



  #Recombine the protocols completed just once back with the protocosl completed more than once ----
  if(length(names_of_protocols_completed_more_than_once) > 0){
  combined_protocol_data <- rbind(protocols_completed_more_than_once, protocol_completed_just_once)
  } else {
    combined_protocol_data <- protocol_completed_just_once
  }

  maximum_number_of_runs <- max(protocols_completed_by_runs$numberofRuns)

  #SECTION 2 ----
  list_of_protocols_by_run <- list()
  for(r in 1:maximum_number_of_runs){
    #Subset the data by the run and analyze the data

    data <- combined_protocol_data[combined_protocol_data$run==r,]
    protocolsCompleted <- as.character(unique(data$protocolName))
    protocolsCompleted <- protocolsCompleted[!is.na(protocolsCompleted)]

    #Extract id
    id <- as.character(data$id[1])

    #Extract participant details
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

    #SECTION 3 ----

    # Create external lists -----
    analyzed_protocols_list <- list()
    #For loops which subsets into the nth protocol completed for a given session ----

    for(p in 1:length(protocolsCompleted)){

      #Basic cleaning of dataframe ----
      protocol <- protocolsCompleted[p]
      protocolData <- data[data$protocolName==protocolsCompleted[p],] #Subset to relevant protocol

      # if(nrow(data)==0){next}

      sessionData <- protocolData

      #Change performance column values to numeric ----
      sessionData <- sessionData[!is.na(sessionData$trialNumber),] #First, remove any rows where there are more trial numbers than you would reasonably expect
      sessionData$responseTime <- as.numeric(as.character(sessionData$responseTime)) #turn responseTime to numeric
      sessionData$correctResponse <- as.numeric(as.character(sessionData$correctResponse)) #turn correctResponse to numeric
      sessionData$value <- as.numeric(as.character(sessionData$value)) #turn string variables into numeric

      numberofPracticeTrials <- as.numeric(as.character(sessionData$numberofPracticeTrials[1])) + 1 #adding one so that the trials start AFTER the n of practice trials

      #here we remove the practice trials if the n > 10, this is because some sites actually ran a whole protocol as a practice, rather than the first n-numnber of trials (usually 3)
      #If prctice trials were ran as a whole protocol, thye are just treated as a protocol
      if(numberofPracticeTrials < 9){
        sessionData <- sessionData[numberofPracticeTrials:nrow(sessionData),] #remove practice trials
        sessionData$trialNumber <- 1:nrow(sessionData) #reset trial numbers
      }

      sessionData_for_thresholds <- sessionData
      sessionData <- sessionData[!is.na(sessionData$response),] #some protocols had a last trial where a response was not made, this response is not included

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
      if(protocol %ni% c("Simple Reaction Time","Choice Reaction Time")){
        sessionData <- sessionData_for_thresholds
        threshold <- mean(sessionData$value[(nrow(sessionData)-4):(nrow(sessionData))])
      }
      if(protocol %in% c("Dynamic Detection Threshold")){
        sessionData <- sessionData_for_thresholds
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



    return(participant_output)
}
