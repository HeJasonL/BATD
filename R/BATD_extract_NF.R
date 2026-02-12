#' BATD_extract_NF
#'
#' Takes a list of filenames in a working directory which contain the performance data recorded from Cortical Metrics. These performance files are typically .txt files. "BATD_extract_NF" was specifically designed to analyze data from Brain Gauge devices.
#' For more information about Cortical Metrics Brain Gauage, see: https://www.corticalmetrics.com/braingauge. BATD_extract_NF requires two inputs, with the first being the list of participants and the second being a string variable referring to the site where the data was collected.
#' While both "BATD_extract_NF" and "BATD_extract_OF" return a combined dataframe for all the participants that have had their data extracted, the individual and combined files are saved in folders created in the original working directory.
#' In the working directory, folders 'output' and 'combined' will be created to store the individual files that were extracted and a file containing all the individual files combined respectively. These files are in .csv format.
#'
#' @param list_of_filenames a list object containing the filenames of the .txt files containing participant performance from Brain Gauge
#' @param site refers to the site where the data was collected. In the past and in the future, the original codes will be patched for different sites. For Site, put the site location inbetween two quotation marks. For e.g., "JHU" refers to the Johns Hopkins University
#'
#' @return BATD_extract_NF returns a dataframe containing the extracted data for each participant. Protocols and protocol names are provided as columns, along with other relevant metrics.
#'
#' @seealso [BATD_extract()] for recommended usage with automatic format detection
#'
#' @examples
#' currently NA
#'
#' @export

BATD_extract_NF <- function(list_of_filenames, site){

  #BATD Version:
  Version <- c("BATD_V.1.7")

  #debugging is used to check what within the BATD_extract_NF function is not working
  #if debugging is set to "on", then you will be able to use whatever data/site arguments you want

  debugging <- "off"
  if(debugging=="on"){
    list_of_filenames <- files[2]
    site <- "UCLA" #specify the site at which this data was collected (make it "NA" if unsure)
    #p <- 4
  }

  # Setup
  inputDirectory <- getwd() #get the current wd

  #WARNING: This if statement below needs to be commented out when debugging
  if(hasArg(site) == FALSE){
    site <- "NA"
  }

  #Entering the outer loop
  allParticipantsOutput <- list() #create a list for the output for each participant
  for(p in 1:length(list_of_filenames)){  #For loop through the participants identified in the inputDirectory

    setwd(inputDirectory) #set working directory to where all the Brain Gauge output files are saved
    output <- read.csv(list_of_filenames[p], header = FALSE) #read in the current participant's file (based on 'p')
    print(paste('now extracting details from file:', list_of_filenames[p]))

# General data cleaning prior to extraction
  output$V1 <- as.character(output$V1) #turn column 1 into a character
  output$V1 <- gsub(" ","",output$V1) #replace all blank spaces in column 1
  tempo <- suppressWarnings(t(as.data.frame(strsplit(output$V1,":")))) #split the string by ':' and transpose it to long format
  rownames(tempo) <- c() #clear the row numbers
  cleaned_output <- suppressWarnings(as.data.frame(tempo)) #turn the output into a dataframe

#Determine the number of sessions and create a dataframe which keeps track of when participants started and ended their session
  #identify all the date and time stamps
  date_and_times <- cleaned_output[cleaned_output$V1=="date",] #look at all the unique date/time stamps
  date <- stringr::str_sub(as.character(date_and_times$V2),1,10) #format date
  hours <- stringr::str_sub(date_and_times$V2, 12, 13) #format time - hours
  minutes <- date_and_times$V3 #format time - minutes
  seconds <- stringr::str_sub(date_and_times$V4, 1, 2)
  time <- paste0(hours, ":", minutes, ":", seconds) #format time - hours and minutes
  date_times <- paste(date, time) #format date-time

  #Clean up the date_and_times dataframe
  date_and_times$V1 <- cleaned_output[cleaned_output$V1=="protocol",]$V2 #turn V1 into the protocol number column
  date_and_times$V2 <- stringr::str_sub(as.character(date_and_times$V2),1,10) #Turn V2 into the date_started column
  date_and_times$V3 <- time #turn V3 into the time started column
  date_and_times$V4 <- as.POSIXct(date_times,format="%Y-%m-%d %H:%M:%S") #Turn V4 into date and time (used to sort by date AND time)
  colnames(date_and_times) <- c("protocol", "date", "time", "date_and_time") #rename column names
  date_and_times <- date_and_times[order(date_and_times$date_and_time),] #sort in order of ascending date and times
  rownames(date_and_times) <- c() #clear the row numbers
  date_and_times <- date_and_times[1:4]
  date_and_times$time_difference <- date_and_times$date_and_time - dplyr::lag(date_and_times$date_and_time) #create a time difference column
  date_and_times$tag <- ifelse(abs(date_and_times$time_difference) > 1000, "start", NA)
  date_and_times$tag[1] <- "start"

  #For loop through the date_and_times and append session to them
  number_of_sessions <- sum(date_and_times$tag=="start", na.rm = TRUE) #identify the number of sessions
  row_where_session_starts <- grep("start", date_and_times$tag)
  start <- 1
  session_list <- list()
  for(s in 1:number_of_sessions){
    #Conditional IF accounting for the fact that the last start will not end at the start of the next start (since there won't be one)
    if(s < number_of_sessions){
      data <- date_and_times[row_where_session_starts[s]: (row_where_session_starts[s+1]-1),]
    }else{
      data <- date_and_times[row_where_session_starts[s]: nrow(date_and_times),]
    }
    data$session <- s #add in session value
    session_list[[s]] <- data #store in list
    start <- start + nrow(data) #update start row
  }

  date_and_times_dataframe <- dplyr::bind_rows(session_list)

  if(debugging == "on"){
    print("Session identification completed")
  }

# Divide data
  cleaned_output$V5 <- ifelse(cleaned_output$V1=="date", "X",NA) #Mark date rows with 'x'
  protocols <- as.list(which(cleaned_output$V5=="X")) #Identify the row numbers in which X is marked
  list_of_protocols <- list() #Create a list external of the for loop for breaking the output into seperate protocols
  for (i in (1:length(protocols))){
    if(i==length(protocols)){break} #break the loop once you are at the last protocol
    list_of_protocols[[i]] <-  cleaned_output[(paste(protocols[i])):(paste(protocols[i+1])),] #subset from start of a protocol to the start of the next protocol
  }
  list_of_protocols[[i]] <- cleaned_output[(paste(protocols[i])):nrow(cleaned_output),] #puts the last protocol into the list (for loop above cannot account for last protocol)

  if(debugging == "on"){
    print("Protocol division completed")
  }

# Extract data
  protocol_output_list <- list()

  for (i in (1:length(list_of_protocols))){

    current_protocol_for_current_participant <- list_of_protocols[[i]] #create a dataframe with the current protocol for the current participant
    rownames(current_protocol_for_current_participant) <- c() #clear all the row numbers for convenience

    #Extract participant details - demographics normalized via shared helper
    participant_data <- data.frame(
      id = as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="number"])[1],
      race = as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="race"])[1],
      gender = as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="gender"])[1],
      handedness = as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="handedness"])[1],
      birthYear = as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="birthYear"])[1],
      stringsAsFactors = FALSE
    )
    participant_data <- normalize_demographics(participant_data)
    participant_details <- participant_data[, c("id", "race", "gender", "handedness", "birthYear")]

    #Extract session details
    timeProtocolStarted <-  paste0(substr(gsub("T","",(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"][1])), 11,12),":", substr(gsub("T","",(current_protocol_for_current_participant$V3[current_protocol_for_current_participant$V1=="date"][1])), 1,2)) #what time did the current protocol start

    date <- stringr::str_sub(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"],1,10)[1] #format date
    hours <- stringr::str_sub(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"], 12, 14)[1] #format time - hours
    minutes <- current_protocol_for_current_participant$V3[current_protocol_for_current_participant$V1=="date"][1] #format time - minutes
    seconds <- current_protocol_for_current_participant$V4[current_protocol_for_current_participant$V1=="date"][1]
    time <- paste0(hours,":",minutes,":", seconds)[1] #format time - hours and minutes
    date_times <- paste(date, time)[1] #format date-time
    date_times_formatted <- sort(as.POSIXct(date_times,format="%Y-%m-%d %H:%M:%S"))[1] #convert to POSIXct and sort in ascending order
    session <- date_and_times_dataframe$session[date_and_times_dataframe$date_and_time==date_times_formatted][1] #extract the session number on data_and_times_dataframe for this protocol
    session_details <- cbind(date, time, site, session)

    #Extract protocol details
    protocol <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="protocol"]) #protocol number
    numberofPracticeTrials <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="numTrainingTrials"][1]) #number of practice trials
    numberofTestTrials <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="numTrials"][1]) #number of test trials
    ISI <- suppressWarnings(as.numeric(as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="ITI"][1]))) #interval between stimuli (interstimlus interval)
    interval_between_adaptive_and_test <- suppressWarnings(as.numeric(as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="intervalBetweenAdaptAndTest"][1]))) #interval between stimuli (interstimlus interval)

    protocol_details <- cbind(protocol, numberofPracticeTrials, numberofTestTrials, ISI, interval_between_adaptive_and_test)

    #Extract stimulus details
    stim1amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[1]
    stim1frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[1]
    stim1duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[1]

    stim2amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[2]
    stim2frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[2]
    stim2duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[2]

    astim1amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[3]
    astim1frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[3]
    astim1duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[3]

    astim2amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[3]
    astim2frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[3]
    astim2duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[3]

    stimulus_characteristics <- cbind(stim1amplitude, stim1frequency, stim1duration,
                                        stim2amplitude, stim2frequency, stim2duration,
                                        astim1amplitude, astim1frequency, astim1duration,
                                        astim2amplitude, astim2frequency, astim2duration)

    #Extract performance details
    trialNumber <- 1:length(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="value"])
    value <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="value"])
    expected <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="expected"])
    response <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="response"])
    correctResponse <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="correct"])
    responseTime <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="responseTime"])

    performance_details <- cbind(trialNumber, value, expected, response, correctResponse, responseTime)

    #Store miscellaneous information
    originalFilename <- list_of_filenames[p]
    format <- "NF"
    extractedBy <- Version

    miscellaneous_information <- cbind(originalFilename, format, extractedBy)

    #Combine all details
    all_details <- cbind(participant_details,
                         session_details,
                         protocol_details,
                         stimulus_characteristics,
                         miscellaneous_information)

    #repeat all_details to match performance_details row count
    all_details <- all_details[rep(seq_len(nrow(all_details)), each = nrow(performance_details)), ]
    all_extracted_data <- as.data.frame(cbind(all_details, performance_details))
    protocol_output_list[[i]] <- all_extracted_data
  }

  if(debugging == "on"){
    print("Data extraction completed")
  }

  #Combine extracted protocol details
  participantTactileData <- do.call(rbind.data.frame, protocol_output_list)

  #Sort by date and time
  participantTactileData$date_times_formatted <- paste(participantTactileData$date, participantTactileData$time)
  participantTactileData$date_times_formatted <- as.POSIXct(participantTactileData$date_times_formatted,format="%Y-%m-%d %H:%M:%S")
  participantTactileData <- participantTactileData[order(participantTactileData$date_times_formatted),]

  #Standardize correctResponse to 0/1
  participantTactileData$correctResponse  <- as.character(participantTactileData$correctResponse)
  participantTactileData$correctResponse[participantTactileData$correctResponse=="true"] <- "1"
  participantTactileData$correctResponse[participantTactileData$correctResponse=="false"] <- "0"

    if(debugging == "on"){
      print("Protocol combination completed")
    }

# Label Protocols
    sites_with_labels <- c(
      "KKI", "CCH", "ARBA1", "ARBA2", "ARBA3", "ARBA4",
      "SPIN", "CARE", "STES", "EU-AIMS", "UCLA"
    )

    if(site %ni% sites_with_labels){
      print("Warning: The 'site' argument provided is not a known site to the function, please contact Jason He : jasonhe93@gmail.com")
    }

    #Construct URL based on site
    url <- NULL
    if(site %in% c("KKI", "CCH", "SPIN", "CARE", "STES", "EU-AIMS", "UCLA")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", site, "_protocol_names.csv")
    } else if(site %in% c("ARBA1", "ARBA2", "ARBA3", "ARBA4")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/ARBA/ARBA_protocol_names.csv")
    } else if(site == "NA"){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/NA/NA_protocol_names.csv")
    }

    protocol_names <- read.csv(url, header = TRUE)

    protocols_completed <- unique(participantTactileData$protocol)
    list_of_labelled_protocols <- list()

    for(pc in protocols_completed){
      data_for_current_protocol_number <- participantTactileData[participantTactileData$protocol==pc,]
      data_for_current_protocol_number$protocolName <- protocol_names$protocol_string_label[protocol_names$protocol_number==pc][1]
      list_of_labelled_protocols[[pc]] <- data_for_current_protocol_number
    }

    #recombine the labelled protocol data
    participantTactileData <- dplyr::bind_rows(list_of_labelled_protocols)

    #Site-specific adjustments
    if(site == "KKI"){
      participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim2amplitude==200] <- "Amplitude Discrimination"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100 & participantTactileData$astim1amplitude==0] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==100] <- "Static Detection Threshold with Adaptation ISI 100"
    }

    if(site == "CCH"){
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==100] <- "Static Detection Threshold with Adaptation ISI 100"
    }

    if(site %in% c("ARBA1", "ARBA2", "ARBA3", "ARBA4")){
      participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
      participantTactileData$protocolName[participantTactileData$protocol==100 &  participantTactileData$stim1amplitude==100] <- "Sequential Amplitude Discrimination"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==0] <- "Dual Staircase Amplitude Discrimination (up)"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==200] <- "Dual Staircase Amplitude Discrimination (down)"
    }

    if(site %in% c("SPIN")){
      participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
      participantTactileData$protocolName[participantTactileData$protocol==100 &  participantTactileData$stim1amplitude==100] <- "Simultaneous Amplitude Discrimination"
      participantTactileData$protocolName[participantTactileData$protocol==171] <- "Dual Staircase Amplitude Discrimination (down)"
    }

    if(site %in% c("CARE")){
      participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
      participantTactileData$protocolName[participantTactileData$protocolName=="TBD"] <- "Simultaneous Amplitude Discrimination"
    }

    if(site %in% c("STES")){
      participantTactileData$protocolName[participantTactileData$protocol==713 & participantTactileData$stim2amplitude==30] <- "Dynamic Detection Threshold (down)"
    }

    if(site == "EU-AIMS"){
      if(713 %in% unique(participantTactileData$protocol) & length(participantTactileData$protocolName[participantTactileData$protocol==713]) > 14){
        participantTactileData$protocolName[participantTactileData$protocol==713] <- c(rep("Dynamic Detection Threshold (up)", 14), rep("Dynamic Detection Threshold (down)", 14))
      }
      participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim2amplitude==200] <- "Amplitude Discrimination Threshold without Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100 & participantTactileData$astim1amplitude==0] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==100] <- "Static Detection Threshold with Adaptation ISI 100"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==0] <- "Dual Staircase Amplitude Discrimination (up)"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==200] <- "Dual Staircase Amplitude Discrimination (down)"
    }

    if(site == "UCLA"){
      if(713 %in% unique(participantTactileData$protocol) & length(participantTactileData$protocolName[participantTactileData$protocol==713]) > 14){
        participantTactileData$protocolName[participantTactileData$protocol==713] <- c(rep("Dynamic Detection Threshold (up)", 14), rep("Dynamic Detection Threshold (down)", 14))
      }

      if("Simultaneous Amplitude Discrimination followed by adaptation" %in% unique(participantTactileData$protocolName)){
        temp <- participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",]
        trial_number_ends <- temp$trialNumber[temp$response=="null"]

        first_protocol <- participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",][participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",]$trialNumber %in% c(1:trial_number_ends[1]),]
        second_protocol <- participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",][!participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",]$trialNumber %in% c(1:trial_number_ends[1]),]

        first_protocol$protocolName <- "Simultaneous Amplitude Discrimination"
        second_protocol$protocolName <- "Simultaneous Amplitude Discrimination with adaptation"

        participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",][participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",]$trialNumber %in% c(1:trial_number_ends[1]),] <- first_protocol
        participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",][!participantTactileData[participantTactileData$protocolName=="Simultaneous Amplitude Discrimination followed by adaptation",]$trialNumber %in% c(1:trial_number_ends[1]),] <- second_protocol
      }
    }

    if(site == "NA"){
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100 & participantTactileData$astim1amplitude==0] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==100] <- "Static Detection Threshold with Adaptation ISI 100"
    }

    if(debugging == "on"){
      print("Protocol labeling completed")
    }

    #Store the protocol outputs in the allParticipantsOutput list
    allParticipantsOutput[[p]] <- as.data.frame(participantTactileData)
  }

  if(debugging == "on"){
    print("All participants processed")
  }

  allParticipantsOutput_combined <-  as.data.frame(data.table::rbindlist(allParticipantsOutput, fill = TRUE))

# Account for Runs - using shared helper function
  allParticipantsOutput_combined <- account_for_repeated_runs(allParticipantsOutput_combined)

  if(debugging == "on"){
    print("Run accounting completed")
  }

# Column Formatting - using shared helper function
  allParticipantsOutput_combined <- standardize_column_types(allParticipantsOutput_combined)

  if(debugging == "on"){
    print("Column formatting completed")
  }

# Final setup
  allParticipantsOutput_combined$Site <- site

  if(debugging == "on"){
    print("Final setup completed")
  }

  print("All participant data extracted succesfully!")

  return(allParticipantsOutput_combined)
}
