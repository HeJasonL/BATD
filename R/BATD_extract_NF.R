#' BATD_extract_NF
#'
#' Takes a list of filenames in a working directory which contain the performance data recorded from Cortical Metrics. These performance files are typically .txt files. "BATD_extract_NF" was specifically designed to analyze data from Brain Gauge devices.
#' For more information about Cortical Metrics Brain Gauage, see: https://www.corticalmetrics.com/braingauge. BATD_extract_NF requires two inputs, with the first being the list of participants and the second being a string variable referring to the site where the data was collected.
#' While both "BATD_extract_NF" and "BATD_extract_OF" return a combined dataframe for all the participants that have had their data extracted, the individual and combined files are saved in folders created in the original working directory.
#' In the working directory, folders 'output' and 'combined' will be created to store the individual files that were extracted and a file containing all the individual files combined respectively. These files are in .csv format.
#'
#' @param list_of_filenames a list object containing the filenames of the .txt files containing participant performance from Brain Gauge
#' @param Site refers to the site where the data was collected. In the past and in the future, the original codes will be patched for different sites. For Site, put the site location inbetween two quotation marks. For e.g., "JHU" refers to the Johns Hopkins University
#'
#' @return BATD_extract_NF returns a dataframe containing the extracted data for each participant. Protocols and protocol names are provided as columns, along with other relevant metrics.
#'
#' @examples
#' currently NA
#'
#' @export

BATD_extract_NF <- function(list_of_filenames, site){

  #BATD Version:
  Version <- c("BATD_V.1.7")

  #Version 1.6. has made significant adjustments to the code, including:
  # . Streamlining of how sessions were accounted for (previously done in section 2, and based only on date)
  #  . Sessions now take into account date AND time, making it more sensitive. Sessions are counted as separate if they are 1000 seconds apart (~16 minutes )
  #  . This has been achieved by accounting for sessions earlier in the code, and back referencing to a dataframe which contains the date/time stamps of each protocol
  #  . The plan for V.1.7 is to make it so that runs (currently done in section 2) are also integrated into section 1
  #  . This will involve naming the protocols earlier in the code (currently done in section 1.4)
  #  . If this is achieved, section 2 can be removed all together
  # . Removal of debugging lines that were cluttering the code
  # . These changes have been made to improve the readability of the code, as it appears the likelihood of others using this code is increasing

  #The extraction function is divided into three separate sections containing subsections within them
  #Section 1 is the bulk of the function - it extracts the data from each file and saves a formatted version for each participant
  #Section 2 is used to make adjustments to the final dataset. Typically, these adjustments are integrated into Section 1 as I continue to update the script
  #Section 3 is used to save the combined data set in .csv file format

  #Version 1.7 made signifciant changes to the labelling and annotating of the code
  # . Sections now have underlines to split the document outlineå

# Section 0 ---------------------------------------------------------------

# __ 0.1 - Debugging ---------------------------------------------------------------
  #debugging is used to check what within the BATD_extract_NF function is not working
  #if debugging is set to "on", then you will be able to use whatever data/site arguments you want

  debugging <- "off"
  if(debugging=="on"){
    list_of_filenames <- tactile_files
    site <- "EU-AIMS" #specify the site at which this data was collected (make it "NA" if unsure)
    p <- 1
  }

# __ 0.2 - Setup -------------------------------------------------------------------
  '%ni%' <- Negate('%in%') #create the function for %not in%
  inputDirectory <- getwd() #get the current wd

  if(hasArg(site) == FALSE){
    site <- "NA"
  }

# Section 1 ---------------------------------------------------------------
  #Entering the outer loop
  allParticipantsOutput <- list() #create a list for the output for each participant
  for(p in 1:length(list_of_filenames)){  #For loop through the participants identified in the inputDirectory

    setwd(inputDirectory) #set working directory to where all the Brain Gauge output files are saved
    output <- read.csv(list_of_filenames[p], header = FALSE) #read in the current participant's file (based on 'p')
    print(paste('now extracting details from file:', list_of_filenames[p]))

# __ 1.1 - Cleaning dataframe -------------------------------------

#General data cleaning prior to extraction
  #The output is read in as a two column dataframe, with columns V1 and V2
  #Column V1 contains all the 'information', followed by a ":', with the 'data' on the right side
  #Here, we are simply taking the first column and dividing the information and the data ...
  # ... and creating a dataframe that we can work with
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
    print("Section 1.1 completed")
  }

# __ 1.2 - Divide data -------------------------------------------

  #Now that the dataframe has been cleaned by section 1.1, we need to divide the output into its constituent protocols
  #Here, we identify a marker for where a protocol begins, and use this mark to break the dataframe up ...
  # ... storing each protocol in a list ('list_of_protocols')
  #Use "X" to mark the beginning of each protocol, and then split the dataframes based on the x
  #Since date appears at the start of every protocol, we mark each row with 'date' with a column that has 'x'

  cleaned_output$V5 <- ifelse(cleaned_output$V1=="date", "X",NA) #Mark date rows with 'x'
  protocols <- as.list(which(cleaned_output$V5=="X")) #Identify the row numbers in which X is marked
  list_of_protocols <- list() #Create a list external of the for loop for breaking the output into seperate protocols
  for (i in (1:length(protocols))){
    if(i==length(protocols)){break} #break the loop once you are at the last protocol
    list_of_protocols[[i]] <-  cleaned_output[(paste(protocols[i])):(paste(protocols[i+1])),] #subset from start of a protocol to the start of the next protocol
  }
  list_of_protocols[[i]] <- cleaned_output[(paste(protocols[i])):nrow(cleaned_output),] #puts the last protocol into the list (for loop above cannot account for last protocol)

  if(debugging == "on"){
    print("Section 1.2 completed")
  }

# __ 1.3 - Extract data -------------------------------------------

  #Given that the protocols have been put into a list called "list_of_protocols", we can now ...
  # ... enter each of the protocols and extract the relevant information
  #This section is further divided into subsections, each which extract a 'group' of information
  protocol_output_list <- list()

  for (i in (1:length(list_of_protocols))){

    current_protocol_for_current_participant <- list_of_protocols[[i]] #create a dataframe with the current protocol for the current participant
    rownames(current_protocol_for_current_participant) <- c() #clear all the row numbers for convenience

    # ____ 1.3.1 - Participant Details -----------------------------------------------------------

    #Extract participant details
    id <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="number"])[1]
    race <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="race"])[1]
    gender <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="gender"])[1]
    handedness <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="handedness"])[1]
    birthYear <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="birthYear"])[1]

    participant_details <- cbind(id, race, gender, handedness, birthYear) #create a dataframe with all participant details

    # ____ 1.3.2 - Session Details -----------------------------------------------------------
    #Extract session details
    timeProtocolStarted <-  paste0(substr(gsub("T","",(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"][1])), 11,12),":", substr(gsub("T","",(current_protocol_for_current_participant$V3[current_protocol_for_current_participant$V1=="date"][1])), 1,2)) #what time did the current protocol start

    #To determine which session this protocol is, we need to compare the date and time of this protocol to the data_and_times_dataframe created inSection 1.1
    #To do this, we need to first convert the date and time of the protocol time stamp to POSIXct so that it can be compared
    date <- stringr::str_sub(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"],1,10)[1] #format date
    hours <- stringr::str_sub(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="date"], 12, 14)[1] #format time - hours
    minutes <- current_protocol_for_current_participant$V3[current_protocol_for_current_participant$V1=="date"][1] #format time - minutes
    seconds <- current_protocol_for_current_participant$V4[current_protocol_for_current_participant$V1=="date"][1]
    time <- paste0(hours,":",minutes,":", seconds)[1] #format time - hours and minutes
    date_times <- paste(date, time)[1] #format date-time
    date_times_formatted <- sort(as.POSIXct(date_times,format="%Y-%m-%d %H:%M:%S"))[1] #convert to POSIXct and sort in ascending order
    session <- date_and_times_dataframe$session[date_and_times_dataframe$date_and_time==date_times_formatted][1] #extract the session number on data_and_times_dataframe for this protocol
    session_details <- cbind(date, time, site, session)

    # ____ 1.3.3 - Protocol Details ---------------------------------------------------------

    #Extract protocol details
    #Each protocol will have a protocol number, followed by details on how many practice/test trials there were ...
    # ... as well as the amplitude and duration of the stimuli
    protocol <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="protocol"]) #protocol number
    numberofPracticeTrials <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="numTrainingTrials"][1]) #number of practice trials
    numberofTestTrials <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="numTrials"][1]) #number of test trials
    ISI <- suppressWarnings(as.numeric(as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="ITI"][1]))) #interval between stimuli (interstimlus interval)
    interval_between_adaptive_and_test <- suppressWarnings(as.numeric(as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="intervalBetweenAdaptAndTest"][1]))) #interval between stimuli (interstimlus interval)

    protocol_details <- cbind(protocol, numberofPracticeTrials, numberofTestTrials, ISI, interval_between_adaptive_and_test)

    # ____ 1.3.4 - Stimulus Details -----------------------------------------------------------

    #Extract stimulus details
    #The stimuli are divided into:
    #stim1 - the first stimulus
    #stim2  - the second stimulus
    #astim1 - the first adaptive stimulus
    #astim2 - the second adaptive stimulus

    #Each stimuli will have amplitude, frequency and duration

    # Stimulus characteristics
      #stim1
      stim1amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[1]
      stim1frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[1]
      stim1duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[1]

      #stim2
      stim2amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[2]
      stim2frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[2]
      stim2duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[2]

      #astim1
      astim1amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[3]
      astim1frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[3]
      astim1duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[3]

      #astim2
      astim2amplitude <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="amplitude"])[3]
      astim2frequency <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="frequency"])[3]
      astim2duration <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="duration"])[3]

      stimulus_characteristics <- cbind(stim1amplitude, stim1frequency, stim1duration,
                                        stim2amplitude, stim2frequency, stim2duration,
                                        astim1amplitude, astim1frequency, astim1duration,
                                        astim2amplitude, astim2frequency, astim2duration)

    # ____ 1.3.5 - Performance Details ---------------------------------------------------------

    #While the details extracted thus far are constant, the performance data is updated every trial
    #Below, we extract the trial-level data for the current protocol, for the current participant
    #Peformance data is split into:
    # Value: what was the current 'value' for the given protocols, which can be any of the stimulus characterisics described above
    # The value will change based on the protocol type (e.g., the value for duration discrimination is stimulus duration)
    # Expected: what was the expected response for the current trial
    # Response: what was the participant's response for the current trial
    # Response Time: what was the participant's response time for the current trial
    trialNumber <- 1:length(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="value"]) #Note to self, potentially replace with numTrials
    value <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="value"])
    expected <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="expected"])
    response <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="response"])
    correctResponse <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="correct"])
    responseTime <- as.character(current_protocol_for_current_participant$V2[current_protocol_for_current_participant$V1=="responseTime"])

    performance_details <- cbind(trialNumber, value, expected, response, correctResponse, responseTime)

    # ____ 1.3.6 - Miscellaneous Details ---------------------------------------------------------
    #Store the miscellaneous information that is otherwise useful
    originalFilename <- list_of_filenames[p] #what was the original filename
    format <- "NF" # Format of data: OF = data collected with research grade cortical metrics stimulator, "NF" = data collected with brain gauge mice
    extractedBy <- Version #What version of BATD was used to extract this participant's data (stated at the very start of the code)

    miscellaneous_information <- cbind(originalFilename, format, extractedBy)


    # ____ 1.3.7 - Store Extracted Details ---------------------------------------------------------

    all_details <- cbind(participant_details,
                         session_details,
                         protocol_details,
                         stimulus_characteristics,
                         miscellaneous_information)

    #repeat all_details to make it have the same rows as performance_details (so they can be joined)
    all_details <- all_details[rep(seq_len(nrow(all_details)), each = nrow(performance_details)), ]
    all_extracted_data <- as.data.frame(cbind(all_details, performance_details))
    protocol_output_list[[i]] <- all_extracted_data
  }

  if(debugging == "on"){
    print("Section 1.3 completed")
  }

# ___ 1.4 - Combine Extracted Protocol Details -------------------------------------------

  participantTactileData <- do.call(rbind.data.frame, protocol_output_list)

  #Recreate the time and date variable and sort all the data by time and date
  participantTactileData$date_times_formatted <- paste(participantTactileData$date, participantTactileData$time)
  participantTactileData$date_times_formatted <- as.POSIXct(participantTactileData$date_times_formatted,format="%Y-%m-%d %H:%M:%S")
  participantTactileData <- participantTactileData[order(participantTactileData$date_times_formatted),]

  #Change correctResponse to a 0 or 1 numeric (currently its in true or false, I just want to standardise this between the old and new format, also string descriptions are not useful here)
    participantTactileData$correctResponse  <- as.character(participantTactileData$correctResponse)
    participantTactileData$correctResponse[participantTactileData$correctResponse=="true"] <- "1"
    participantTactileData$correctResponse[participantTactileData$correctResponse=="false"] <- "0"

    unique(participantTactileData$protocol)


    if(debugging == "on"){
      print("Section 1.4 completed")
    }

# ___ 1.5 - Label Protocols ----------------------------------------------
#The function will only work with sites where the protocol values have known protocol labels
#If the site argument provided in the function is not known to the developer, the code will exit prematurely

    sites_with_labels <- c(
      "KKI",
      "CCH",
      "ARBA1",
      "ARBA2",
      "ARBA3",
      "ARBA4",
      "SPIN",
      "CARE",
      "STES",
      "EU-AIMS"
      )

    if(site %ni% sites_with_labels){
      print("Warning: The 'site' argument provided is not a known site to the function, please contact Jason He : jasonhe93@gmail.com")
    }

    #Label protocols with names
    #There are subsections for RT protocols, Detection tasks, Discrimination tasks, and Judgment tasks
    # * Note that this is done in a specific order since sometimes protocols share numeric codes
    # * Note, there are far more protocols in the new format than the old format, hence the greater number of protocol numbers

    #As of version 1.7, protocol labeling has been changed to done separately for each site, rather than together all at once
    #This means that the user will have to specify the site in the function itself
    #'Site-specific labeling' which I am using to refer to this new method, is going to be the default
    #If users do not specify the site, then the code will attempt to label the protocols based on the method used in version 1.6 and below
    #This change was done in order to tackle the ongoing issue with trying to label all the protocols regardless of site, which led to various errors


    #read in the file from github that contains the site, and their protocol numbers/string_labels

    #if site is either KKI or CCH, read from this location
    if(site %in% c("KKI", "CCH")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name
    }

    #If site is one of the ARBA sites, read from this location
    if(site %in% c("ARBA1", "ARBA2", "ARBA3", "ARBA4")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/ARBA/ARBA_protocol_names.csv") #file name
    }

    #If site is the SPIN site, read from this location
    if(site %in% c("SPIN")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name

    }

    #If site is the SPIN site, read from this location
    if(site %in% c("CARE")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name
    }

    #If site is the STES site, read from this location
    if(site %in% c("STES")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name
    }

    #If site is the EU-AIMS site, read from this location
    if(site %in% c("EU-AIMS")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name
    }

    #If site is the NA site, read from this location
    if(site %in% c("EU-AIMS")){
      url <- paste0("https://raw.githubusercontent.com/HeJasonL/BATD/master/Site%20specific%20protocols/",
                    site, "/", #folder name
                    site, "_protocol_names.csv") #file name
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

    #Adjust for site customization
    if(site == "KKI"){
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
      participantTactileData$protocolName[participantTactileData$protocol==171] <- "Dual Staircase Amplitude Discrimination (down)" #Reminder, need to figure out whether 171 is truly Dual Staircase Amp down
    }

    if(site %in% c("CARE")){
      participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
      participantTactileData$protocolName[participantTactileData$protocolName=="TBD"] <- "Simultaneous Amplitude Discrimination"
    }

    if(site %in% c("STES")){
      participantTactileData$protocolName[participantTactileData$protocol==713 & participantTactileData$stim2amplitude==30] <- "Dynamic Detection Threshold (down)"
    }

    if(site == "EU-AIMS"){
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100 & participantTactileData$astim1amplitude==0] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==100] <- "Static Detection Threshold with Adaptation ISI 100"
    }

    if(site == "NA"){
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100 & participantTactileData$astim1amplitude==0] <- "Amplitude Discrimination with Single Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==30] <- "Static Detection Threshold with Adaptation ISI 30"
      participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$interval_between_adaptive_and_test==100] <- "Static Detection Threshold with Adaptation ISI 100"
    }


    if(debugging == "on"){
      print("Section 1.5 completed")
    }


# ___  1.6 - Store Labelled Protocols -------------------------------------------------------------

    #Store the protocol outputs in the allParticipantsOutput list
    allParticipantsOutput[[p]] <- as.data.frame(participantTactileData)
  }

  if(debugging == "on"){
    print("Section 1.6 completed")
  }

  allParticipantsOutput_combined <-  as.data.frame(data.table::rbindlist(allParticipantsOutput, fill = TRUE)) #combine the output into a unitary dataframe
  #allParticipantsOutput_combined$protocol[is.na(allParticipantsOutput_combined$protocolName)]

# Section 2 ---------------------------------------------------------------
  #Annotation pending
# __ 2.1 - Accounting For Runs ----------------------------------------

list_for_runs <- list()
participants <- unique(allParticipantsOutput_combined$id)
for(p in 1:length(participants)){
  current_p <- allParticipantsOutput_combined[allParticipantsOutput_combined$id==participants[p],]
  sessions <- unique(current_p$session)
    for(s in 1:length(sessions)){
    current_s <- current_p[current_p$session==sessions[s],]
    times <- unique(current_s$time)
      protocols_completed_in_session <- list()
      for(t in 1:length(times)){
      current_t <- current_p[current_p$time==times[t],] #subset to current unique timepoint
      protocols_completed_in_session <- append(protocols_completed_in_session, current_t$protocolName[1]) #add the protocol completed to the protocols_completed_in_session list
      repeats <- sum(protocols_completed_in_session==current_t$protocolName[1]) #calculate number of repeats of this protocol

      if(repeats > 1){ #if there are more than 1 repeats of the current protocol, then repeat the value of the repeats by nrow of the current trial
        t_repeated <- rep(repeats, times = nrow(current_t))
      } else {
        t_repeated <- rep(1, times = nrow(current_t)) #otherwise, just repeat the value 1 by nrow of the current trial
      }

      list_for_runs <- append(list_for_runs, t_repeated)
      }
    }
  }

allParticipantsOutput_combined$run <- unlist(list_for_runs)

if(debugging == "on"){
  print("Section 2.1 completed")
}


# __ 2.2 - Column Formatting  ----------------------------------------------------------------
#Changing variables to the right format (numeric)
allParticipantsOutput_combined <- suppressWarnings(data.frame(lapply(allParticipantsOutput_combined, as.character), stringsAsFactors=FALSE))
allParticipantsOutput_combined[9:25] <- suppressWarnings(sapply(allParticipantsOutput_combined[9:25], as.numeric))
allParticipantsOutput_combined[29:30] <- suppressWarnings(sapply(allParticipantsOutput_combined[29:30], as.numeric))
allParticipantsOutput_combined[30] <- suppressWarnings(sapply(allParticipantsOutput_combined[30], as.numeric))
allParticipantsOutput_combined$numberofPracticeTrials[is.na(allParticipantsOutput_combined$numberofPracticeTrials)] <- 0

if(debugging == "on"){
  print("Section 2.2 completed")
}

# Section 3 ---------------------------------------------------------------

# __ 3.1 - Labelling and Saving data -----------------------------------------------------------------
#annotation pending

allParticipantsOutput_combined$Site <- site #create a site column (site is provided by an argument in the function)

if(debugging == "on"){
  print("Section 3.1 completed")
}

print("All participant data extracted succesfully!")



return(allParticipantsOutput_combined) #return the combined data
}

