#' BATD_extract_OF
#'
#' Takes a list of filenames in a working directory which contain the performance data recorded from Cortical Metrics. These performance files are typically .txt files. 'BATD_extract_NF' was specifically designed to analyze data from the Brain Gauge Research Editions and any version prior.
#' For more information about Cortical Metrics Brain Gauage, see: https://www.corticalmetrics.com/braingaugeresearch. BATD_extract_OF requires two inputs, with the first being the list of participants and the second being a string variable referring to the site where the data was collected.
#' While both 'BATD_extract_NF' and 'BATD_extract_OF' return a combined dataframe for all the participants that have had their data extracted, the individual and combined files are saved in folders created in the original working directory.
#' In the working directory, folders 'output' and 'combined' will be created to store the individual files that were extracted and a file containing all the individual files combined respectively. These files are in .csv format.
#'
#' @param list_of_filenames a list object containing the filenames of the .txt files containing participant performance from Brain Gauge
#' @param Site refers to the site where the data was collected. In the past and in the future, the original codes will be patched for different sites. For Site, put the site location inbetween two quotation marks. For e.g., 'JHU' refers to the Johns Hopkins University
#'
#' @return BATD_extract_OF returns a dataframe containing the extracted data for each participant. Protocols and protocol names are provided as columns, along with other relevant metrics.
#'
#' @examples
#' currently NA
#'
#' @export

BATD_extract_OF <- function(list_of_filenames, Site){

##VERSION ----
Version <- c("BATD_V.1.5")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    setwd("~/Dropbox/Documents/Data repository/Tactile Data/Raw/Old Format/KKI") #setwd to old format data from JHU
    list_of_filenames <- list.files(pattern = "-")
    Site <- ("KKI")
    getwd()
  }

  ## SECTION 1 (setup for entry into the master for loop) ----
  inputDirectory <- getwd()
  dir.create("output", showWarnings = FALSE)  #set the wd to the folder where you wish to save the output data to
  outputDirectory <- paste0(inputDirectory, "/output")  #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.

  if(debugging=="on"){print("SECTION 1: COMPLETED")}

  ## SECTION 2 (For loop through the participants identified in the inputDirectory)

  allParticipantsOutput <- list()  #creates a list to store the output containing all the output from all the protocols combined (outer loop)
  for (p in 1:length(list_of_filenames)){

    ## SECTION 2.0 ----
    setwd(paste0(inputDirectory, "/", list_of_filenames[p]))  #setwd to participant's folder [this folder contains another folder named as the date of when the particpant completed the task]
    participantsFolder <- getwd()
    filesinFolder <- list.files(pattern = "-")  #We want to specify the folder named as the date of testing and nothing else (it must have have a '-' that splits the dates)
    datefileinFolder <- filesinFolder[!grepl(".csv", filesinFolder)]  #Then, we want to make sure we don't pick up any .csvs (which also tends to be present in these folders)

    setwd(paste0(participantsFolder, "/", datefileinFolder))  #set the working directory to the folder (datefileinFolder) which contains all the protocols
    protocolsFolder <- getwd()
    protocolsinfolder <- list.files()  #list all the files in the protocolsFolder
    protocolsinprotocolsFolder <- protocolsinfolder[!stringr::str_length(protocolsinfolder) > 3]  #subset to only the folders with a str length of 3, since all protocols have a 3 digit identifier


    nthProtocolOutputList <- list()  #creates a list to store the output from the nth protocol (middle loop)
    for (n in 1:length(protocolsinprotocolsFolder)){
      setwd(paste0(protocolsFolder, "/", protocolsinprotocolsFolder[n]))  #set the directory to the n^th protocol
      protocolOutputs <- list.files(pattern = "1-")  #identify the particpant files (the pattern '1-' refers to the protocol output files)
      print(paste("now processing protocol: ", protocolsinprotocolsFolder[n], " for participant: ", list_of_filenames[p]))

      if(identical(list.files(pattern = ".txt"), character(0))){
        # print("folder was empty, next")
          next
        # This is a short piece of code to test whether the folder has any items within the folder
        # This needs to exist because while all the folders for each protocol are
        # present, the participant may not have completed the protocol (i.e., the folder
        # is empty) This is an issue specific to the old format, the 'next' just skips
        # the current protocol within the loop
      }

      protocol_output_for_a_given_participant_for_a_given_run <- list()
      #participants may have completed more than one run of the protocol. This is discernible if they have more than one file with the "1-" pattern (check using length(protocolOutputs))
      for(s in 1:length(protocolOutputs)){
        output <- read.csv(protocolOutputs[s], header = FALSE, sep = "\t")  #read in the current protocol for the current session (s), and assign it to a dataframe named output

        ## SECTION 2.1 Participant details ----
        id <- as.character(output$V2[output$V1 == "Subject_Number"])
        race <- as.character(output$V2[output$V1 == "Race"])
        gender <- as.character(output$V2[output$V1 == "Gender"])
        handedness <- as.character(output$V2[output$V1 == "Handedness"])
        birthYear <- as.character(output$V2[output$V1 == "Birthdate"])

        participantDetails <- as.data.frame(cbind(id, race, gender, handedness, birthYear))  #Combine participant details into a dataframe

        # adjust the participant details so the strings line up with how they are
        # referred to in the new format

        if(participantDetails$race == "Default"){
          participantDetails$race <- "caucasian"
        } else {
          participantDetails$race <- participantDetails$race
        }  #in the old format, caucasians were referred to as default, but in the new format, caucasians are just referred to as caucasians
        if (participantDetails$gender == "M") {
          participantDetails$gender <- "Male"
        } else {
          participantDetails$gender <- "Female"
        }  #In the new format, M and F are written in full (note, prefer not to say was not an option in either format)
        if (participantDetails$handedness == "R") {
          participantDetails$handedness <- "Right"
        } else {
          participantDetails$handedness <- "Left"
        }  #R is fully expressed Right, and L as Left in the new output
        participantDetails$birthYear <- stringr::str_sub(participantDetails$birthYear,
          -4, -1)  #birthYear is presented as just year (this gets the last four numbers )

        ## SECTION 2.2 Protocol details ----
        Filename <- list.files(pattern = "2-")  #identify the original filename for the protocol trial details
        protocolTrialDetails <- read.csv(Filename[s], header = TRUE, sep = "\t")[, 1:4]  #the last hard bracket section removes a column with just NAs

        date <- gsub("/", "-", as.character(output$V2[output$V1 == "Date"]))
        protocol <- protocolsinprotocolsFolder[n]
        timeProtocolStarted <- as.character(output$V2[output$V1 == "Time"], format = "%H:%M")
        site <- Site
        format <- "OF"
        extractedBy <- Version
        numberofPracticeTrials <- as.factor("0")
        numberofTestTrials <- as.numeric(as.character(nrow(protocolTrialDetails))) #nrow = ntrials
        ISI <- as.numeric(as.character(output$V2[output$V1 == "Interval_b/w_Adaptor_and_Test"]))
        stim1amplitude <- as.numeric(as.character(output$V2[output$V1 == "Stimulus_1_amp"]))
        stim1duration <- as.numeric(as.character(output$V2[output$V1 == "Stimulus_1_StimDuration"])) #compared to below, there is always a stim 1
        if ("Stimulus_2_amp" %in% output$V1) {
          stim2amplitude <- as.numeric(as.character(output$V2[output$V1 == "Stimulus_2_amp"]))
          stim2duration <- as.numeric(as.character(output$V2[output$V1 == "Stimulus_2_StimDuration"]))
        } else {
          stim2amplitude <- NA
          stim2duration <- NA
        }

        if ("Adapting_Stimulus_1_amp" %in% output$V1) {
          astim1amplitude <- as.numeric(as.character(output$V2[output$V1 == "Adapting_Stimulus_1_amp"]))
          astim1duration <- as.numeric(as.character(output$V2[output$V1 == "Adapting_Stimulus_1_StimDuration"]))
        } else {
          astim1amplitude <- NA
          astim1duration <- NA
        }

        if ("Adapting_Stimulus_2_amp" %in% output$V1) {
          astim2amplitude <- as.numeric(as.character(output$V2[output$V1 == "Adapting_Stimulus_2_amp"]))
          astim2duration <- as.numeric(as.character(output$V2[output$V1 == "Adapting_Stimulus_2_StimDuration"]))
        } else {
          astim2amplitude <- NA
          astim2duration <- NA
        }

        originalFilename <- Filename[s]


        protocolDetails <- as.data.frame(cbind(date, timeProtocolStarted, site, format, extractedBy, protocol, numberofPracticeTrials, numberofTestTrials, ISI,
                                               stim1amplitude, stim2amplitude,
                                               astim1amplitude, astim2amplitude,
                                               stim1duration, stim2duration,
                                               astim1duration, astim2duration, originalFilename))


        ## SECTION 2.3 Performance details ----
        names(protocolTrialDetails)[names(protocolTrialDetails) == "Response_time"] <- "responseTime"
        names(protocolTrialDetails)[names(protocolTrialDetails) == "Val"] <- "value"
        names(protocolTrialDetails)[names(protocolTrialDetails) == "Response"] <- "response"
        names(protocolTrialDetails)[names(protocolTrialDetails) == "Correct"] <- "correctResponse"
        correctResponse <- suppressWarnings(as.character(as.numeric(protocolTrialDetails$response[protocolTrialDetails$correctResponse == 1][1])))
        incorrectResponse <- 0
        protocolTrialDetails$expected <- ifelse(protocolTrialDetails$correctResponse == 1, correctResponse, incorrectResponse)
        protocolTrialDetails <- cbind(1:nrow(protocolTrialDetails),protocolTrialDetails)
        colnames(protocolTrialDetails)[1] <- "trialNumber"

        performanceDetails <- protocolTrialDetails

        ## SECTION 2.4 Combine the participant details, protocol details and performance details and put into protocolOutput_for_given_participant list ----
        allProtocolDetails <- cbind(participantDetails, protocolDetails, performanceDetails)


        #Put the subProtocolOutput into the external list ----
        protocol_output_for_a_given_participant_for_a_given_run[[s]] <- allProtocolDetails
      }

      print(length(protocol_output_for_a_given_participant_for_a_given_run))
      combined_protocol_outputs_for_a_given_participant_for_a_given_run <- data.table::rbindlist(protocol_output_for_a_given_participant_for_a_given_run, use.names = TRUE) #row binds the dataframes in protocolOutput_for_given_participant (usually this is just a single file, since most participants only do one session)
      nthProtocolOutputList[[n]] <- combined_protocol_outputs_for_a_given_participant_for_a_given_run

    }

    allProtocolOutputs <- data.table::rbindlist(nthProtocolOutputList, fill = TRUE)  #row binds the dataframes in nthProtocolOutputList (usually this is just a single file, since most participants only do one session)

    #Inclusion of session & run information

    allProtocolOutputs <- as.data.frame(allProtocolOutputs)
    allProtocolOutputs <- allProtocolOutputs[with(allProtocolOutputs, order(allProtocolOutputs$timeProtocolStarted)),]
    allProtocolOutputs$timeProtocolStarted <- as.POSIXct(allProtocolOutputs$timeProtocolStarted,format="%H:%M") #convert the time variable to time (ignore the inclusion of the current YEAR)
    allProtocolOutputs$orderCompleted <- cumsum(c(0,as.numeric(diff(allProtocolOutputs$timeProtocolStarted))!=0)) + 1 #create a column for the completion order of the protocols (I cannot remember how the cumsum function works)

    if(debugging =="on"){print("SECTION 2: COMPLETED")}

    ## SECTION 3.0 Label protocols with names (note that this is done in a specific order, do not change) ----
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 801] <- "Simple Reaction Time"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 800] <- "Choice Reaction Time"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 100 & allProtocolOutputs$stim1amplitude == 20 & is.na(allProtocolOutputs$stim2amplitude)] <- "Static Detection Threshold"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 713] <- "Dynamic Detection Threshold"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 100 & allProtocolOutputs$stim1amplitude == 100 & allProtocolOutputs$stim2amplitude == 200] <- "Simultaneous Amplitude Discrimination"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 220 & allProtocolOutputs$stim1amplitude == 200 & allProtocolOutputs$stim2amplitude == 200 & allProtocolOutputs$ISI == 0] <- "Sequential Frequency Discrimination"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 200 & allProtocolOutputs$stim1amplitude == 200 & allProtocolOutputs$stim2amplitude == 200 & allProtocolOutputs$ISI == 0] <- "Simultaneous Frequency Discrimination"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 109 & allProtocolOutputs$stim1amplitude == 15 & is.na(allProtocolOutputs$stim2amplitude) & allProtocolOutputs$ISI == 30] <- "Static Detection Threshold with Adaptation ISI 30"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 109 & allProtocolOutputs$stim1amplitude == 15 & is.na(allProtocolOutputs$stim2amplitude) & allProtocolOutputs$ISI == 100] <- "Static Detection Threshold with Adaptation ISI 100"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 109 & allProtocolOutputs$stim1amplitude == 100 & allProtocolOutputs$stim2amplitude == 200 & allProtocolOutputs$astim1amplitude == 100 & allProtocolOutputs$astim1amplitude == 100 & allProtocolOutputs$ISI == 1000] <- "Amplitude Discrimination with Single Site Adaptation"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 109 & allProtocolOutputs$stim1amplitude == 100 & allProtocolOutputs$stim2amplitude == 200 & allProtocolOutputs$astim1amplitude == 100 & allProtocolOutputs$ISI == 100] <- "Amplitude Discrimination with Single Site Adaptation"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 350 & allProtocolOutputs$stim1amplitude == 300 & allProtocolOutputs$stim2amplitude == 300 & allProtocolOutputs$ISI == 0] <- "Duration Discrimination"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 300] <- "Temporal Order Judgement"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 301] <- "Temporal Order Judgement with Carrier"
    allProtocolOutputs$protocolName[allProtocolOutputs$protocol == 103] <- "Amplitude Discrimination with Dual Site Adaptation"

    if(debugging=="on"){print("SECTION 3: COMPLETED")}

    #SECTION 4.0 Account for sessions ----
    #Create a column for the sessions a participant completed (based on the date)
    month <- as.numeric(sub("(^[^-]+)-.*", "\\1", allProtocolOutputs$date)[1])
    if(month < 10){
      allProtocolOutputs$date <- paste0(0,allProtocolOutputs$date)
    }
    allProtocolOutputs$date <- as.Date(allProtocolOutputs$date , "%m-%d-%Y")

    dates <- sort(unique(allProtocolOutputs$date))
    timePointList <- list()

    for (d in 1:length(dates)){
      givenDate <- allProtocolOutputs[allProtocolOutputs$date==dates[d],]
      givenDate$session <- d
      timePointList[[d]] <- givenDate}

    allProtocolOutputs <- plyr::rbind.fill(timePointList)

    ## SECTION 4 Create column(s) to detail the extraction process ----
    allProtocolOutputs$site <- Site
    allProtocolOutputs$format <- "OF"  #Specify that the format of the data is the old format
    allProtocolOutputs$extractedBy <- "BATD V.1.5"  #Specify that the data was extracted by BATD version V.X.X

    if(debugging=="on"){print("SECTION 4: COMPLETED")}

    ## SECTION 5 Save the extracted file for each individual participant in the output directory -----
    # currentDirectory <- getwd()  #remember the current wd
    # setwd(outputDirectory)  #setwd to the outputDirectory
    # write.csv(allProtocolOutputs, file = paste0("BATD_extracted_", list_of_filenames[p], "_OF.csv"))  #save the output of all the protocols for each participant as a csv

    print(paste("Extracted participant:", list_of_filenames[p]))


    allParticipantsOutput[[p]] <- as.data.frame(allProtocolOutputs)

    if(debugging=="on"){print("SECTION 5: COMPLETED")}
    setwd(inputDirectory)  #(re)set the input directory
  }

  allParticipantsOutput_combined <- as.data.frame(data.table::rbindlist(allParticipantsOutput, fill = TRUE))

  ## SECTION 2 (Accounting for multiple runs ) -----
  #I genuinely apologize for the stress that anyone who has to read through SECTION 2 of the code will inevitably go through, it is very convoluted and likely inefficient
  #This section accounts for the fact that *some* protocols have participants complete the same protocol twice
  #Here we account for this by splitting the data into a) the participants, b) the sessions that participant completed and c) the protocols completed by that participant within that session
  #Once we are at the level of the protocols that participant completed, we then reorder the protocols completed using the TIME at which the protocol started
  #We then give each protocol a number for the order in which it was completed
  #Unfortunately, this has been done through a series of nested for loops, which makes it difficult to navigate and troubleshoot

  # SECTION 2.1 (setup) ----
  alldata <- allParticipantsOutput_combined #assign the output from SECTION 1 to a new dataframe called 'alldata'
  alldata <- alldata[!is.na(alldata$protocolName),] #remove any rows where the protocol name is not avaialble (if the above has run correctly, this should not even be necessary)

  uniqueParticipants <- unique(alldata$id) #identify the unique ids of all the participants that have been extracted
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)] #remove any participants whose id is NA

  #Create a series of external lists for the for loops below
  participants_outPut_list <- list()
  participants_detail_list <- list()
  participant_data_after_accounting_for_runs <- list()

  # SECTION 2.2 (split dataframe by unique participants) ----
  for(x in 1:length(uniqueParticipants)){
    participantData <- alldata[alldata$id==uniqueParticipants[x],] #subset to the current participant (denoted by uniqueParticipants[x])
    #print(paste("ID at the start of the loop:",participantData$id[x]))
    sessions <- unique(participantData$session) #identify the number of unique sessions completed by this participant (provided by Section 6, based on date)
    sessions_for_loop_output <- list()

    # SECTION 2.3 (split dataframe further into unique sessions completed by a given participant) ----
    for(s in 1:length(sessions)){
      participantSessionData <- participantData[participantData$session==sessions[s],] #subset to the current session
      protocols_within_session_completed <- as.character(unique(participantSessionData$protocolName)) #identify the protocols that were completed within the given session
      list_of_protocols_completed_with_runs <- list()

      #SECTION 2.4 (split dataframe further into unique protocols completed by a given participant for a given session) ----
      for(p in 1:length(protocols_within_session_completed)){
        protocolName <- protocols_within_session_completed[p]
        numberofRuns <- nlevels(as.factor(participantSessionData$orderCompleted[participantSessionData$protocolName==protocols_within_session_completed[p]]))
        ProtocolRuns <- as.data.frame(cbind(protocolName, numberofRuns))
        list_of_protocols_completed_with_runs[[p]] <- ProtocolRuns
      }

      protocols_completed_by_runs <- plyr::rbind.fill(list_of_protocols_completed_with_runs) #table with protocolNames and the number of runs for each protocolName
      protocols_completed_by_runs$numberofRuns <- as.numeric(as.character(protocols_completed_by_runs$numberofRuns))
      names_of_protocols_completed_more_than_once <- protocols_completed_by_runs$protocolName[as.numeric(as.character(protocols_completed_by_runs$numberofRuns)) > 1]
      names_of_protocols_completed_only_once <- protocols_completed_by_runs$protocolName[protocols_completed_by_runs$numberofRuns == 1]

      #SECTION 2.4.1 Create a column called run (and give number referring to the run number) ----
      #Protocols completed more than once ----
      if(length(names_of_protocols_completed_more_than_once) > 0){
        #1. Identify which protocols were completed more than once, and how many times they were completed more than once
        #2. subset to the protocol completed more than once and add this to a column called run

        list_of_protocols_completed_more_than_once <- list()

        for(o in 1:length(names_of_protocols_completed_more_than_once)){
          protocol_completed_more_than_once <- participantSessionData[participantSessionData$protocolName==names_of_protocols_completed_more_than_once[o],] #subset to the protocol completed more than once
          number_of_runs <- length(unique(protocol_completed_more_than_once$orderCompleted)) #identifies the number of runs
          run_number <- unique(protocol_completed_more_than_once$orderCompleted) #identifies the run number

          list_of_runs <- list()
          for(r in 1:number_of_runs){
            currentRun <- protocol_completed_more_than_once[protocol_completed_more_than_once$orderCompleted==run_number[r],] #subset to the 'r' unique run, given by the orderCompleted
            currentRun$run <- r #apply run number to column
            list_of_runs[[r]] <- currentRun
          }
          protocol_completed_more_than_once <- plyr::rbind.fill(list_of_runs)
          list_of_protocols_completed_more_than_once[[p]] <- protocol_completed_more_than_once
        }
        all_protocols_completed_more_than_once <- plyr::rbind.fill(list_of_protocols_completed_more_than_once)

      }#end of if length(names_of_protocols_completed_more_than_once > 0)

      #protocols completed only once ----
      if(length(names_of_protocols_completed_only_once) > 0){
        all_protocols_completed_just_once <- participantSessionData[!participantSessionData$protocolName %in% names_of_protocols_completed_more_than_once,] #subset to the protocols only completed once
        all_protocols_completed_just_once$run <- 1 #add run number

      } #end of if length(names_of_protocols_completed_only_once > 0)

      #Recombine the protocols completed more than once back with the protocols completed only once ----
      if(length(names_of_protocols_completed_more_than_once) > 0){
        combined_protocol_data <- rbind(all_protocols_completed_more_than_once, all_protocols_completed_just_once)
      }
      else {combined_protocol_data <- all_protocols_completed_just_once #if there were only protocols ocompleted once, than combined_protocol-data are just the protocols completed once
      }
      sessions_for_loop_output[[s]] <- combined_protocol_data
    } #end of sessions loop

    combined_protocol_data_with_runs <- plyr::rbind.fill(sessions_for_loop_output)
    combined_protocol_data_with_runs <- combined_protocol_data_with_runs[with(combined_protocol_data_with_runs, order(combined_protocol_data_with_runs$orderCompleted)),] #sort though that the orderCompleted is in order again (was out of order based on rbind above)

    participant_data_after_accounting_for_runs[[x]] <- combined_protocol_data_with_runs
    # print(paste("ID at the end of the loop:",participantData$id[x]))

  } #end of participant loop

  combined_participant_data_after_accounting_for_runs <- plyr::rbind.fill(participant_data_after_accounting_for_runs)

  if(debugging=="on"){print("Succesfully Completed SECTION 2: Runs accounted for")}


  setwd(inputDirectory)
  # dir.create("combined", showWarnings = FALSE)  #set the wd to the folder where you wish to save the combined data to
  # combinedDirectory <- paste0(inputDirectory, "/combined")  #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.
  # setwd(combinedDirectory)
  # write.csv(allParticipantsOutput_combined, file = "BATD_extracted_combined.csv")
  # setwd(inputDirectory)
  # print(paste0("Combined extracted data saved in:", combinedDirectory))

  print("All participants extracted")
  return(combined_participant_data_after_accounting_for_runs)
}
