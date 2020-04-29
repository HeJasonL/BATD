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

BATD_extract_NF <- function(list_of_filenames, Site){

  ##Version
  Version <- c("BATD_V.1.5")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    setwd("~/Dropbox/Documents/Projects/POND Project/POND Data/ARBA1")
    list_of_filenames <- list.files(pattern = "-") #list the txt files containing participant's performance
    Site <- ("ARBA1")
  }

  '%ni%' <- Negate('%in%') #create the function for %not in%
  inputDirectory <- getwd()
  dir.create("output", showWarnings = FALSE) #set the wd to the folder where you wish to save the output data to
  outputDirectory <- paste0(inputDirectory,"/output") #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.

  ##SECTION 1 ----

  #Create external lists -----
  allParticipantsOutput <- list()

  #For loop through the participants identified in the inputDirectory ----

  for(p in 1:length(list_of_filenames)){
    #p <- 1
    setwd(inputDirectory) #set working directory
    output <- read.csv(list_of_filenames[p], header = FALSE) #read in the current participant[p]'s file

    #General data cleaning prior to extraction ----
    output$V1 <- as.character(output$V1) #turn column 1 into a character
    output$V1 <- gsub(" ","",output$V1) #replace all blank spaces in column 1
    tempo <- suppressWarnings(t(as.data.frame(strsplit(output$V1,":")))) #split the string by ':' and transpose it to long format
    rownames(tempo) <- c() #clear the row numbers
    temp <- suppressWarnings(as.data.frame(tempo)) #turn the output into a dataframe

        #Break the dataframe up into separate protocols stored in a list ----
          #Use "X" to mark the beginning of each protocol
          temp$V4 <- ifelse(temp$V1=="date", "X",NA) #All of the files start with date, we will use date as marker of the start of each protocol - create a column which marks date with a 'X'
          protocols <- (which(temp$V4=="X")) #This line tells us all of the instances in *which* 'X' occurs
          protocols <- as.list(protocols)
          list <- list() #Create a list external of the for loop for breaking the output into seperate protocols
          for (i in (1:length(protocols))){
            if(i==length(protocols)){break} #Note to self, if it's 14, then we want i+1 to be something else (i.e., the end of the file)
            list[[i]] <-  temp[(paste(protocols[i])):(paste(protocols[i+1])),]
          } #loop for breaking the output into separate protocols
          list[[i]] <- temp[(paste(protocols[i])):nrow(temp),] #puts the last protocol into the list (for loop above cannot account for last protocol)

    #For loop through the protocols in the protocolsFolder and extract the participant/protocol/performance details ----
    ProtocolOutputList <- list()
    for (i in (1:length(list))){
      #i <- 1
      output <- list[[i]]
      rownames(output) <- c() #clear the row numbers

      # (1) Extract Participant details ----
      race <- as.character(output$V2[output$V1=="race"])
      gender <- as.character(output$V2[output$V1=="gender"])
      handedness <- as.character(output$V2[output$V1=="handedness"])
      birthYear <- as.character(output$V2[output$V1=="birthYear"])
      id <- as.character(output$V2[output$V1=="number"]) #is this participant id?

      participantDetails <- as.data.frame(cbind(id, race, gender, handedness, birthYear))

      # (2) Extract Protocol Details ----

      date <- substr(gsub("T","",(output$V2[output$V1=="date"][1])), 1,10)
      site <- Site
      format <- "NF"
      extractedBy <- Version

      protocol <- as.character(output$V2[output$V1=="protocol"])
      numberofPracticeTrials <- as.character(output$V2[output$V1=="numTrainingTrials"][1])
      numberofTestTrials <- as.character(output$V2[output$V1=="numTrials"][1])
      stim1amplitude <- as.character(output$V2[output$V1=="amplitude"])[1]
      stim2amplitude <- as.character(output$V2[output$V1=="amplitude"])[2]
      astim1amplitude <- as.character(output$V2[output$V1=="amplitude"])[3]
      astim2amplitude <- as.character(output$V2[output$V1=="amplitude"])[4]

      stim1duration <- as.character(output$V2[output$V1=="duration"])[1]
      stim2duration <- as.character(output$V2[output$V1=="duration"])[2]
      astim1duration <- as.character(output$V2[output$V1=="duration"])[3]
      astim2duration <- as.character(output$V2[output$V1=="duration"])[4]

      #output$V1[output$V1=="ITI"] <- "ISI" #for the new format, ISI is referred to as ITI
      ISI <- suppressWarnings(as.numeric(as.character(output$V2[output$V1=="ITI"][1])))

      originalFilename <- list_of_filenames[p]

      protocolDetails <- as.data.frame(cbind(date, site, format, extractedBy, protocol, numberofPracticeTrials, numberofTestTrials, ISI,
                                         stim1amplitude, stim2amplitude,
                                         astim1amplitude, astim2amplitude,
                                         stim1duration, stim2duration,
                                         astim1duration, astim2duration, originalFilename))

      # (3) Extract Performance details ----
      value <- as.character(output$V2[output$V1=="value"])
      expected <- as.character(output$V2[output$V1=="expected"])
      response <- as.character(output$V2[output$V1=="response"])
      correctResponse <- as.character(output$V2[output$V1=="correct"])
      responseTime <- as.character(output$V2[output$V1=="responseTime"])

      performanceDetails <- as.data.frame(cbind(value, expected, response, correctResponse, responseTime))

      All <- cbind(participantDetails, protocolDetails, performanceDetails)

      All$trialNumber <- 1:nrow(All)

      ProtocolOutputList[[i]] <- All
    }

    participantTactileData <- do.call(rbind.data.frame, ProtocolOutputList)

    #Change correctResponse to a 0 or 1 numeric (currently its in true or false, I just want to standardise this between the old and new format, also string descriptions are not useful here)
    participantTactileData$correctResponse  <- as.character(participantTactileData$correctResponse)
    participantTactileData$correctResponse[participantTactileData$correctResponse=="true"] <- "1"
    participantTactileData$correctResponse[participantTactileData$correctResponse=="false"] <- "0"

    #Label protocols with names (note that this is done in a specific order since sometimes protocols share numeric codes ----
    #Note, there are far more protocols in the new format than the old format, hence the greater number of protocol numbers

    participantTactileData$protocolName[participantTactileData$protocol==801] <- "Simple Reaction Time"
    participantTactileData$protocolName[participantTactileData$protocol==800] <- "Choice Reaction Time"

    participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
    participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==5000] <- "Static Detection Threshold"

    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==30] <- "Static Detection Threshold with Adaptation ISI 30"
    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==100] <- "Static Detection Threshold with Adaptation ISI 100"

    participantTactileData$protocolName[participantTactileData$protocol==713] <- "Dynamic Detection Threshold"

    participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim1amplitude==100] <- "Sequential Amplitude Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==100 &  participantTactileData$stim1amplitude==100] <- "Sequential Amplitude Discrimination"

    participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$protocolName != "Static Detection Threshold"]

    participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim2amplitude!=0] <- "Simultaneous Amplitude Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==105] <- "Simultaneous Amplitude Discrimination"

    participantTactileData$protocolName[participantTactileData$protocol==905 & participantTactileData$ISI==100] <- "Sequential Amplitude Discrimination" #Sequential amplitude discrimination for Calgary

    participantTactileData$protocolName[participantTactileData$protocol==925] <- "Sequential Frequency Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==920] <- "Simultaneous Frequency Discrimination"

    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100] <- "Amplitude Discrimination with Single Site Adaptation"
    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"

    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==200] <- "Dual Staircase Amplitude Discrimination (up)"
    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==200] <- "Dual Staircase Amplitude Discrimination (down)"

    participantTactileData$protocolName[participantTactileData$protocol==350] <- "Duration Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==950] <- "Duration Discrimination"

    participantTactileData$protocolName[participantTactileData$protocol==930] <- "Temporal Order Judgement"
    participantTactileData$protocolName[participantTactileData$protocol==931] <- "Temporal Order Judgement with Carrier"

    if(debugging=="on"){
      print("SECTION 1: COMPLETED")
    }



     ##SECTION 2 ----

    #Accounting for session ----------------
    #Where this code (BATD_extract_NF) differs from the old format (BATD_extract_OF) is that sessions are not accounted for within the same folder, but rather, are accounted for posthoc (i.e., after all the data for a given participant is  combined)
    #In the old format, a participant who does the same session twice will have data stored in the same folder, whereas in the new format, this was not the case (at least at JHU/KKI)
    #There are some cases where participants completed the same protocol twice, we need to be able to differentiate whether it was their first or second attempt
    #The attempts are always in chronological order (again, at least at JHU and KKI)

    #Create a column for the sessions a participant completed (based on the date) ----
    participantTactileData$date <- as.Date(participantTactileData$date)
    dates <- sort(unique(participantTactileData$date))
    timePointList <- list()
    for (d in 1:length(dates)){
      givenDate <- participantTactileData[participantTactileData$date==dates[d],]
      givenDate$session <- d
      timePointList[[d]] <- givenDate}
    participantTactileData <- plyr::rbind.fill(timePointList)

    #Split the dataframe into the sessions ----
    sessions <- unique(participantTactileData$session)
    sessionDataList <- list()
    for(s in 1:length(sessions)){
      participantData_at_s_session <- participantTactileData[participantTactileData$session==sessions[s],]
      participantData_at_s_session$orderCompleted <- cumsum(c(0,as.numeric(diff(participantData_at_s_session$protocol))!=0)) + 1 #great a column for the completion order of the protocols
      sessionDataList[[s]] <- participantData_at_s_session
    }
    participantTactileData <- plyr::rbind.fill(sessionDataList)

    if(debugging=="on"){
      print("SECTION 2: COMPLETED")
    }

    allProtocolOutputs <- participantTactileData

    ## SECTION 3 ----

    #Accounting for discrimination tasks not subtracting the comparison stimulus (this issue is specific to the new format at some sites (i.e., University of Calgary)) ----
    if(Site == "University of Calgary"){
      allProtocolOutputs$value[grep("Amplitude Discrimination", allProtocolOutputs$protocolName)] <- allProtocolOutputs$value[grep("Amplitude Discrimination", allProtocolOutputs$protocolName)]-200
      allProtocolOutputs$value[grep("Frequency Discrimination", allProtocolOutputs$protocolName)] <- allProtocolOutputs$value[grep("Frequency Discrimination", allProtocolOutputs$protocolName)]-30
    }

    if(Site %in% c("KKI","CCH","JHU")){
      #Note to self, this is a temporary brute force fix for the problem that we have where the number of trials completed by protocol are NOT equal
      #I will eventually need the code to recognize how many trials were completed within a given session, rather than assume it is exactly half of the total number of tirals completed
      allProtocolOutputs$sessions[allProtocolOutputs$protocolName=="Simultaneous Amplitude Discrimination"][25:52] <- 2
    }

    if(debugging=="on"){
      print("SECTION 3: COMPLETED")
    }

    # SECTION 4 (tidying up the dataframe) ----

    #Change performance column values to numeric ----
    allProtocolOutputs <- suppressWarnings(as.data.frame(allProtocolOutputs))
    allProtocolOutputs[,20:25] <- suppressWarnings(sapply(allProtocolOutputs[,20:25], suppressWarnings(as.character))) #supressWarnings is on because some values are already NA and then turn into NA
    allProtocolOutputs[,20:25] <- suppressWarnings(sapply(allProtocolOutputs[,20:25], suppressWarnings(as.numeric)))

   #move the session column to be earlier in the dataframe
   # allProtocolOutputs <- allProtocolOutputs[, c(1:10, 31, 11:30)]


    #Save the extracted file for each participant in the output directory -----
    currentDirectory <- getwd() #remember the current wd
    setwd(outputDirectory) #setwd to the outputDirectory
    write.csv(allProtocolOutputs, file = paste0("BATD_extracted_", list_of_filenames[p],"_NF.csv")) #save the output of all the protocols for each participant as a csv
    setwd(currentDirectory) #return to the currentDirectory
    allParticipantsOutput[[p]] <- as.data.frame(allProtocolOutputs)
    print(paste("Extracted participant:", id))
  }

  if(debugging=="on"){
    print("SECTION 4: COMPLETED")
  }

  ## SECTION 5 ----
  allParticipantsOutput_combined <-  as.data.frame(data.table::rbindlist(allParticipantsOutput, fill = TRUE))
  setwd(inputDirectory)
  dir.create("combined", showWarnings = FALSE) #set the wd to the folder where you wish to save the combined data to
  combinedDirectory <- paste0(inputDirectory,"/combined") #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.
  setwd(combinedDirectory)
  write.csv(allParticipantsOutput_combined, file = "BATD_extracted_combined.csv")
  setwd(inputDirectory)

  if(debugging=="on"){
    print("SECTION 5: COMPLETED")
  }

  print(paste0("Combined extracted data saved in:", combinedDirectory))

  return(allParticipantsOutput_combined)

}
