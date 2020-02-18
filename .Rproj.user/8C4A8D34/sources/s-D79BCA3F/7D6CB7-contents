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
  inputDirectory <- getwd()
  dir.create("output", showWarnings = FALSE) #set the wd to the folder where you wish to save the output data to
  outputDirectory <- paste0(inputDirectory,"/output") #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.

  #Create external lists -----
  allParticipantsOutput <- list()

  #For loop through the participants identified in the inputDirectory ----
  for(p in 1:length(list_of_filenames)){
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
      protocol <- as.character(output$V2[output$V1=="protocol"])
      date <- substr(gsub("T","",(output$V2[output$V1=="date"][1])), 1,10)
      numberofPracticeTrials <- as.character(output$V2[output$V1=="numTrials"][1])
      numberofTestTrials <- as.character(output$V2[output$V1=="numTrials"][2])
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

      protocolDetails <- as.data.frame(cbind(protocol, date, numberofPracticeTrials, numberofTestTrials, ISI,
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

      All <- cbind(protocolDetails, participantDetails, performanceDetails)

      All$trialNumber <- 1:nrow(All)

      ProtocolOutputList[[i]] <- All
    }

    participantTactileData <- do.call(rbind.data.frame, ProtocolOutputList)

    #Change correctResponse to a 0 or 1 numeric (currently its in true or false, I just want to standardise this between the old and new format, also string descriptions are not useful here)
    participantTactileData$correctResponse <- suppressWarnings(as.numeric(participantTactileData$correctResponse)) #make correctResponse numeric, making true = 2, and false = 1
    participantTactileData$correctResponse <- participantTactileData$correctResponse - 1 #subtract 1, so that 1 = correct, 0 = incorrecty

    #Label protocols with names (note that this is done in a specific order since sometimes protocols share numeric codes ----
    #Note, there are far more protocols in the new format than the old format, hence the greater number of protocol numbers
    #Simple and Choice Reaction times
    participantTactileData$protocolName[participantTactileData$protocol==801] <- "Simple Reaction Time"
    participantTactileData$protocolName[participantTactileData$protocol==800] <- "Choice Reaction Time"

    #Static Detection Thresholds with and without adaptation (with ISI 30 and 100)
    participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
    participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==0] <- "Static Detection Threshold"
    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==5000] <- "Static Detection Threshold"
    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==30] <- "Static Detection Threshold with Adaptation ISI 30"
    participantTactileData$protocolName[participantTactileData$protocol==910 & participantTactileData$ISI==100] <- "Static Detection Threshold with Adaptation ISI 100"

    #Duration discrimination
    participantTactileData$protocolName[participantTactileData$protocol==350] <- "Duration Discrimination"

    #Dynamic detection threshold
    participantTactileData$protocolName[participantTactileData$protocol==713] <- "Dynamic Detection Threshold"

    #Amplitude Discrimination Threshold with and without adaptation (with single site and dual site)
    participantTactileData$protocolName[participantTactileData$protocol==900 & participantTactileData$stim2amplitude!=0] <- "Amplitude Discrimination Threshold without Adaptation"
    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==100] <- "Amplitude Discrimination with Single Site Adaptation"
    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==100] <- "Amplitude Discrimination with Dual Site Adaptation"

    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim1amplitude==200] <- "Dual Staircase Amplitude Discrimination (up)"
    participantTactileData$protocolName[participantTactileData$protocol==171 & participantTactileData$astim2amplitude==200] <- "Dual Staircase Amplitude Discrimination (down)"

    #Simultaneous and Sequential requency Discrimination
    participantTactileData$protocolName[participantTactileData$protocol==920] <- "Simultaneous Frequency Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==925] <- "Sequential Frequency Discrimination"

    #Simultaneous and sequential Amplitude Discrimination
    participantTactileData$protocolName[participantTactileData$protocol==105] <- "Simultaneous Amplitude Discrimination"
    participantTactileData$protocolName[participantTactileData$protocol==100 & participantTactileData$stim1amplitude==100] <- "Sequential Amplitude Discrimination"

    #Temporal Order Judgement with and without carrier
    participantTactileData$protocolName[participantTactileData$protocol==930] <- "Temporal Order Judgement"
    participantTactileData$protocolName[participantTactileData$protocol==931] <- "Temporal Order Judgement with Carrier"

    #Duration Discrimination
    participantTactileData$protocolName[participantTactileData$protocol==950] <- "Duration Discrimination"

    #University of Calgary patch (temporary fix)
    if(Site=="University of Calgary"){
      participantTactileData$protocolName[participantTactileData$protocol==900] <- "Sequential Amplitude Discrimination"
      participantTactileData$protocolName[participantTactileData$protocol==905] <- "Simultaneous Amplitude Discrimination"
    }

    #Accounting for session ----------------
    #Where this code differs from the old format is that sessions are not accounted for within the same folder, but rather, are accounted for posthoc (i.e., after all the data for a given participant is  combined)
    #In the old format, a participant who does the same session twice will have data stored in the same folder, whereas in the new format, this was not the case (at least at JHU/KKI)
    #There are some cases where participants completed the same protocol twice, we need to be able to differentiate whether it was their first or second attempt
    #The attempts are always in chronological order (again, at least at JHU and KKI)
    #This code below is really inefficient, and was admittedly done in a hurry, this will be fixed in the next version update (nonetheless, it works)
    Table <- as.data.frame(table(participantTactileData$protocolName))
    Table <- Table[Table$Freq>25,] #Identify any of the protocols that have more than 25 trials (these must be protocols where the participant did it twice, since no protocol has more than 25 trials)
    if(nrow(Table)>0){
      ProtocolsAttemptedMoreThanOnce <- list()
      for(i in 1:length(Table$Var1)){ #for all the protocols that were completed more than once
        temp <- participantTactileData[participantTactileData$protocolName %in% Table$Var1,] #narrow dataframe to those completed more than once
        temp <- temp[temp$protocolName==Table$Var1[i],] #split off the nth protocol that was completed more than once (denoted by "i)
        sessions <- which(temp$trialNumber==1) #identify the start of the protocol (trial number one)
        list <- list() #create an external list
        for (s in 1:length(sessions)){ #for the given the number of attempts or 'sessions'
          if(is.na(sessions[2])){
            sessions[2] <- 1
          }else{
            triallength <- sessions[2] - sessions[1] #identify the trial length
          }
          tempo <- temp[sessions[s]:(sessions[s]+triallength),] #here it is just,
          tempo <- as.data.frame(tempo)[1:triallength,] #convert to dataframe and remove last row (it's ane extra row, unsure why)
          tempo$session <- s #label the session number as 's', which updates every loop
          list[[s]] <- tempo #put the reduced and labelled session into a list
        }
        temp <- data.table::rbindlist(list) #row bind the list - adding all the sessions of the same protocol together
        ProtocolsAttemptedMoreThanOnce[[i]] <- temp #put the rowbinded sessions into this list
      }

      moreThanOnce <- do.call(rbind.data.frame, ProtocolsAttemptedMoreThanOnce) #rowbind all the sessions that were completed more than once and save them as a dataframe
      once <- participantTactileData[!participantTactileData$protocolName %in% Table$Var1,] #These are the protocols which have NOT been completed more than once

    }
    if(nrow(once)>1){once$session <- 1}
    allProtocolOutputs <- as.data.frame(plyr::rbind.fill(moreThanOnce, once)) #combine sessions completed more than once with those completed just once


    #Change performance column values to numeric ----
    allProtocolOutputs <- suppressWarnings(as.data.frame(allProtocolOutputs))
    allProtocolOutputs[,20:25] <- suppressWarnings(sapply(allProtocolOutputs[,20:25], suppressWarnings(as.character))) #supressWarnings is on because some values are already NA and then turn into NA
    allProtocolOutputs[,20:25] <- suppressWarnings(sapply(allProtocolOutputs[,20:25], suppressWarnings(as.numeric)))

    #Create column(s) to detail the extraction process ----
    allProtocolOutputs$site <- Site
    allProtocolOutputs$format <- "NF" #Specify that the format of the data is the old format
    allProtocolOutputs$extractedBy <- "BATD V.1.3" #Specify that the data was extracted by BATD version V.X.X

    #Save the extracted file for each participant in the output directory -----
    print(paste("Extracting participant:", id))
    currentDirectory <- getwd() #remember the current wd
    setwd(outputDirectory) #setwd to the outputDirectory
    write.csv(allProtocolOutputs, file = paste0("BATD_extracted_", list_of_filenames[p],"_NF.csv")) #save the output of all the protocols for each participant as a csv
    setwd(currentDirectory) #return to the currentDirectory

    allParticipantsOutput[[p]] <- as.data.frame(allProtocolOutputs)
  }

  allParticipantsOutput_combined <-  as.data.frame(data.table::rbindlist(allParticipantsOutput, fill = TRUE))
  setwd(inputDirectory)
  dir.create("combined", showWarnings = FALSE) #set the wd to the folder where you wish to save the combined data to
  combinedDirectory <- paste0(inputDirectory,"/combined") #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.
  setwd(combinedDirectory)
  write.csv(allParticipantsOutput_combined, file = "BATD_extracted_combined.csv")
  setwd(inputDirectory)

  print(paste0("Combined extracted data saved in:", combinedDirectory))

  return(allParticipantsOutput_combined)

}








