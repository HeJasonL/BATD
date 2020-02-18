#' BATD_extract_OF
#'
#' Takes a list of filenames in a working directory which contain the performance data recorded from Cortical Metrics. These performance files are typically .txt files. "BATD_extract_NF" was specifically designed to analyze data from the Brain Gauge Research Editions and any version prior.
#' For more information about Cortical Metrics Brain Gauage, see: https://www.corticalmetrics.com/braingaugeresearch. BATD_extract_OF requires two inputs, with the first being the list of participants and the second being a string variable referring to the site where the data was collected.
#' While both "BATD_extract_NF" and "BATD_extract_OF" return a combined dataframe for all the participants that have had their data extracted, the individual and combined files are saved in folders created in the original working directory.
#' In the working directory, folders 'output' and 'combined' will be created to store the individual files that were extracted and a file containing all the individual files combined respectively. These files are in .csv format.
#'
#' @param list_of_filenames a list object containing the filenames of the .txt files containing participant performance from Brain Gauge
#' @param Site refers to the site where the data was collected. In the past and in the future, the original codes will be patched for different sites. For Site, put the site location inbetween two quotation marks. For e.g., "JHU" refers to the Johns Hopkins University
#'
#' @return BATD_extract_OF returns a dataframe containing the extracted data for each participant. Protocols and protocol names are provided as columns, along with other relevant metrics.
#'
#' @examples
#' currently NA
#'
#' @export

BATD_extract_OF <- function(list_of_filenames, Site){
  inputDirectory <- getwd()
  dir.create("output", showWarnings = FALSE) #set the wd to the folder where you wish to save the output data to
  outputDirectory <- paste0(inputDirectory,"/output") #automatically creates a folder in that directory named 'output' - if you already have a folder named output, ignore this code.

  #Create external lists ----
  allParticipantsOutput <- list() #creates a list to store the output containing all the output from all the protocols combined (outer loop)
    nthProtocolOutputList <- list() #creates a list to store the output from the nth protocol (middle loop)
       protocolOutput_for_givenSession <- list()  #creates a list to store the output from a given session (inner loop )

  #For loop through the participants identified in the inputDirectory ----
    for(p in 1:length(list_of_filenames)){
    list_of_filenames <- list.files(pattern = "-")

    #set the working directory to the participant directory and identify the protocols completed by participant[p] ----
     setwd(paste0(inputDirectory,"/", list_of_filenames[p])) #setwd to participant's folder [this folder contains another folder named as the date of when the particpant completed the task]
     participantsFolder <- getwd()
     filesinFolder <- list.files(pattern = "-") #We want to specify the folder named as the date of testing and nothing else (it must have have a "-" that splits the dates)
     datefileinFolder <- filesinFolder[!grepl(".csv",filesinFolder)] #Then, we want to make sure we don't pick up any .csvs (which also tends to be present in these folders)
     setwd(paste0(participantsFolder, "/", datefileinFolder)) #set the working directory to the folder (datefileinFolder) which contains all the protocols
     protocolsFolder <- getwd()

     protocolsinfolder<- list.files() #list all the files in the protocolsFolder
     protocolsinprotocolsFolder <- protocolsinfolder[!stringr::str_length(protocolsinfolder)>3] #subset to only the folders with a str length of 3, since all protocols have a 3 digit identifier

          #For loop through the protocols in the protocolsFolder and extract the participant/protocol/performance details ----
            for(n in 1:length(protocolsinprotocolsFolder)){
            setwd(paste0(protocolsFolder,"/", protocolsinprotocolsFolder[n])) #set then set the directory to the n^th protocol
              protocolOutputs <- list.files(pattern="1-") #identify the particpant files (the pattern "1-" refers to the protocol output files)

                if(identical(list.files(pattern = ".txt"), character(0))){next} #This is a short piece of code to test whether the folder has any items within the folder
                  #This needs to exist because while all the folders for each protocol are present, the participant may not have completed the protocol (i.e., the folder is empty)
                  #This is an issue specific to the old format, the 'next' just skips the current protocol within the loop

                      #For loop through the sessions (s), since some participants completed more than one session or attempt for a given protocol ----
                        for(s in 1:length(protocolOutputs)){ #The following is nested in a for loop because some participants may have completed a given protocol more than once (s stands for sesson here)
                        output <- read.csv(protocolOutputs[s], header = FALSE, sep = "\t") #read in the current protocol for the current session (s), and assign it to a dataframe named output

                        # (1) Extract the participant details ----
                        id <- as.character(output$V2[output$V1=="Subject_Number"])
                        race <- as.character(output$V2[output$V1=="Race"])
                        gender <- as.character(output$V2[output$V1=="Gender"])
                        handedness <- as.character(output$V2[output$V1=="Handedness"])
                        birthYear <- as.character(output$V2[output$V1=="Birthdate"])
                        date <- gsub("/","-",as.character(output$V2[output$V1=="Date"]))
                        participantDetails <- as.data.frame(cbind(id, date, race, gender, handedness, birthYear)) #Combine participant details into a dataframe


                        #adjust the participant details so the strings line up with how they are referred to in the new format
                        if(participantDetails$race=="Default"){participantDetails$race <- "caucasian"} else {participantDetails$race <- participantDetails$race} #in the old format, caucasians were referred to as default, but in the new format, caucasians are just referred to as caucasians
                        if(participantDetails$gender=="M"){participantDetails$gender <- "Male"} else {participantDetails$gender <- "Female"} #In the new format, M and F are written in full (note, prefer not to say was not an option in either format)
                        if(participantDetails$handedness=="R"){participantDetails$handedness <- "Right"} else {participantDetails$handedness <- "Left"} #R is fully expressed Right, and L as Left in the new output
                        participantDetails$birthYear <- stringr::str_sub(participantDetails$birthYear,-4,-1) #birthYear is presented as just year (this gets the last four numbers )

                    # (2) Protocol details ----
                    protocolDetails <- as.data.frame(output$V2[output$V1=="Protocol_ID"]) #protocol id
                    protocolDetails$originalFilename <- protocolOutputs[s]
                    colnames(protocolDetails)[1] <- "protocol"
                    protocolDetails$numberofPracticeTrials <- "0"
                    protocolDetails$numberofTestTrials <- output$V2[output$V1=="Number_of_Trials"]
                    protocolDetails$ISI <- output$V2[output$V1=="Interval_b/w_Adaptor_and_Test"]
                    protocolDetails$stim1amplitude <- output$V2[output$V1=="Stimulus_1_amp"]

                    #The below is self-explanatory, but in case readers are curious,
                    #the code below checks to see whether stimulus 1 and/or 2 amplitudes are present in the output column V1,
                    #if it is true, the code then extracts the relevant value, otherwise it make sthat value NA
                    if("Stimulus_2_amp" %in% output$V1){protocolDetails$stim2amplitude <- output$V2[output$V1=="Stimulus_2_amp"]} else {protocolDetails$stim2amplitude <- NA}
                    if("Adapting_Stimulus_1_amp" %in% output$V1){protocolDetails$astim1amplitude <- output$V2[output$V1=="Adapting_Stimulus_1_amp"]} else {protocolDetails$astim1amplitude <- NA}
                    if("Adapting_Stimulus_2_amp" %in% output$V1){protocolDetails$astim1amplitude <- output$V2[output$V1=="Adapting_Stimulus_2_amp"]} else {protocolDetails$astim2amplitude <- NA}

                    # (3) Extract Performance details ----
                    performanceDetails <- list.files(pattern="2-") #identify the participantPerformance file
                    performanceDetails <- read.csv(performanceDetails[s], header = TRUE, sep = "\t")[,1:4] #the last hard bracket section removes a column with just NAs
                    correctResponse <- suppressWarnings(as.character(as.numeric(performanceDetails$Response[performanceDetails$Correct==1][1])))
                    incorrectResponse <- 0
                    performanceDetails$expected <- ifelse(performanceDetails$Correct==1, correctResponse, incorrectResponse)
                    performanceDetails$trialNumber <- 1:nrow(performanceDetails)


                    #Combine the participant details, protocol details and performance details and put into protocolOutput_for_givenSession list ----
                    allProtocolDetails <- cbind(participantDetails, protocolDetails, performanceDetails)
                    allProtocolDetails$session <- s #this adds in a column called session, which is 'i' in the for current for loop
                    protocolOutput_for_givenSession[[s]] <- allProtocolDetails #Put the subProtocolOutput into the external list
                  }

      X <- data.table::rbindlist(protocolOutput_for_givenSession, fill = TRUE) #row binds the dataframes in protocolOutput_for_givenSession (usually this is just a single file, since most participants only do one session)
      nthProtocolOutputList[[n]] <- X

      }

      allProtocolOutputs <- data.table::rbindlist(nthProtocolOutputList, fill = TRUE) #row binds the dataframes in nthProtocolOutputList (usually this is just a single file, since most participants only do one session)

      #Label protocols with names (note that this is done in a specific order since sometimes protocols share numeric codes ----
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==801] <- "Simple Reaction Time"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==800] <- "Choice Reaction Time"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==100 & allProtocolOutputs$stim1amplitude==20 & is.na(allProtocolOutputs$stim2amplitude)] <- "Static Detection Threshold"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==713] <- "Dynamic Detection Threshold"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==100 & allProtocolOutputs$stim1amplitude==100 & allProtocolOutputs$stim2amplitude==200] <- "Amplitude Discrimination Threshold without Adaptation"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==220 & allProtocolOutputs$stim1amplitude==200 & allProtocolOutputs$stim2amplitude==200 & allProtocolOutputs$ISI == 0] <- "Sequential Frequency Discrimination"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==200 & allProtocolOutputs$stim1amplitude==200 & allProtocolOutputs$stim2amplitude==200 & allProtocolOutputs$ISI == 0] <- "Simultaneous Frequency Discrimination"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==109 & allProtocolOutputs$stim1amplitude==15 & is.na(allProtocolOutputs$stim2amplitude) & allProtocolOutputs$ISI == 30] <- "Static Detection Threshold with Adaptation ISI 30"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==109 & allProtocolOutputs$stim1amplitude==15 & is.na(allProtocolOutputs$stim2amplitude) & allProtocolOutputs$ISI == 100] <- "Static Detection Threshold with Adaptation ISI 100"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==109 & allProtocolOutputs$stim1amplitude==100 & allProtocolOutputs$stim2amplitude==200 & allProtocolOutputs$astim1amplitude==100 & allProtocolOutputs$astim1amplitude==100 & allProtocolOutputs$ISI == 1000] <- "Amplitude Discrimination with Single Site Adaptation"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==109 & allProtocolOutputs$stim1amplitude==100 & allProtocolOutputs$stim2amplitude==200 & allProtocolOutputs$astim1amplitude==100 & allProtocolOutputs$ISI == 100] <- "Amplitude Discrimination with Single Site Adaptation"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==350 & allProtocolOutputs$stim1amplitude==300 & allProtocolOutputs$stim2amplitude==300 & allProtocolOutputs$ISI == 0] <- "Duration Discrimination"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==300] <- "Temporal Order Judgement"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==301] <- "Temporal Order Judgement with Carrier"
        allProtocolOutputs$protocolName[allProtocolOutputs$protocol==103] <- "Amplitude Discrimination with Dual Site Adaptation"

      #Rename the columns so that they correspond to the new format (this is to make the column names consistent with the column names provided by BATD_extract for the new format (i.e., NF)) ----
        names(allProtocolOutputs)[names(allProtocolOutputs) == 'Response_time'] <- 'responseTime'
        names(allProtocolOutputs)[names(allProtocolOutputs) == 'Val'] <- 'value'
        names(allProtocolOutputs)[names(allProtocolOutputs) == 'Response'] <- 'response'
        names(allProtocolOutputs)[names(allProtocolOutputs) == 'Correct'] <- 'correctResponse'

      #change performance column values to numeric ----
        allProtocolOutputs <- as.data.frame(allProtocolOutputs)
        allProtocolOutputs[,9:21] <- suppressWarnings(sapply(allProtocolOutputs[,9:21], suppressWarnings(as.character))) #supressWarnings is on because some values are already NA and then turn into NA
        allProtocolOutputs[,9:21] <- suppressWarnings(sapply(allProtocolOutputs[,9:21], suppressWarnings(as.numeric)))


      #Create column(s) to detail the extraction process ----
        allProtocolOutputs$site <- Site
        allProtocolOutputs$format <- "OF" #Specify that the format of the data is the old format
        allProtocolOutputs$extractedBy <- "BATD V.1.3" #Specify that the data was extracted by BATD version V.X.X

        #Save the extracted file for each participant in the output directory -----
        print(paste("Extracting participant:", list_of_filenames[p]))
        currentDirectory <- getwd() #remember the current wd
        setwd(outputDirectory) #setwd to the outputDirectory
        write.csv(allProtocolOutputs, file = paste0("BATD_extracted_", list_of_filenames[p],"_OF.csv")) #save the output of all the protocols for each participant as a csv
        setwd(inputDirectory) #(re)set the input directory

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








