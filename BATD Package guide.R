#BATD Package guide -----

#Section 1: Introduction and setup ----
  #'Note that currently, you will have to set your own working directories, but I will eventually create 'dummy' working directories that you can read off github
  #'Install the package devtools first by removing the front hashtag, then initiate the library
  #'Once devtools has been installed and initiated, remove the hashtag from the line starting with 'install_github...' and install the latest version of the package
  #'Initiate the BATD library and then follow the rest of the steps below - see section 2

#Setup ----
#install.packages("devtools") #need to install the packages devtools first if this doesn't already exist (allows for install_github)
devtools::install_github("HeJasonL/BATD", force = TRUE) #install the latest version of the package
library(BATD)

#Section 2: Guide ----
#' To test or to use the BATD package, I've provided a series of guided steps below that demonstrate how the package works
#' A general way to think about this is the package has three steps:
#' Extract, Analyze and Plot
#' Extract is necessary to run both Analyze and Plot
#' Extract works on the raw data and extracts the relevant variables from those raw data
#' Analyze works on the dataframe produced by the 'Extract' functions and produces the relevant performance outcomes
#' Plot works on the dataframe produced by the 'Extract' functions and produces the relevant performance outcomes

#Functions ----
#The functions below are the functions we recommend to users. To get more information about these functions, simply run them with the question mark in front (as they currently are below)
?BATD_extract_OF #used to extract data saved by old verisons of the CM tactile stimulator
?BATD_extract_NF #used to extract ddata saved by new versions of the CM tactile stimulator (e.g., Brain Gauge)
?BATD_analyze_all ##used to analyze a combined dataframe containing performance from multiple participants (i.e., a dataframe produced by BATD_analyze_all)
?BATD_plot_all #used to plot a combined dataframe containing performance from multiple participants (i.e., a dataframe produced by BATD_analyze_all)

#Other functions (not recommended) ----
#These other functions are used by the functions above. While they work, we do not recommend them to be used.
#?BATD_plot #used to plot a single dataframe for a single participant produced by BATD_extract_XX
#?BATD_analyze #used to analyze a single dataframe for a single participant produced by BATD_extract_XX
library(BATD)
## STEP 1: BATD_extract for both old and new formats ----
#Old Format
setwd("") #setwd to the folder containing all the raw .txt files
participants_OF_list <- list.files(pattern = "-") #list the txt files containing participant's performance
OF <- BATD_extract_OF(participants_OF_list, "KKI") #run BATD_extract_XX and assign the output to a dataframe
BATD_analyze_all(OF)

#New Format
setwd("") #setwd to the folder containing all the folders which contain participant's performance
participants_NF_list <- list.files(pattern = "-") #list the txt files containing participant's performance
NF <- BATD_extract_NF(participants_NF_list[1:3], "Site") #run BATD_extract_XX and assign the output to a dataframe
BATD_analyze_all(NF)

setwd("~/Dropbox/Documents/Data repository/Tactile Data/Raw/New Format/Toronto/ARBA1")
participants_from_ARBA1 <- list.files(pattern = "-") #list the txt files containing participant's performance
ARBA1 <- BATD_extract_NF(participants_from_ARBA1[1:3], "ARBA1")
BATD_analyze_all(ARBA1)

## STEP 2: BATD_analyze_all for old and new formats ----
BATD_analyze_all(OF) #Old format
BATD_analyze_all(NF) #New format

## STEP 3: Examples of BATD_plot_all ----
BATD_plot_all(OF) #Old format
BATD_plot_all(NF) #New format





