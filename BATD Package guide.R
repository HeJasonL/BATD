#BATD Package guide -----
#' readme
#' Currently, you will have to set your own working directories, but I will eventually create 'dummy' working directories that you can read off github

#Section 1: Setup ----
#' 1. Install the package devtools and load it into the Global Environment.
  #' To do, you will need to remove the hashtag in front of install.packages'devtools' on line #13. Once installed, run line #14 to load it into your Global Environment
#' 2. Install BATD
  #' Once devtools has been installed and initiated, remove the hashtag in front of install_github "HeJasonL/BATD, force = TRUE" to install BATD, run line #16 to load BATD into your Global Environment


#Setup ----
#install.packages("devtools") #need to install the packages devtools first if this doesn't already exist (allows for install_github)
library(devtools) #initiate devtools package
#install_github("HeJasonL/BATD", force = TRUE) #install the latest version of the package
library(BATD)

#Section 2: Guide ----
#' To test or to use the BATD package, I've provided a series of guided steps below that demonstrate how the package works
#' Once you're familiarized with the functions, you won't need to use this script to run BATD
#'
#' A good way to think about BATD is that the package provides 3 'steps' to extract/analyze/plot your vibrotactile data
#'
#' Extract, Analyze and Plot:
#'
#' BATD_extract_OF and BATD_extract_NF is used on the raw data to 'extract' the primary variables of interest, and is necessary to subsequently run both BATD_analyze_all and BATD_plot_all
#' BATD_analyze_all works on the dataframe produced by either of the extract functions and produces the relevant performance outcomes
#' BATD_plot_all also works on the dataframe produced by either of the extract functions and saves plots of the protocols completed by each participant. Note, these plots are not saved to the Global Environment. They will appear in your working directory.

#The functions of BATD and their descriptions ----
#The functions below are the functions we recommend to users. To get more information about these functions, simply run them with the question mark in front (as they currently are below)
?BATD_extract_OF #used to extract data saved by old verisons of the CM tactile stimulator
?BATD_extract_NF #used to extract ddata saved by new versions of the CM tactile stimulator (e.g., Brain Gauge)
?BATD_analyze_all ##used to analyze a combined dataframe containing performance from multiple participants (i.e., a dataframe produced by BATD_analyze_all)
?BATD_plot_all #used to plot a combined dataframe containing performance from multiple participants (i.e., a dataframe produced by BATD_analyze_all)
  #Other functions (not recommended) ----
#These other functions are used by the functions above. While they work, we do not recommend them to be used.
#?BATD_plot #used to plot a single dataframe for a single participant produced by BATD_extract_XX
#?BATD_analyze #used to analyze a single dataframe for a single participant produced by BATD_extract_XX


## STEP 1: BATD_extract for both old and new formats ----
#You will likely only have to use either BATD_extract_OF or BATD_extract_NF, not both.
#BATD_extract_NF is used for data collected using the Brain Gauge
#BATD_extract_OF is used for data collected using the older CM3, CM4 stimulators
#By default, I have commented out the old format since most people using this script will likely be using the new format

#Old Format
# setwd("") #setwd to the folder containing all the raw .txt files
# participants_OF_list <- list.files(pattern = "-") #list the txt files containing participant's performance
# OF <- BATD_extract_OF(participants_OF_list, "KKI") #run BATD_extract_XX and assign the output to a dataframe
# BATD_analyze_all(OF)

#New Format
setwd("") #setwd to the folder containing all the folders which contain participant's performance
participants_NF_list <- list.files(pattern = "-") #list the txt files containing participant's performance
NF <- BATD_extract_NF(participants_NF_list[1], "Site") #run BATD_extract_XX and assign the output to a dataframe
BATD_analyze_all(NF)

## STEP 2: BATD_analyze_all for old and new formats ----
# BATD_analyze_all(OF) #Old format
BATD_analyze_all(NF) #New format

## STEP 3: Examples of BATD_plot_all ----
# BATD_plot_all(OF) #Old format
BATD_plot_all(NF) #New format


