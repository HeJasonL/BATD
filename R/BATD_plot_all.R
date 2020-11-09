#' BATD_plot_all
#'
#' Plot the extracted data from "BATD_extract_NF" or "BATD_extract_OF" for multiple participants. This function is practically the same as BATD_analyze, but applies it to multiple participants within a for loop.
#' We recommend users to always use "BATD_plot_all", even for single participants.
#'
#' @param x Numeric vector.
#'
#' @return Factor variable.
#'
#' @examples
#'
#' @export

BATD_plot_all <- function(extracted_Data){

  ##Version
  Version <- c("BATD_V.1.6")

  #DEBUGGING ----
  debugging <- "on"
  if(debugging=="on"){
    setwd(here("POND Data", "ARBA1"))
    extracted_Data <- ARBA1[ARBA1$id=="pond-0869",]

  }

  #Setup ----
  baseDirectory <- getwd()
  dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
  setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots
  extracted_Data <- extracted_Data[!is.na(extracted_Data$id),]
  extracted_Data <- extracted_Data[!is.na(extracted_Data$protocolName),]

  uniqueParticipants <- unique(extracted_Data$id)
  uniqueParticipants <- uniqueParticipants[!is.na(uniqueParticipants)]

  participants_outPut_list <- list()

  for(x in 1:length(uniqueParticipants)){

    print(paste0("Now plotting participant:", uniqueParticipants[x]))
    data <- extracted_Data[extracted_Data$id==uniqueParticipants[x],]
    uniqueSessions <- sort(unique(data$session)) #identify the number of unique sessions

    list_of_all_plots_for_a_given_session <- list()
    for(s in uniqueSessions){

      sessionData <- data[data$session==s,]

      #identify the number of unique runs
      uniqueRuns <- unique(sessionData$run)
      list_of_all_plots_for_a_given_run <- list()

      for(r in 1:length(uniqueRuns)){
        r <- 1
        sessionData_for_given_run <- sessionData[sessionData$run==uniqueRuns[r],]

        plot <- BATD_plot(sessionData_for_given_run)

        plot_with_run_header <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(paste0("Run: ", uniqueRuns[r]), color = "black", face = "italic", size = 15))
        plot_with_session_header <- ggpubr::annotate_figure(plot_with_run_header, top = ggpubr::text_grob(paste0("Participant: ", uniqueParticipants[x], "; Session: ", uniqueSessions[s], " (", sessionData$date, ")"), color = "black", face = "italic", size = 15))
        plot_with_all_headers <- ggpubr::annotate_figure(plot_with_session_header, top = ggpubr::text_grob("Batch Analysis of Tactile Data (BATD)", color = "black", face = "bold", size = 20))

        list_of_all_plots_for_a_given_run[[r]] <- plot_with_all_headers
      }#end of runs loop

      list_of_all_plots_for_a_given_session[[s]] <- list_of_all_plots_for_a_given_run

    }#end of sessions loop

    pdf(paste0(uniqueParticipants[x],".pdf"), onefile = TRUE, width = 20, height = 12)
    print(list_of_all_plots_for_a_given_session)
    dev.off()

  } #end of participants loop
  setwd(baseDirectory)

}
