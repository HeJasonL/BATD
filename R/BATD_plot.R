#' BATD_plot
#'
#' Plot extracted data for from BATD_extract_NF or BATD_extract_OF for a single participant. This function only requires a dataframe containing the performance output of participants who have had their data extracted using either BATD_extract_NF or BATD_extract_OF.
#' BATD_plot is used to analyze the datafrom a single participant. In order to analyze data from multiple participants, we recommend users use BATD_plot_all.
#'
#' @param data referring to the dataframe extracted from BATD_extract_NF and BATD_extract_OF
#'
#' @return BATD plot does not return any plots in the R environment. Instead, BATD_plot creates a folder called plot in the users working directory, and saves a series of plots with the filename pattern of id_session_X.pdf, with id being the participant id and X being the session.
#'
#' @examples
#' Examples are currently NA
#'
#' @export

BATD_plot <- function(data){

  ##Version
  Version <- c("BATD_V.1.6")

  #DEBUGGING ----
  debugging <- "off"
  if(debugging=="on"){
    # print("Note: Debugging on")
    data <- sessionData_for_given_run
  }

  #Set themes and functions ----
  theme_JH = ggplot2::theme(
    text = ggplot2::element_text(size = 12, family = "Helvetica"),
    plot.tag = ggplot2::element_text(size = 20, face = "bold"),
    axis.title.x = ggplot2::element_text(size = 10),#, margin = unit(c(1, 0, 0, 0), "mm")),
    axis.title.y = ggplot2::element_text(size = 10, margin = ggplot2::unit(c(0, 1, 0, 0), "mm")),
    axis.text = ggplot2::element_text(size = 8),
    axis.text.x = ggplot2::element_text(angle = 0, vjust = 0, color = "black"),
    legend.title = ggplot2::element_text(size=16),
    legend.text = ggplot2::element_text(size=16),
    legend.position = "none",
    plot.title = ggplot2::element_text(lineheight=.8, face= "bold", size = 16),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "black"),
    axis.ticks = ggplot2::element_line(colour = "black"))

  cols <- c("0" = "red", "1" = "royalblue", "-" = "gray") #0's are incorrect, coded as red, and 1's are correct, coded as blue

  '%ni%' <- Negate('%in%') #create the function for %not in%

  #Setup -----
  data_to_plot <- data
  data_to_plot <- data_to_plot[!is.na(data_to_plot$protocolName),]
  protocols_completed <- as.character(unique(data_to_plot$protocolName)) #identy the protocols completed

  sessions <- unique(data_to_plot$session) #Identifies the number of sessions completed outside of the loop
  plots_of_protocols_completed <- list() #Create a list for the plots of protocols completed to be put in
  plots_of_protocols_by_session <- list() #creates a list for the plots by session to go in

#For loop across protocols completed by a given participant ----
    for(completed in protocols_completed){
      Data <- data[data_to_plot$protocolName==completed,] #Reset the Data to the relevant protocol
      Analyzed_data <- BATD_analyze(Data) #BATD_analyze the data to get the performance metrics
      colnames(Analyzed_data) <- gsub("_.*", "",colnames(Analyzed_data)) #strip the tags (makes universally consistent across protocols)

      #Temporary fix for the removal of practice trials
      Data <- Data[!is.na(Data$trialNumber),] #First, remove any rows where there are more trial numbers than you would reasonably expect
      numberofPracticeTrials <- as.numeric(as.character(Data$numberofPracticeTrials[1])) + 1 #adding one so that the trials start AFTER the n of practice trials

      #here we remove the practice trials if the n > 10, this is because some sites actually ran a whole protocol as a practice, rather than the first n-numnber of trials (usually 3)
      #If prctice trials were ran as a whole protocol, thye are just treated as a protocol
      if(numberofPracticeTrials < 9 ){
        Data <- Data[numberofPracticeTrials:nrow(Data),] #remove practice trials
        Data$trialNumber <- 1:nrow(Data) #reset trial numbers
      }

      #if the current protocol are the tactile threshold protocols:
      if(completed %ni% c("Simple Reaction Time","Choice Reaction Time")){
        #Value and response plots ----

        if(completed %in% c(
          "Simultaneous Amplitude Discrimination",
          "Sequential Amplitude Challenge",
          "SSA 2 Block",
          "Dual Staircase Amplitude Discrimination (up)",
          "Dual Staircase Amplitude Discrimination (down)",
          "Amplitude Discrimination with Single Site Adaptation",
          "Sequential Amplitude Discrimination",
          "Amplitude Discrimination with Dual Site Adaptation")){#NOTE TO SELF: ADD NOTES
          Data$value <- as.numeric(Data$value) - 100
        }

        if(grepl("Duration Discrimination", completed)){#NOTE TO SELF: ADD NOTES
          Data$value <- as.numeric(Data$value) - 500
        }

        if(completed %in% c(
          "Simultaneous Frequency Discrimination",
          "Sequential Frequency Discrimination"
        )){#NOTE TO SELF: ADD NOTES
          Data$value <- as.numeric(Data$value) - 30
        }

        value_plot <- ggplot2::ggplot(Data, ggplot2::aes(x=trialNumber, y=as.numeric(value), color = as.factor(correctResponse))) +
          ggplot2::geom_point(size =3) + ggplot2::scale_colour_manual(values = cols) + theme_JH +
          ggplot2::labs(y = "Value", x = "Trial Number") + #note that this needs to be estimated
          ggplot2::geom_hline(yintercept = Analyzed_data$threshold) #Note that this needs to be estimated as threshold

        plot_text_thresholds <- paste("\n","Threshold:", round(Analyzed_data$threshold, digits = 2),"\n",
                                      "Reversals:", Analyzed_data$reversals, "\n",
                                      "\n", "\n", "\n", "\n", "\n", "\n", #Breaks up the top and bottom of the texts (could do two separate plots but not efficient)
                                      "Median RT:", round(Analyzed_data$medianRT, digits = 2), "\n",
                                      "Accuracy:", round(Analyzed_data$accuracy, digits = 2), "% \n", "\n")

        text_plot_thresholds <- ggplot2::ggplot() + ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 4, y = 20, label = plot_text_thresholds,) +
          ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                         panel.grid.minor=ggplot2::element_blank(),
                         panel.border=ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.title = ggplot2::element_blank())
      }

      response_plot <- ggplot2::ggplot(Data, ggplot2::aes(x=trialNumber, y=as.numeric(as.character(responseTime)), color = as.factor(correctResponse))) +
        ggplot2::geom_point(size =3) + ggplot2::scale_colour_manual(values = cols) + theme_JH +
        ggplot2::labs(y = expression("Response Times"), x = "Trial Number") + #note that this needs to be estimated
        ggplot2::geom_hline(yintercept= Analyzed_data$medianRT , linetype = "dashed") #Note that this needs to be estimated as mean RT

      #Text plots ----
        plot_text_SRT_CRT <- paste("Mean RT:", round(as.numeric(Analyzed_data$meanRT), digits = 2), "\n", "\n",
                           "Median RT:", round(Analyzed_data$medianRT, digits = 2), "\n",
                           "Accuracy:", round(Analyzed_data$accuracy, digits = 2), "% \n", "\n")

        text_plot_SRT_CRT <- ggplot2::ggplot() + ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 4, y = 20, label = plot_text_SRT_CRT,) +
          ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                         panel.grid.minor=ggplot2::element_blank(),
                         panel.border=ggplot2::element_blank(),
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.title = ggplot2::element_blank())

      #Combine plots for a given protocol ----
      #if the protocols are not simple or choice reaction time, then plot the value_plot and response_plot
      if(Data$protocolName[1] %ni% c("Simple Reaction Time","Choice Reaction Time")){
        value_plot <- ggpubr::ggarrange(value_plot, response_plot, ncol = 1, nrow = 2)
        plots_combined <- ggpubr::ggarrange(value_plot, text_plot_thresholds, ncol = 2, nrow = 1)}

      #if the protocols are simple and choice reaction time, then plot the response_plot and text_plot_SRT_CRT plot
      if(Data$protocolName[1] %in% c("Simple Reaction Time","Choice Reaction Time")){
      plots_combined <- ggpubr::ggarrange(response_plot, text_plot_SRT_CRT, ncol = 1, nrow = 2)}

      annotated_plots <- ggpubr::annotate_figure(plots_combined,
                                                 top = ggpubr::text_grob(Data$protocolName[1],
                                                 color = "black", face = "bold", size = 12))

      plots_of_protocols_completed[[completed]] <- annotated_plots
    }

    if(data$site[1]=="University of Calgary"){
      plots_of_protocols_completed <- rev(plots_of_protocols_completed)
    }

    all_plots_combined <- ggpubr::ggarrange(plotlist=plots_of_protocols_completed,#There are many ways to do this,
                                            widths = c(1,1),  #but here you can arrange all the plots from the list you put plots in earlier
                                            common.legend = FALSE, #common legend
                                            ncol = 4,
                                            nrow = 3,
                                            align = "hv")

  return(all_plots_combined)
}
