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

baseDirectory <- getwd()
dir.create("Plots", showWarnings = FALSE) #Creates a directory to put the combined .csv file into
setwd(paste0(baseDirectory,"/Plots")) #Switch to a folder to save the plots

  #Setup -----
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

  sessions <- unique(data$session) #Identifies the number of sessions completed outside of the loop
  plots_of_protocols_completed <- list() #Create a list for the plots of protocols completed to be put in
  plots_of_protocols_by_session <- list() #creates a list for the plots by session to go in

#For loop across sessions completed by a given participant ----
  for(s in 1:length(sessions)){
    temp <- data[data$session==sessions[s],] #Specify to the session
    protocols_completed <- unique(temp$protocolName) #List the protocols completed
    plots_of_protocols_completed <- list() #Create a list for the plots of protocols completed to be put in

#For loop across protocols completed by a given participant ----
    for(completed in protocols_completed){
      Data <- temp[temp$protocolName==completed,] #Reset the Data to the relevant protocol
      Analyzed_data <- BATD_analyze(Data) #BATD_analyze the data to get the performance metrics
      colnames(Analyzed_data) <- gsub("_.*", "",colnames(Analyzed_data)) #strip the tags (makes universally consistent across protocols)
      #Value and response plots ----
      value_plot <- ggplot2::ggplot(Data, ggplot2::aes(x=trialNumber, y=value, color = as.factor(correctResponse))) +
        ggplot2::geom_point(size =3) + ggplot2::scale_colour_manual(values = cols) + theme_JH +
        ggplot2::labs(y = expression("Amplitude"~(mu*m)), x = "Trial Number") + #note that this needs to be estimated
        ggplot2::geom_hline(yintercept = Analyzed_data$threshold) #Note that this needs to be estimated as threshold

      response_plot <- ggplot2::ggplot(Data, ggplot2::aes(x=trialNumber, y=responseTime, color = as.factor(correctResponse))) +
        ggplot2::geom_point(size =3) + ggplot2::scale_colour_manual(values = cols) + theme_JH +
        ggplot2::labs(y = expression("Response Times"), x = "Trial Number") + #note that this needs to be estimated
        ggplot2::geom_hline(yintercept=Analyzed_data$medianRT, linetype = "dashed") #Note that this needs to be estimated as mean RT

      #Text plots ----
        plot_text_SRT_CRT <- paste("Mean RT:", round(as.numeric(Analyzed_data$meanRT), digits = 2), "\n", "\n",
                           "Median RT:", round(Analyzed_data$medianRT, digits = 2), "\n",
                           "Accuracy:", round(Analyzed_data$accuracy, digits = 2), "% \n", "\n")

        plot_text_thresholds <- paste("\n","Threshold:", round(as.numeric(Analyzed_data$threshold),digits = 2),"\n",
                                      "Reversals:", Analyzed_data$reversals, "\n",
                                      "\n", "\n", "\n", "\n", "\n", "\n", #Breaks up the top and bottom of the texts (could do two separate plots but not efficient)
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

        text_plot_thresholds <- ggplot2::ggplot() + ggplot2::theme_bw() +
         ggplot2::annotate("text", x = 4, y = 20, label = plot_text_thresholds,) +
         ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                        panel.grid.minor=ggplot2::element_blank(),
                        panel.border=ggplot2::element_blank(),
                        axis.text = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank())

      #Combine plots for a given protocol ----
      plot_pair <- ggpubr::ggarrange(value_plot, response_plot, ncol = 1, nrow = 2)
      if(Data$protocolName[1] %in% c("Simple Reaction Time","Choice Reaction Time")){
      plots_combined <- ggpubr::ggarrange(response_plot, text_plot_SRT_CRT, ncol = 1, nrow = 2)
      }
      else{
      plots_combined <- ggpubr::ggarrange(plot_pair, text_plot_thresholds, ncol = 2, nrow = 1)
      }
      annotated_plots <- ggpubr::annotate_figure(plots_combined, top = ggpubr::text_grob(Data$protocolName[1], color = "black", face = "bold", size = 12))
      plots_of_protocols_completed[[completed]] <- annotated_plots

    }

    all_plots_combined <- ggpubr::ggarrange(plotlist=rev(plots_of_protocols_completed),#There are many ways to do this,
                                            widths = c(1,1),  #but here you can arrange all the plots from the list you put plots in earlier
                                            common.legend = FALSE, #common legend
                                            ncol = 4,
                                            nrow = 3,
                                            align = "hv")

    plots_of_protocols_by_session[[s]] <- all_plots_combined
  }

  #Save the plots completed by a given participant, containing all the completed protocols, for both sessions --------
  for(a in 1:length(sessions)){
    id <- temp$id[1]
    session <- temp$sessions[1]
    plot <- plots_of_protocols_by_session[[a]]
    plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(paste0("Participant: ",temp$id[1], "; Session: ", a), color = "black", face = "italic", size = 15))
    plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Batch Analysis of Tactile Data (BATD)", color = "black", face = "bold", size = 20))
    ggplot2::ggsave(filename = paste0(id,"_","session_",a,".pdf"), plot = plot, width = 20, height = 12 )
  }

  setwd(baseDirectory) #Return to the baseDirectory

}



