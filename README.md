# Batch analysis of tactile data (BATD)

BATD is a package written by Jason He while he was at Johns Hopkins University and King's College London under the supervision of Dr. Nicolaas Puts. The code is intended to take the raw text files produced by Brain Gauage, an app-based program used to administer vibrotactile psychophysical protocols (see [link](https://www.corticalmetrics.com/)). The functions contained in this package require working knowledge of R programming. I recognise the code can be unclear at times (though it is heavily annotated). If you need help, please feel free to email me at jasonhe93@gmail.com and I will endeavour to solve your problem. 

BATD contains three key functions:

1. BATD_extract_NF
2. BATD_analyze_all
3. BATD_plot_all


## The Basics 

**BATD_extract_NF** is intended to turn the raw text file (".txt") produced by Brain Gauge into a comma-separated value dataframe that you can work with in R. The function has two arguments:
* **list_of_filenames** - the name of the raw .txt files you wish to analyze. A simple strategy would be to use the list.files() function that is native to R to identify the files that you wish to analyze and then assigning that list to a variable that you then feed into this function. 
* **site** - the name of the site that you are from. It is assumed that anyone using this code is someone that I have been in contact with. If I have been in contact with you, I will have edited the script so that it accounts for the specific protocols that were delivered through Brain Gauge at your _site_. However, if I have not been in contact with you and you wish to use this code, you can simply state your site argument as "NA". 

*The site argument was included because Brain Gauge has different protocol numbers that relate to different protocol names. Sometimes, the same protocol number will be used for different protocols, making it quite difficult for me to discern what the protocol that was delivered actually was. I have created a folder which contains the site specific protocols and their corresponding protocl names. This folder is accessible as part of this package.*

BATD_extract_NF returns a dataframe (which can be saved as a .csv file using base R function "read.csv") with each column being either a participant or task characteristic and each row being a different trial. 

**BATD_analyze_all** is intended to turn the extracted data (i.e., the text data that went through the BATD_extract_NF function) into a dataframe that has been analyzed. This analyzed dataframe will have a separate row for each participant, and a separate row for each run (a run is considered separate if a participant did two runs of the same protocol in the same session). Separate rows are also included for separate sessions (e.g., if a participant came in on different days). 

The dataframe produced by BATD_analyze_all can also be saved using "read.csv". 

**BATD_visualize_all** is intended to visualize the extracted data. It produces plots which are displayed in the plot panel of R studio (for each participant), and can also be saved. 



