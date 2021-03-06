% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BATD_analyze_all.R
\name{BATD_analyze_all}
\alias{BATD_analyze_all}
\title{BATD_analyze_all}
\usage{
BATD_analyze_all(dataframe)
}
\arguments{
\item{dataframe}{BATD_analyze all requires the dataframe created by BATD_extract}
}
\value{
like BATD_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
However, BATD_analyze_all will return a dataframe with multiple rows, referring to each participant in the dataframe entered, separated by sessions if they completed multiple sessions.
}
\description{
Analyze the extracted data from "BATD_extract_NF" or "BATD_extract_OF" for multiple participants. This function is practically the same as "BATD_analyze", but applies it to multiple participants within a for loop.
Over and above "BATD_analyze", "BATD_analyze_all" also appends information about the participant that completed the protocol, as well as parameters of the protocol itself.
For this reason, we recommend users to always use BATD_analyze_all, even for single participants.
}
\examples{
Examples are currently NA

}
