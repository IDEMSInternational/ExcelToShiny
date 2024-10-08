% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_shet.R
\name{download_sheet}
\alias{download_sheet}
\title{Create a Download Sheet for Shiny Dashboard}
\usage{
download_sheet(
  data_list,
  spreadsheet_name,
  status = "primary",
  colour = "blue",
  j = 1
)
}
\arguments{
\item{data_list}{A list containing data and configurations for the dashboard. It includes the \code{contents} data frame, which specifies the layout and structure of the tabs.}

\item{spreadsheet_name}{A character string specifying the name of the spreadsheet within \code{data_list} that defines the content and download options for this tab item.}

\item{status}{A character string indicating the status of the tab item, used for styling. Default is \code{"primary"}.}

\item{colour}{A character string specifying the background colour of the tab item. Default is \code{"blue"}.}

\item{j}{An integer index used to uniquely identify the current tab item within the dashboard. Default is \code{1}.}
}
\value{
A \code{tabItem} object for inclusion in a Shiny dashboard, configured for downloading data.
}
\description{
This function generates a tab item in a Shiny dashboard specifically for downloading data. It sets up the layout for download labels, data options, and integrates user authentication where needed.
}
