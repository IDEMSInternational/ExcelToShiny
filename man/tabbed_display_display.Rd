% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabbed_display_display.R
\name{tabbed_display_display}
\alias{tabbed_display_display}
\title{Generate Tabbed Display Layout}
\usage{
tabbed_display_display(spreadsheet_ID_names, data_list, d_box, q = 1)
}
\arguments{
\item{spreadsheet_ID_names}{A list of spreadsheet ID names corresponding to different display elements.}

\item{data_list}{A list containing the data associated with each spreadsheet ID.}

\item{d_box}{A list containing display box configurations for each element.}

\item{q}{An integer indicating which spreadsheet ID and display box to use (default is 1).}
}
\value{
A list of \code{shiny::fluidRow} objects that represent the layout for the tabbed display.
}
\description{
This function generates a tabbed display layout for a Shiny application. It creates and arranges rows within each tab
based on the provided spreadsheet ID names and the associated data.
}
