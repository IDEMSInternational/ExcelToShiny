% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_sheet.R
\name{display_sheet}
\alias{display_sheet}
\title{Create a Display Sheet for Shiny Dashboard}
\usage{
display_sheet(
  data_list,
  spreadsheet_name,
  d_box,
  status = "primary",
  colour = "blue",
  j = 1
)
}
\arguments{
\item{data_list}{A list containing data and configurations for the dashboard. It should include the \code{contents} data frame, which specifies the layout and structure.}

\item{spreadsheet_name}{A character string specifying the name of the spreadsheet within \code{data_list} that defines the content for this tab item.}

\item{d_box}{A list of display box objects created by the \code{display_sheet_setup} function, to be arranged within the tab.}

\item{status}{A character string indicating the status of the tab item, used for styling. Default is \code{"primary"}.}

\item{colour}{A character string specifying the background colour of the tab item. Default is \code{"blue"}.}

\item{j}{An integer index used for identifying the current tab item within the dashboard. Default is \code{1}.}
}
\value{
A \code{tabItem} object for inclusion in a Shiny dashboard, containing the organised display elements.
}
\description{
This function generates a tab item for a Shiny dashboard based on the contents of a spreadsheet. It organises display boxes within a tab layout, allowing for flexible row-based arrangement of elements in the dashboard.
}
