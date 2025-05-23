% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_table.R
\name{specify_table}
\alias{specify_table}
\title{Create a Table Based on Data Manipulation Instructions}
\usage{
specify_table(data, spreadsheet, grouped_vars = NULL)
}
\arguments{
\item{data}{A dataset to manipulate and plot. Can be a list or a data frame.}

\item{spreadsheet}{An object containing data manipulation and graph manipulation instructions.}

\item{grouped_vars}{Optional. Variables to group the data by, provided as a character vector.}
}
\value{
A list containing two elements:
\describe{
\item{\code{table}}{A table generated from the data based on the \code{spreadsheet$table_manip} instructions.}
}
}
\description{
This function generates a  table from a dataset based on manipulation instructions provided in a \code{spreadsheet} object.
The function supports optional grouping and allows for dynamic data manipulation and plot creation.
}
