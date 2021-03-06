% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_file.R
\name{validate_file}
\alias{validate_file}
\title{Validate a CSV File}
\usage{
validate_file(path, file_name, map, ...)
}
\arguments{
\item{path}{A path to a .csv file to be validated.}

\item{file_name}{name of file for which details need to be extracted from map.}

\item{map}{mapping of dictionary of data tests to be applied to import files.}

\item{\ldots}{ignored.}
}
\value{
Prints a report to the console of tests for file type, comma broken
csv, number of columns, spaces in column headers, correct column names,
required columns containing values.
}
\description{
Validate a .csv file.
}
