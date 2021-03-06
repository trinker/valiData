% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vf_non_empty.R
\name{vf_non_empty}
\alias{vf_non_empty}
\title{Checks If File Not Empty}
\usage{
vf_non_empty(path, ...)
}
\arguments{
\item{path}{path to file}

\item{\dots}{ignored.}
}
\description{
Checks If File Not Empty
}
\examples{
writeLines("", '~/filename.csv')
		vf_non_empty('~/filename.csv') 		#  FALSE
writeLines("hi", '~/filename.csv')
		vf_non_empty('~/filename.csv') 	#  TRUE
}
